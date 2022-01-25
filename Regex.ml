open Base

type t =
  | Empty
  | Eps
  | Char_set of Char_set.t
  | Seq of t list
  | Alt of t list
  | Star of t
  | And of t list
  | Complement of t
[@@deriving compare, equal, sexp]

(* No Xor = 1 *)
let rec prec = function
  | Empty | Eps | Char_set _ | Seq [] | Alt [] | And [] -> 6
  | Seq _ -> 3
  | Star _ -> 5
  | Alt [ re ] | And [ re ] -> prec re
  | Alt _ -> 0
  | And _ -> 2
  | Complement _ -> 4
;;

let rec pp ppf re =
  let open Fmt in
  match re with
  | Empty -> pf ppf "{empty}"
  | Eps -> pf ppf "{eps}"
  | Char_set cset -> Char_set.pp ppf cset
  | Seq [] -> ()
  | Seq [ re ] -> pp ppf re
  | Seq res -> list pp ppf res
  | Star re' -> pp' (prec re) ppf re'
  | Alt [] | And [] -> pf ppf "{}"
  | Alt [ re ] | And [ re ] -> pp ppf re
  | Alt res -> list ~sep:(any "|@,") (pp' (prec re)) ppf res
  | And res -> list ~sep:(any "&@,") (pp' (prec re)) ppf res
  | Complement re -> pf ppf "!%a" pp re

and pp' prec' ppf re = if prec' > prec re then Fmt.parens pp ppf re else pp ppf re

let ( >>> ) t1 t2 =
  match t1, t2 with
  | Eps, re | re, Eps -> re
  | Empty, _ | _, Empty -> Empty
  | Seq rs1, Seq rs2 -> Seq (rs1 @ rs2)
  | Seq rs, r -> Seq (rs @ [ r ])
  | r, Seq rs -> Seq (r :: rs)
  | _ -> Seq [ t1; t2 ]
;;

let chr c = Char_set (Char_set.singleton c)
let str s = s |> String.to_list |> List.map ~f:chr |> List.fold_right ~init:Eps ~f:( >>> )
let star = function Eps -> Eps | Empty -> Eps | Star _ as re -> re | re -> Star re
let plus re = re >>> star re

let merge_csets in_res merge_op =
  let charsets, res =
    List.partition_map in_res ~f:(function
        | Char_set cset -> Either.First cset
        | re -> Second re)
  in
  match charsets with
  | [] | [ _ ] -> in_res
  | cset :: csets ->
    let merged_cset = List.fold_left ~f:merge_op ~init:cset csets in
    List.merge ~compare [ Char_set merged_cset ] res
;;

let merge_with ctor re1 re2 =
  match compare re1 re2 with x when x < 0 -> ctor re1 re2 | 0 -> re1 | _ -> ctor re2 re1
;;

let merge_alts a b =
  match merge_csets (List.merge ~compare a b) Char_set.union with
  | [] -> Empty
  | [ re ] -> re
  | res -> Alt res
;;

let ( || ) re1 re2 =
  match re1, re2 with
  | Empty, re | re, Empty -> re
  | Char_set s1, Char_set s2 -> Char_set (Char_set.union s1 s2)
  | Alt res1, Alt res2 -> merge_alts res1 res2
  | Alt res1, _ -> merge_alts res1 [ re2 ]
  | _, Alt res2 -> merge_alts [ re1 ] res2
  | _ -> merge_with (fun x y -> Alt [ x; y ]) re1 re2
;;

let merge_ands a b =
  match merge_csets (List.merge ~compare a b) Char_set.inter with
  | [] -> Empty
  | [ re ] -> re
  | res -> And res
;;

let ( && ) re1 re2 =
  match re1, re2 with
  | Empty, _ | _, Empty -> Empty
  | Char_set s1, Char_set s2 -> Char_set (Char_set.inter s1 s2)
  | And res1, And res2 -> merge_ands res1 res2
  | And res1, _ -> merge_ands res1 [ re2 ]
  | re1, And res2 -> merge_ands [ re1 ] res2
  | _ -> merge_with (fun x y -> And [ x; y ]) re1 re2
;;

let any = Char_set Char_set.any
let complement = function Complement re -> re | Empty -> star any | re -> Complement re

(* Eps if the language defined contains the empty string, Empty otherwise *)
let rec nullable = function
  | Eps | Star _ -> true
  | Empty | Char_set _ -> false
  | Seq rs | And rs -> List.for_all ~f:nullable rs
  | Alt rs -> List.exists ~f:nullable rs
  | Complement r -> not (nullable r)
;;

let nullability re = if nullable re then Eps else Empty

let rec delta c = function
  | Empty | Eps -> Empty
  | Char_set set -> if Char_set.mem c set then Eps else Empty
  | Seq [] -> Empty
  | Alt [] | And [] -> failwith "error"
  | Seq [ re ] | Alt [ re ] | And [ re ] -> delta c re
  | Seq (re :: res) ->
    let mk_concat_list = List.fold_right ~init:Eps ~f:( >>> ) in
    mk_concat_list (delta c re :: res) || nullability re >>> delta c (Seq res)
  | Star r' as r -> delta c r' >>> r
  | Alt (re :: res) -> delta c re || delta c (Alt res)
  | And (re :: res) -> delta c re && delta c (And res)
  | Complement r -> Complement (delta c r)
;;

let string_delta str =
  let len = String.length str in
  let rec loop i re =
    if Int.(i >= len) then re else loop (i + 1) (delta (String.unsafe_get str i) re)
  in
  loop 0
;;

(* Cross product (intersection) of two sets of character sets. *)
let cross s1 s2 =
  Set.fold
    s1
    ~init:(Set.empty (module Char_set))
    ~f:(fun accum s1_elem ->
      Set.fold s2 ~init:accum ~f:(fun accum s2_elem ->
          Set.add accum (Char_set.inter s1_elem s2_elem)))
;;

(* Trivial derivative class, the singleton set of any character. *)
let trivial = Set.singleton (module Char_set) Char_set.any

(* Approximate derivative class for a regex. *)
let rec class' = function
  | Eps | Empty | Seq [] -> trivial
  | Char_set cs -> Set.of_list (module Char_set) [ cs; Char_set.complement cs ]
  | Seq [ re ] -> class' re
  | Seq (re :: res) ->
    if nullable re then class' re else cross (class' re) (class' (Seq res))
  | Star re -> class' re
  | Alt res | And res -> res |> List.map ~f:class' |> List.fold ~init:trivial ~f:cross
  | Complement re -> class' re
;;

module Vector = struct
  type nonrec t = t list

  let delta c = List.map ~f:(delta c)
  let string_delta str = List.map ~f:(string_delta str)
end

let derivatives res =
  res
  |> List.map ~f:class'
  |> List.fold ~init:trivial ~f:cross
  |> Set.to_list
  |> List.filter_map ~f:(fun set ->
         if Char_set.is_empty set
         then None
         else (
           let rep, _ = Char_set.choose set in
           Some (Vector.delta rep res, set)))
;;

(*
  let compress sets =
    let rec part1 set1 = function
      | [] -> if Char_set.is_empty set1 then [] else [ set1 ]
      | set2 :: sets ->
        if Char_set.is_empty set1
        then set2 :: sets
        else (
          let i = Char_set.inter set1 set2 in
          if Char_set.is_empty i
          then set2 :: part1 set1 sets
          else (
            let s1 = Char_set.diff set1 i in
            let s2 = Char_set.diff set2 i in
            let sets' = if Char_set.is_empty s1 then sets else part1 s1 sets in
            if Char_set.is_empty s2 then i :: sets' else i :: s2 :: sets'))
    in
    List.fold
      ~f:(fun sets char_set -> part1 char_set sets)
      ~init:[]
      (Char_set.any :: sets)
  ;;

  module Re_list = struct
    module T = struct
      type nonrec t = t list

      let compare x y = List.compare compare x y
      let sexp_of_t = List.sexp_of_t sexp_of_t
      let t_of_sexp = List.t_of_sexp t_of_sexp
    end

    include T
    include Comparable.Make (T)
  end

  let derivatives2 res =
    let rec classes class_map sets =
      match sets with
      | [] -> Map.to_alist class_map
      | set :: sets ->
        let rep, _ = Char_set.choose set in
        let derivs = List.map ~f:(delta rep) res in
        (match Map.find class_map derivs with
        | None -> classes (Map.set class_map ~key:derivs ~data:set) sets
        | Some set' ->
          let map' = Map.set class_map ~key:derivs ~data:(Char_set.union set set') in
          classes map' sets)
    in
    res
    |> List.map ~f:class'
    |> List.fold ~init:trivial ~f:cross
    |> Set.to_list
    |> compress
    |> classes (Map.empty (module Re_list))
  ;;
     *)

(*
  type char_set_set = (Char_set.t, Char_set.comparator_witness) Set.t

  let derivatives3 (res : t list) =
    let rec goto current_state class' states =
      let rep, _ = Char_set.choose class' in
      let new_state = Vector.delta rep current_state in
      if Set.mem states new_state
      then states
      else explore (Set.add states new_state) new_state
    and explore states current_state =
      List.fold ~f:(goto current_state) ~init:states (class' current_state)
    in
    let q0 : t = List.hd_exn res in
    explore (Set.singleton (module Char_set) q0 : char_set_set) q0
  ;;
  *)

let%test_module _ =
  (module struct
    let pp' title = Fmt.pr "@[<hv 2>%S:@ %a@]@." title pp

    let%expect_test "pp" =
      let pp = pp' in
      pp "Empty" Empty;
      pp "Eps" Eps;
      pp "Char_set Char_set.empty" (Char_set Char_set.empty);
      pp "Alt []" (Alt []);
      pp "Alt [ chr 'c' ]" (Alt [ chr 'c' ]);
      pp "Alt [ chr 'c'; chr 'd' ]" (Alt [ chr 'c'; chr 'd' ]);
      pp "And []" (And []);
      pp "And [ chr 'c' ]" (And [ chr 'c' ]);
      pp "And [ chr 'c'; chr 'd' ]" (And [ chr 'c'; chr 'd' ]);
      pp "complement (chr 'c')" (complement (chr 'c'));
      pp "complement (complement (chr 'c'))" (complement (complement (chr 'c')));
      [%expect
        {|
          "Empty": {empty}
          "Eps": {eps}
          "Char_set Char_set.empty": []
          "Alt []": {}
          "Alt [ chr 'c' ]": [c]
          "Alt [ chr 'c'; chr 'd' ]": [c]|[d]
          "And []": {}
          "And [ chr 'c' ]": [c]
          "And [ chr 'c'; chr 'd' ]": [c]&[d]
          "complement (chr 'c')": ![c]
          "complement (complement (chr 'c'))": [c] |}]
    ;;

    let%expect_test "delta" =
      let pp = pp' in
      let go title re c = delta c re |> pp title in
      go "delta 'c' (chr 'c')" (chr 'c') 'c';
      pp "chr 'd' || Empty >>> Empty" (chr 'd' || Empty >>> Empty);
      go {|delta 'c' (str "cd")|} (str "cd") 'c';
      go {|delta 'c' (str "dc")|} (str "dc") 'c';
      [%expect
        {|
          "delta 'c' (chr 'c')": {eps}
          "chr 'd' || Empty >>> Empty": [d]
          "delta 'c' (str \"cd\")": [d]
          "delta 'c' (str \"dc\")": {empty} |}]
    ;;

    let%expect_test "string_delta" =
      let go title re str = string_delta str re |> pp' title in
      go {|delta "ab" (str "abc")|} (str "abc") "ab";
      [%expect {|
          "delta \"ab\" (str \"abc\")": [c] |}]
    ;;

    let go res =
      let open Fmt in
      let pp_derivative =
        braces
          (record
             ~sep:semi
             [ field "res" fst (brackets (list pp ~sep:semi))
             ; field "char_set" snd Char_set.pp
             ])
      in
      pr
        "@[<hv 2>%a ->@ %a@]@."
        (brackets (list pp ~sep:semi))
        res
        (hvbox (list pp_derivative))
        (derivatives res)
    ;;

    let%expect_test "derivative []" =
      go [];
      [%expect {|
          [] -> {res: [];
                 char_set: [.]} |}]
    ;;

    let%expect_test "derivative ['c']" =
      go [ chr 'c' ];
      [%expect
        {|
          [[c]] ->
            {res: [{empty}];
             char_set: [\u0-bd-\u255]}{res: [{eps}];
                                       char_set: [c]} |}]
    ;;

    let%expect_test "derivative ['c'; 'd']" =
      go [ chr 'c'; chr 'd' ];
      [%expect
        {|
          [[c]; [d]] ->
            {res: [{empty}; {empty}];
             char_set: [\u0-be-\u255]}
            {res: [{eps}; {empty}];
             char_set: [c]}
            {res: [{empty}; {eps}];
             char_set: [d]} |}]
    ;;

    let%expect_test "derivative ['c'; {empty}]" =
      go [ chr 'c'; Empty ];
      [%expect
        {|
          [[c]; {empty}] ->
            {res: [{empty}; {empty}];
             char_set: [\u0-bd-\u255]}
            {res: [{eps}; {empty}];
             char_set: [c]} |}]
    ;;

    (* Regex derivatives reexamined fig 4b. *)
    let%expect_test {|derivative ["ab"; "bc"]|} =
      go [ str "ab" || str "bc" ];
      [%expect
        {|
          [[a][b]|[b][c]] ->
            {res: [{empty}];
             char_set: [\u0-`d-\u255]}
            {res: [[b]];
             char_set: [a]}
            {res: [[c]];
             char_set: [b]}
            {res: [{empty}];
             char_set: [c]} |}]
    ;;

    let%expect_test {|derivative ["ab"; "bc"]|} =
      go
        [ plus (Char_set (Char_set.range 'a' 'z'))
        ; chr ' ' || chr '\n'
        ; chr '('
        ; chr ')'
        ];
      [%expect
        {|
          [[a-z][a-z]; [\u10 ]; [(]; [)]] ->
            {res: [{empty}; {empty}; {empty}; {empty}];
             char_set: [\u0-\u9\u11-\u31!-'*-`{-\u255]}
            {res: [{empty}; {eps}; {empty}; {empty}];
             char_set: [\u10 ]}
            {res: [{empty}; {empty}; {eps}; {empty}];
             char_set: [(]}
            {res: [{empty}; {empty}; {empty}; {eps}];
             char_set: [)]}
            {res: [[a-z]; {empty}; {empty}; {empty}];
             char_set: [a-z]} |}]
    ;;
  end)
;;
