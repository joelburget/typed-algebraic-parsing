open Base

module Re = struct
  module T = struct
    type t =
      | Char_class of Char_class.t
      | Seq of t list
      | Star of t
      | Alt of t list
      | And of t list
      | Complement of t
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

open Re

let empty = Char_class Char_class.empty
let any = Char_class Char_class.any
let eps = Star empty

(* No Xor = 1 *)
let rec prec = function
  | Char_class _ | Seq [] | Alt [] | And [] -> 6
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
  | Char_class cset -> Char_class.pp ppf cset
  | Seq [] -> ()
  | Seq [ re ] -> pp ppf re
  | Seq res -> list pp ppf res
  | Star re' -> Fmt.pf ppf "%a*" (pp' (prec re)) re'
  | Alt [] | And [] -> pf ppf "{}"
  | Alt [ re ] | And [ re ] -> pp ppf re
  | Alt res -> list ~sep:(any "|@,") (pp' (prec re)) ppf res
  | And res -> list ~sep:(any "&@,") (pp' (prec re)) ppf res
  | Complement re -> pf ppf "!%a" pp re

and pp' prec' ppf re = if Int.(prec' > prec re) then Fmt.parens pp ppf re else pp ppf re

let ( >>> ) t1 t2 =
  match t1, t2 with
  | Star (Char_class cls), re when Char_class.is_empty cls -> re
  | re, Star (Char_class cls) when Char_class.is_empty cls -> re
  | Char_class cls, _ when Char_class.is_empty cls -> empty
  | _, Char_class cls when Char_class.is_empty cls -> empty
  | Seq rs1, Seq rs2 -> Seq (rs1 @ rs2)
  | Seq rs, r -> Seq (rs @ [ r ])
  | r, Seq rs -> Seq (r :: rs)
  | _ -> Seq [ t1; t2 ]
;;

let chr c = Char_class (Char_class.singleton (Uchar.of_char c))
let str s = s |> String.to_list |> List.map ~f:chr |> List.fold_right ~init:eps ~f:( >>> )
let star = function Star _ as re -> re | re -> Star re
let plus re = re >>> star re

let merge_csets in_res merge_op =
  let charsets, res =
    List.partition_map in_res ~f:(function
        | Char_class cset -> Either.First cset
        | re -> Second re)
  in
  match charsets with
  | [] | [ _ ] -> in_res
  | cset :: csets ->
    let merged_cset = List.fold_left ~f:merge_op ~init:cset csets in
    List.merge ~compare [ Char_class merged_cset ] res
;;

let merge_with ctor re1 re2 =
  match compare re1 re2 with
  | x when Int.(x < 0) -> ctor re1 re2
  | 0 -> re1
  | _ -> ctor re2 re1
;;

let merge_alts a b =
  match merge_csets (List.merge ~compare a b) Char_class.union with
  | [] -> empty
  | [ re ] -> re
  | res -> Alt res
;;

let ( || ) re1 re2 =
  match re1, re2 with
  | Char_class s1, Char_class s2 -> Char_class (Char_class.union s1 s2)
  | (Char_class cls, re | re, Char_class cls) when Char_class.is_empty cls -> re
  | Alt res1, Alt res2 -> merge_alts res1 res2
  | Alt res1, _ -> merge_alts res1 [ re2 ]
  | _, Alt res2 -> merge_alts [ re1 ] res2
  | _ -> merge_with (fun x y -> Alt [ x; y ]) re1 re2
;;

let merge_ands a b =
  match merge_csets (List.merge ~compare a b) Char_class.inter with
  | [] -> empty
  | [ re ] -> re
  | res -> And res
;;

let ( && ) re1 re2 =
  match re1, re2 with
  | Char_class s1, Char_class s2 -> Char_class (Char_class.inter s1 s2)
  | (Char_class cls, _ | _, Char_class cls) when Char_class.is_empty cls -> empty
  | And res1, And res2 -> merge_ands res1 res2
  | And res1, _ -> merge_ands res1 [ re2 ]
  | re1, And res2 -> merge_ands [ re1 ] res2
  | _ -> merge_with (fun x y -> And [ x; y ]) re1 re2
;;

let complement = function
  | Complement re -> re
  | Char_class cls when Char_class.is_empty cls -> star any
  | re -> Complement re
;;

let rec nullable = function
  | Star _ -> true
  | Char_class _ -> false
  | Seq rs | And rs -> List.for_all ~f:nullable rs
  | Alt rs -> List.exists ~f:nullable rs
  | Complement r -> not (nullable r)
;;

(* Eps if the language defined contains the empty string, Empty otherwise *)
let nullability re = if nullable re then eps else empty

let rec delta c = function
  | Char_class set -> if Char_class.mem set c then eps else empty
  | Seq [] -> empty
  | Alt [] | And [] -> failwith "error"
  | Seq [ re ] | Alt [ re ] | And [ re ] -> delta c re
  | Seq (re :: res) ->
    let mk_concat_list = List.fold_right ~init:eps ~f:( >>> ) in
    mk_concat_list (delta c re :: res) || nullability re >>> delta c (Seq res)
  | Star r' as r -> delta c r' >>> r
  | Alt (re :: res) -> delta c re || delta c (Alt res)
  | And (re :: res) -> delta c re && delta c (And res)
  | Complement r -> Complement (delta c r)
;;

let string_delta str =
  let len = String.length str in
  let rec loop i re =
    if Int.(i >= len)
    then re
    else loop (i + 1) (delta (Uchar.of_char (String.unsafe_get str i)) re)
  in
  loop 0
;;

(* Cross product (intersection) of two sets of character sets. *)
let cross s1 s2 =
  Set.fold
    s1
    ~init:(Set.empty (module Char_class))
    ~f:(fun accum s1_elem ->
      Set.fold s2 ~init:accum ~f:(fun accum s2_elem ->
          Set.add accum (Char_class.inter s1_elem s2_elem)))
;;

(* Trivial derivative class, the singleton set of any character. *)
let trivial = Set.singleton (module Char_class) Char_class.any

(* Approximate derivative class for a regex. *)
let rec class' = function
  | Star (Char_class cls) when Char_class.is_empty cls -> trivial
  | Seq [] -> trivial
  | Char_class cs -> Set.of_list (module Char_class) [ cs; Char_class.negate cs ]
  | Seq [ re ] | Star re | Complement re -> class' re
  | Seq (re :: res) ->
    if not (nullable re) then class' re else cross (class' re) (class' (Seq res))
  | Alt res | And res -> res |> List.map ~f:class' |> List.fold ~init:trivial ~f:cross
;;

type re = t

module Int_pair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Dfa : sig
  (* start state is always 0 *)
  type t =
    { state_numbers : (int * re) list
    ; accepting : int list
    ; transitions : ((int * int) * Char_class.t) list
    }

  val make : re -> t
end = struct
  type t =
    { state_numbers : (int * re) list
    ; accepting : int list
    ; transitions : ((int * int) * Char_class.t) list
    }

  let pp_re = pp

  let pp =
    let open Fmt in
    let pair x y = parens (pair ~sep:comma x y) in
    let list elem = brackets (list elem ~sep:semi) in
    braces
      (record
         ~sep:semi
         [ field "state_numbers" (fun t -> t.state_numbers) (list (pair int pp_re))
         ; field "accepting" (fun t -> t.accepting) (list int)
         ; field
             "transitions"
             (fun t -> t.transitions)
             (list (pair (pair int int) Char_class.pp))
         ])
  ;;

  type re_map = (Re.t, int, Re.comparator_witness) Map.t
  type connection_alist = ((int * int) * Char_class.t) list
  type connection_map = (Int_pair.t, Char_class.t, Int_pair.comparator_witness) Map.t
  type graph_state = re_map * connection_alist

  let make re =
    let rec explore graph state : graph_state =
      state
      |> class'
      |> Set.filter ~f:(fun cls -> not (Char_class.is_empty cls))
      |> Set.to_list
      |> List.fold_left ~init:graph ~f:(goto state)
    and goto re (states, edges) char_class : graph_state =
      let char = Char_class.choose_exn char_class in
      let derived_re = delta char re in
      let mk_edge dest = ((Map.find_exn states re, dest), char_class) :: edges in
      match Map.find states derived_re with
      | Some w -> states, mk_edge w
      | None ->
        let size = Map.length states in
        explore (Map.add_exn states ~key:derived_re ~data:size, mk_edge size) derived_re
    in
    let graph : graph_state = Map.singleton (module Re) re 0, [] in
    let states, edges = explore graph re in
    let transitions =
      edges |> Map.of_alist_reduce (module Int_pair) ~f:Char_class.union |> Map.to_alist
    in
    let state_numbers = states |> Map.to_alist |> List.map ~f:(fun (re, i) -> i, re) in
    let accepting =
      states |> Map.filteri ~f:(fun ~key:re ~data:_i -> nullable re) |> Map.data
    in
    { state_numbers; accepting; transitions }
  ;;

  let%test_module "Dfa.make" =
    (module struct
      let go re = re |> make |> Fmt.pr "%a@." pp

      let%expect_test _ =
        go eps;
        [%expect
          {|
          {state_numbers: [(1, []); (0, []*)];
           accepting: [0];
           transitions: [((0, 1), .); ((1, 1), .)]} |}]
      ;;

      let%expect_test _ =
        go any;
        [%expect
          {|
          {state_numbers: [(2, []); (0, .); (1, []*)];
           accepting: [1];
           transitions: [((0, 1), .); ((1, 2), .); ((2, 2), .)]} |}]
      ;;

      let%expect_test _ =
        go (chr 'c');
        [%expect
          {|
          {state_numbers: [(2, []); (0, c); (1, []*)];
           accepting: [1];
           transitions: [((0, 1), c); ((0, 2), [^c]); ((1, 2), .); ((2, 2), .)]} |}]
      ;;

      let%expect_test _ =
        go (str "ab" || str "bc");
        [%expect
          {|
          {state_numbers: [(3, []); (1, b); (4, c); (2, []*); (0, ab|bc)];
           accepting: [2];
           transitions:
            [((0, 1), a); ((0, 3), [^a-b]); ((0, 4), b); ((1, 2), b); ((1, 3), [^b]);
             ((2, 3), .); ((3, 3), .); ((4, 2), c); ((4, 3), [^c])]} |}]
      ;;

      let%expect_test _ =
        go
          (star (Char_class (Char_class.range (Uchar.of_char 'a') (Uchar.of_char 'z')))
          && complement (str "()" || str "do" || str "for" || str "if" || str "while"));
        [%expect
          {|
          {state_numbers:
            [(1, []); (2, [a-z]*&![]); (11, [a-z]*&!e); (7, [a-z]*&!f); (3, [a-z]*&!o);
             (6, [a-z]*&!r); (8, [a-z]*&!hile); (9, [a-z]*&!ile); (10, [a-z]*&!le);
             (5, [a-z]*&!or); (4, [a-z]*&![]*); (0, [a-z]*&!\(\)|do|for|if|while)];
           accepting: [2; 11; 7; 3; 6; 8; 9; 10; 5; 0];
           transitions:
            [((0, 1), [^a-z]); ((0, 2), [a-ceg-hj-vx-z]); ((0, 3), d); ((0, 5), f);
             ((0, 7), i); ((0, 8), w); ((1, 1), .); ((2, 1), [^a-z]); ((2, 2), [a-z]);
             ((3, 1), [^a-z]); ((3, 2), [a-np-z]); ((3, 4), o); ((4, 1), [^a-z]);
             ((4, 2), [a-z]); ((5, 1), [^a-z]); ((5, 2), [a-np-z]); ((5, 6), o);
             ((6, 1), [^a-z]); ((6, 2), [a-qs-z]); ((6, 4), r); ((7, 1), [^a-z]);
             ((7, 2), [a-eg-z]); ((7, 4), f); ((8, 1), [^a-z]); ((8, 2), [a-gi-z]);
             ((8, 9), h); ((9, 1), [^a-z]); ((9, 2), [a-hj-z]); ((9, 10), i);
             ((10, 1), [^a-z]); ((10, 2), [a-km-z]); ((10, 11), l); ((11, 1), [^a-z]);
             ((11, 2), [a-df-z]); ((11, 4), e)]} |}]
      ;;
    end)
  ;;
end

module Vector = struct
  module Core = struct
    module T = struct
      type t = Re.t list [@@deriving compare, sexp]
    end

    include T
    include Comparable.Make (T)
  end

  open Core

  let delta c = List.map ~f:(delta c)
  let string_delta str = List.map ~f:(string_delta str)
  let pp_vec = Fmt.(brackets (list pp ~sep:semi))

  module Dfa : sig
    type nonrec t =
      { state_numbers : (int * t) list
      ; accepting : int list
      ; transitions : ((int * int) * Char_class.t) list
      }

    val make : Core.t -> t
  end = struct
    type nonrec t =
      { state_numbers : (int * t) list
      ; accepting : int list
      ; transitions : ((int * int) * Char_class.t) list
      }

    let pp =
      let open Fmt in
      let pair x y = parens (pair ~sep:comma x y) in
      let list elem = brackets (list elem ~sep:semi) in
      braces
        (record
           ~sep:semi
           [ field "state_numbers" (fun t -> t.state_numbers) (list (pair int pp_vec))
           ; field "accepting" (fun t -> t.accepting) (list int)
           ; field
               "transitions"
               (fun t -> t.transitions)
               (list (pair (pair int int) Char_class.pp))
           ])
    ;;

    type re_map = (Core.t, int, Core.comparator_witness) Map.t
    type connection_alist = ((int * int) * Char_class.t) list
    type connection_map = (Int_pair.t, Char_class.t, Int_pair.comparator_witness) Map.t
    type graph_state = re_map * connection_alist

    let class' : Core.t -> (Char_class.t, Char_class.comparator_witness) Set.t =
     fun res -> res |> List.map ~f:class' |> List.fold ~init:trivial ~f:cross
   ;;

    let nullable res = List.exists res ~f:nullable

    let make vec =
      let rec explore graph (state : Core.t) : graph_state =
        state
        |> class'
        |> Set.filter ~f:(fun cls -> not (Char_class.is_empty cls))
        |> Set.to_list
        |> List.fold_left ~init:graph ~f:(goto state)
      and goto (vec : Core.t) (states, edges) char_class : graph_state =
        let char = Char_class.choose_exn char_class in
        let derived_re = delta char vec in
        let mk_edge dest = ((Map.find_exn states vec, dest), char_class) :: edges in
        match Map.find states derived_re with
        | Some w -> states, mk_edge w
        | None ->
          let size = Map.length states in
          explore (Map.add_exn states ~key:derived_re ~data:size, mk_edge size) derived_re
      in
      let graph : graph_state = Map.singleton (module Core) vec 0, [] in
      let states, edges = explore graph vec in
      let transitions =
        edges |> Map.of_alist_reduce (module Int_pair) ~f:Char_class.union |> Map.to_alist
      in
      let state_numbers =
        states |> Map.to_alist |> List.map ~f:(fun (vec, i) -> i, vec)
      in
      let accepting =
        states |> Map.filteri ~f:(fun ~key:vec ~data:_i -> nullable vec) |> Map.data
      in
      { state_numbers; accepting; transitions }
    ;;
  end
end

let derivatives res =
  res
  |> List.map ~f:class'
  |> List.fold ~init:trivial ~f:cross
  |> Set.to_list
  |> List.filter_map ~f:(fun set ->
         if Char_class.is_empty set
         then None
         else (
           let rep = Char_class.choose_exn set in
           Some (Vector.delta rep res, set)))
;;

let%test_module _ =
  (module struct
    let pp' title = Fmt.pr "@[<hv 2>%S:@ %a@]@." title pp

    let%expect_test "pp" =
      let pp = pp' in
      pp "eps" eps;
      pp "Char_class Char_class.empty" (Char_class Char_class.empty);
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
          "eps": []*
          "Char_class Char_class.empty": []
          "Alt []": {}
          "Alt [ chr 'c' ]": c
          "Alt [ chr 'c'; chr 'd' ]": c|d
          "And []": {}
          "And [ chr 'c' ]": c
          "And [ chr 'c'; chr 'd' ]": c&d
          "complement (chr 'c')": !c
          "complement (complement (chr 'c'))": c |}]
    ;;

    let%expect_test "delta" =
      let pp = pp' in
      let go title re c = delta c re |> pp title in
      let c = Uchar.of_char 'c' in
      let x = Uchar.of_char 'x' in
      let class_of_string str = Char_class (Char_class.of_string str) in
      go "delta 'c' empty" empty c;
      go "delta 'c' eps" eps c;
      go "delta 'c' any" any c;
      go "delta 'c' (c)" (chr 'c') c;
      go "delta 'c' (c || d)" (chr 'c' || chr 'd') c;
      go "delta 'x' (c || d)" (chr 'c' || chr 'd') x;
      go "delta 'c' [^cd]" Char_class.(Char_class (negate (of_string "cd"))) c;
      go "delta 'x' [^cd]" Char_class.(Char_class (negate (of_string "cd"))) x;
      go "delta 'c' ([abc] && [cde])" (class_of_string "abc" || class_of_string "cde") c;
      go "delta 'x' ([abc] && [cde])" (class_of_string "abc" || class_of_string "cde") x;
      go {|delta 'c' (str "cd")|} (str "cd") c;
      go {|delta 'c' (str "dc")|} (str "dc") c;
      [%expect
        {|
          "delta 'c' empty": []
          "delta 'c' eps": []
          "delta 'c' any": []*
          "delta 'c' (c)": []*
          "delta 'c' (c || d)": []*
          "delta 'x' (c || d)": []
          "delta 'c' [^cd]": []
          "delta 'x' [^cd]": []*
          "delta 'c' ([abc] && [cde])": []*
          "delta 'x' ([abc] && [cde])": []
          "delta 'c' (str \"cd\")": d
          "delta 'c' (str \"dc\")": [] |}]
    ;;

    let%expect_test "string_delta" =
      let go title re str = string_delta str re |> pp' title in
      go {|delta "ab" (str "abc")|} (str "abc") "ab";
      [%expect {|
          "delta \"ab\" (str \"abc\")": c |}]
    ;;

    let go res =
      let open Fmt in
      let pp_derivative =
        braces
          (record
             ~sep:semi
             [ field "res" fst (brackets (list pp ~sep:semi))
             ; field "char_class" snd Char_class.pp
             ])
      in
      pr
        "@[<hv 2>%a ->@ %a@]@."
        (brackets (list pp ~sep:semi))
        res
        (hvbox (list pp_derivative ~sep:sp))
        (derivatives res)
    ;;

    let%expect_test "derivative []" =
      go [];
      [%expect {|
          [] -> {res: [];
                 char_class: .} |}]
    ;;

    let%expect_test "derivative ['c']" =
      go [ chr 'c' ];
      [%expect
        {|
          [c] -> {res: [[]*];
                  char_class: c} {res: [[]];
                                  char_class: [^c]} |}]
    ;;

    let%expect_test "derivative ['c'; 'd']" =
      go [ chr 'c'; chr 'd' ];
      [%expect
        {|
          [c; d] ->
            {res: [[]*; []];
             char_class: c}
            {res: [[]; []*];
             char_class: d}
            {res: [[]; []];
             char_class: [^c-d]} |}]
    ;;

    let%expect_test "derivative ['c'; []]" =
      go [ chr 'c'; empty ];
      [%expect
        {|
          [c; []] -> {res: [[]*; []];
                      char_class: c} {res: [[]; []];
                                      char_class: [^c]} |}]
    ;;

    (* Regex derivatives reexamined fig 4b. *)
    let%expect_test {|derivative ["ab"; "bc"]|} =
      go [ str "ab" || str "bc" ];
      [%expect
        {|
          [ab|bc] ->
            {res: [b];
             char_class: a}
            {res: [c];
             char_class: b}
            {res: [[]];
             char_class: [^a-b]} |}]
    ;;

    let%expect_test {|derivative ["ab"; "bc"]|} =
      go
        [ (* TODO plus (Char_class (Char_class.range 'a' 'z')) *)
          chr ' ' || chr '\n'
        ; chr '('
        ; chr ')'
        ];
      [%expect
        {|
          [[\u10\u32]; \(; \)] ->
            {res: [[]*; []; []];
             char_class: [\u10\u32]}
            {res: [[]; []*; []];
             char_class: \(}
            {res: [[]; []; []*];
             char_class: \)}
            {res: [[]; []; []];
             char_class: [^\u10\u32\(-\)]} |}]
    ;;
  end)
;;
