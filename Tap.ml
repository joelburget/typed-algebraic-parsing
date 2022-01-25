open Base
module Stream = Stdlib.Stream

type 'a parser = char Stream.t -> 'a

exception Parse_error of string

let error fmt = Stdlib.Format.kasprintf (fun str -> raise (Parse_error str)) fmt

module Type = struct
  type t =
    { first : Char_set.t
    ; flast : Char_set.t
    ; null : bool
    ; guarded : bool
    }

  let pp_set ppf set = Char_set.pp ppf set

  let pp =
    Fmt.(
      braces
        (record
           ~sep:semi
           [ field "first" (fun t -> t.first) pp_set
           ; field "flast" (fun t -> t.flast) pp_set
           ; field "null" (fun t -> t.null) bool
           ; field "guarded" (fun t -> t.guarded) bool
           ]))
  ;;

  let ( = ) t1 t2 =
    Char_set.equal t1.first t2.first
    && Char_set.equal t1.flast t2.flast
    && Bool.(t1.null = t2.null && t1.guarded = t2.guarded)
  ;;

  let check b msg = if not b then failwith msg
  let empty = Char_set.empty
  let singleton = Char_set.singleton
  let ( ==> ) b cs = if b then cs else empty
  let bot = { first = empty; flast = empty; null = false; guarded = true }
  let eps = { first = empty; flast = empty; null = true; guarded = true }
  let chr c = { first = singleton c; flast = empty; null = false; guarded = true }
  let separable t1 t2 = Char_set.(is_empty (inter t1.flast t2.first)) && not t1.null

  let apart t1 t2 =
    Char_set.(is_empty (inter t1.first t2.first)) && not (t1.null && t2.null)
  ;;

  let alt t1 t2 =
    check (apart t1 t2) (Fmt.str "alt must be apart @[(%a@ vs@ %a)@]" pp t1 pp t2);
    { first = Char_set.union t1.first t2.first
    ; flast = Char_set.union t1.flast t2.flast
    ; null = t1.null || t2.null
    ; guarded = t1.guarded && t2.guarded
    }
  ;;

  let seq t1 t2 =
    check (separable t1 t2) (Fmt.str "seq must be separable @[(%a@ vs@ %a)@]" pp t1 pp t2);
    { first = t1.first
    ; flast = Char_set.union t2.flast (t2.null ==> Char_set.union t2.first t1.flast)
    ; null = false
    ; guarded = t1.guarded
    }
  ;;

  let star t = { (seq t t) with null = true; flast = Char_set.union t.flast t.first }
  let min = { first = empty; flast = empty; null = false; guarded = false }

  let fix f =
    let rec loop tp =
      let tp' = f tp in
      if tp = tp' then tp else loop tp'
    in
    loop min
  ;;
end

module Parse = struct
  open Type

  let eps _s = ()

  let chr c s =
    match Stream.peek s with
    | None -> error "Unexpected end of stream"
    | Some c' ->
      if Char.(c' = c)
      then (
        Stream.junk s;
        c)
      else error "Unexpected char %C (expected %C)" c' c
  ;;

  let bot _ = error "bottom"

  let seq p1 p2 s =
    let a = p1 s in
    let b = p2 s in
    a, b
  ;;

  let map f p s = f (p s)

  let alt tp1 p1 tp2 p2 s =
    match Stream.peek s with
    | None ->
      if tp1.null
      then p1 s
      else if tp2.null
      then p2 s
      else error "Unexpected end of stream"
    | Some c ->
      if Char_set.mem c tp1.first
      then p1 s
      else if Char_set.mem c tp2.first
      then p2 s
      else if tp1.null
      then p1 s
      else if tp2.null
      then p2 s
      else error "No progress possible"
  ;;
end

module Var = struct
  type ('ctx, 'a) t =
    | Z : ('a * 'ctx, 'a) t
    | S : ('rest, 'a) t -> ('b * 'rest, 'a) t
end

module Env (T : sig
  type 'a t
end) =
struct
  type 'ctx t =
    | [] : unit t
    | ( :: ) : 'a T.t * 'ctx t -> ('a * 'ctx) t

  let rec lookup : type ctx a. ctx t -> (ctx, a) Var.t -> a T.t =
   fun ctx v -> match ctx, v with x :: _, Z -> x | _ :: xs, S v -> lookup xs v | _ -> .
 ;;

  type fn = { f : 'a. 'a T.t -> 'a T.t }

  let rec map : type ctx. fn -> ctx t -> ctx t =
   fun { f } -> function [] -> [] | x :: xs -> f x :: map { f } xs
 ;;
end

module Type_env = Env (struct
  type _ t = Type.t
end)

module Parse_env = Env (struct
  type 'a t = char Stream.t -> 'a
end)

type 'a type_env = 'a Type_env.t
type 'a parse_env = 'a Parse_env.t

module Grammar = struct
  type ('ctx, 'a, 'd) t' =
    | Eps : ('ctx, unit, 'd) t'
    | Seq : ('ctx, 'a, 'd) t * ('ctx, 'b, 'd) t -> ('ctx, 'a * 'b, 'd) t'
    | Chr : char -> ('ctx, char, 'd) t'
    | Bot : ('ctx, 'a, 'd) t'
    | Alt : ('ctx, 'a, 'd) t * ('ctx, 'a, 'd) t -> ('ctx, 'a, 'd) t'
    | Map : ('a -> 'b) * ('ctx, 'a, 'd) t -> ('ctx, 'b, 'd) t'
    | Fix : ('a * 'ctx, 'a, 'd) t -> ('ctx, 'a, 'd) t'
    | Star : ('ctx, 'a, 'd) t -> ('ctx, 'a list, 'd) t'
    | Var : ('ctx, 'a) Var.t -> ('ctx, 'a, 'd) t'

  and ('ctx, 'a, 'd) t = 'd * ('ctx, 'a, 'd) t'

  let data (d, _) = d

  let rec typeof : type ctx a d. ctx Type_env.t -> (ctx, a, d) t -> (ctx, a, Type.t) t =
   fun env (_, g) ->
    let data = data in
    match g with
    | Eps -> Type.eps, Eps
    | Seq (g1, g2) ->
      let env' = Type_env.map { f = (fun ty -> { ty with guarded = true }) } env in
      let g1 = typeof env g1 in
      let g2 = typeof env' g2 in
      Type.seq (data g1) (data g2), Seq (g1, g2)
    | Chr c -> Type.chr c, Chr c
    | Bot -> Type.bot, Bot
    | Alt (g1, g2) ->
      let g1 = typeof env g1 in
      let g2 = typeof env g2 in
      Type.alt (data g1) (data g2), Alt (g1, g2)
    | Map (f, g) ->
      let g (* TODO: is this right? *) = typeof env g in
      data g, Map (f, g)
    | Fix g ->
      let ty = Type.fix (fun ty -> data (typeof (ty :: env) g)) in
      Type.check ty.Type.guarded "fix must be guarded";
      let g = typeof (ty :: env) g in
      data g, Fix g
    | Star g ->
      let g = typeof env g in
      Type.star (data g), Star g
    | Var v -> Type_env.lookup env v, Var v
 ;;
end

let typeof env gram = Grammar.typeof env gram |> fst

let rec parse : type ctx a. (ctx, a, Type.t) Grammar.t -> ctx Parse_env.t -> a parser =
 fun (_, g) env ->
  let data = Grammar.data in
  let open Parse in
  match g with
  | Eps -> eps
  | Seq (g1, g2) ->
    let p1 = parse g1 env in
    let p2 = parse g2 env in
    seq p1 p2
  | Chr c -> chr c
  | Bot -> bot
  | Alt (g1, g2) -> alt (data g1) (parse g1 env) (data g2) (parse g2 env)
  | Map (f, g) -> parse g env |> map f
  | Star g ->
    let p = parse g env in
    let first_set = (data g).Type.first in
    let rec go ret s =
      match Stream.peek s with
      | Some c when Char_set.mem c first_set -> go (p s :: ret) s
      | _ -> List.rev ret
    in
    go []
  | Fix g ->
    let r = ref (fun _ -> assert false) in
    let p s = !r s in
    let q = parse g (p :: env) in
    r := q;
    p
  | Var n -> Parse_env.lookup env n
;;

module Hoas = struct
  module Ctx = Env (struct
    type _ t = unit
  end)

  type 'a t = { tdb : 'ctx. 'ctx Ctx.t -> ('ctx, 'a, unit) Grammar.t }

  let rec len : type n. n Ctx.t -> int =
   fun ctx -> match ctx with [] -> 0 | _ :: ctx -> 1 + len ctx
 ;;

  let rec tshift' : type a i j. int -> j Ctx.t -> (a * i) Ctx.t -> (j, a) Var.t =
   fun n c1 c2 ->
    match n, c1, c2 with
    (* Both the 'assert false' and the 'Obj.magic' are safe here,
       since we know (although it's not captured in the types) that
       (a * i) is a prefix of j.
       More details: "Unembedding Domain Specific Languages" §4.4.
     *)
    | _, [], _ -> assert false
    | 0, _ :: _, _ :: _ -> Stdlib.Obj.magic Var.Z
    | n, () :: c1, c2 -> Var.S (tshift' (n - 1) c1 c2)
 ;;

  let tshift : type a i j. j Ctx.t -> (a * i) Ctx.t -> (j, a) Var.t =
   fun c1 c2 -> tshift' (len c1 - len c2) c1 c2
 ;;

  let eps = { tdb = (fun _ -> (), Eps) }
  let chr c = { tdb = (fun _ -> (), Chr c) }
  let bot = { tdb = (fun _ -> (), Bot) }
  let seq f g = { tdb = (fun ctx -> (), Seq (f.tdb ctx, g.tdb ctx)) }
  let alt x y = { tdb = (fun ctx -> (), Alt (x.tdb ctx, y.tdb ctx)) }
  let map f a = { tdb = (fun ctx -> (), Map (f, a.tdb ctx)) }

  let fix : type b. (b t -> b t) -> b t =
   fun f ->
    { tdb =
        (fun i ->
          (), Fix ((f { tdb = (fun j -> (), Var (tshift j (() :: i))) }).tdb (() :: i)))
    }
 ;;

  let star g = { tdb = (fun p -> (), Star (g.tdb p)) }

  module Library = struct
    type assoc =
      | Left
      | Right

    let always x _ = x
    let ( ++ ) = seq
    let ( ==> ) p f = map f p
    let any gs = List.fold_left ~f:alt ~init:bot gs
    let option r = any [ eps ==> always None; (r ==> fun x -> Some x) ]
    let plus g = g ++ star g ==> fun (x, xs) -> x :: xs
    let charset s = any (List.map ~f:chr (String.to_list s))
    let lower = charset "abcdefghijklmnopqrstuvwxyz"
    let upper = charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let sep_by1 sep p = p ++ star (sep ++ p ==> snd) ==> fun (x, xs) -> x :: xs
    let sep_by sep p = option (sep_by1 sep p) ==> function None -> [] | Some xs -> xs

    module Sexp = struct
      type sexp =
        | Sym of string
        | Seq of sexp list

      let rec pp ppf =
        let open Fmt in
        function
        | Sym str -> string ppf str | Seq sexps -> parens (list pp ~sep:sp) ppf sexps
      ;;

      let paren p = chr '(' ++ p ++ chr ')' ==> fun ((_, x), _) -> x
      let word = plus lower ==> String.of_char_list

      let sexp =
        fix (fun sexp ->
            any
              [ (word ==> fun sym -> Sym sym)
              ; (paren (sep_by (chr ' ') sexp) ==> fun s -> Seq s)
              ])
      ;;
    end

    let infixr op base =
      let process (accum, rhs) =
        match rhs with None -> accum | Some (f, e') -> f accum e'
      in
      fix (fun g -> base ++ option (op ++ g) ==> process)
    ;;

    let infixl op base =
      let reassociate (init, oes) =
        List.fold_left ~f:(fun e (op, e') -> op e e') ~init oes
      in
      base ++ star (op ++ base) ==> reassociate
    ;;

    let infix ops base =
      let make_level base (fixity, op) =
        match fixity with Left -> infixl op base | Right -> infixr op base
      in
      List.fold_left ~f:make_level ~init:base ops
    ;;

    module Arith = struct
      let digits = plus (charset "0123456789")

      let num : float t =
        digits ++ chr '.' ++ digits
        ==> fun ((digits1, _), digits2) ->
        Float.of_string (String.of_char_list digits1 ^ "." ^ String.of_char_list digits2)
      ;;

      let arith =
        fix (fun arith ->
            infix
              [ Right, chr '^' ==> always Stdlib.Float.pow
              ; Left, chr '*' ==> always Float.( * )
              ; Left, chr '+' ==> always Float.( + )
              ]
              (any [ num; Sexp.paren arith ]))
      ;;
    end
  end
end

module Regex = struct
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

  let str s =
    s |> String.to_list |> List.map ~f:chr |> List.fold_right ~init:Eps ~f:( >>> )
  ;;

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
    match compare re1 re2 with
    | x when x < 0 -> ctor re1 re2
    | 0 -> re1
    | _ -> ctor re2 re1
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

  let complement = function
    | Complement re -> re
    | Empty -> star any
    | re -> Complement re
  ;;

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
end

let%test_module _ =
  (module struct
    open Hoas
    open Library

    let mk_gram p = p.tdb []
    let typeof' p = typeof [] (mk_gram p)
    let go ty = Fmt.pr "%a@." Type.pp (typeof' ty)

    let%expect_test "typechecking" =
      go eps;
      go (chr 'a');
      go bot;
      go upper;
      [%expect
        {|
        {first: [];
         flast: [];
         null: true;
         guarded: true}
        {first: [a];
         flast: [];
         null: false;
         guarded: true}
        {first: [];
         flast: [];
         null: false;
         guarded: true}
        {first: [A-Z];
         flast: [];
         null: false;
         guarded: true} |}]
    ;;

    let typecheck : type a. a Hoas.t -> (unit, a, Type.t) Grammar.t =
     fun { tdb } -> Grammar.typeof [] (tdb [])
   ;;

    let parse p = parse (typecheck p) []

    let go p pp str =
      try Fmt.pr "@[%a@]@." pp (parse p (Stream.of_string str)) with
      | Parse_error msg -> Fmt.pr "failed parse: %s@." msg
    ;;

    let%expect_test "chr" =
      go (chr 'c') Fmt.char "c";
      go (chr 'c') Fmt.char "d";
      [%expect {|
        c
        failed parse: Unexpected char 'd' (expected 'c') |}]
    ;;

    let%expect_test "eps" =
      go eps (Fmt.any "()") "";
      go eps (Fmt.any "()") "c";
      [%expect {|
        ()
        () |}]
    ;;

    let%expect_test "bot" =
      go bot (Fmt.any "()") "c";
      [%expect {| failed parse: bottom |}]
    ;;

    let%expect_test "seq" =
      go (seq (chr 'a') (chr 'b')) Fmt.(pair ~sep:comma char char) "ab";
      [%expect {| a, b |}]
    ;;

    let%expect_test "alt" =
      go (alt (chr 'a') (chr 'b')) Fmt.char "a";
      go (alt (chr 'a') (chr 'b')) Fmt.char "b";
      go (alt (chr 'a') (chr 'b')) Fmt.char "c";
      [%expect {|
        a
        b
        failed parse: No progress possible |}]
    ;;

    let%expect_test "star, plus, map" =
      go (star (chr 'a')) Fmt.(list char) "";
      go (star (chr 'a')) Fmt.(list char) "aaa";
      go (plus (chr 'a')) Fmt.(list char) "";
      go (plus (chr 'a')) Fmt.(list char) "aaa";
      go (map (List.map ~f:Char.uppercase) (plus (chr 'a'))) Fmt.(list char) "aaa";
      [%expect
        {|

        aaa
        failed parse: Unexpected end of stream
        aaa
        AAA |}]
    ;;

    let%expect_test "star again" =
      go (star (charset "ab")) Fmt.(list char) "abbb";
      [%expect "abbb"]
    ;;

    let%expect_test "any, option" =
      let go' = go (any [ chr 'a'; chr 'b' ]) Fmt.char in
      go' "a";
      go' "a";
      let go' = go (option (chr 'a')) Fmt.(option char) in
      go' "a";
      go' "";
      [%expect {|
        a
        a
        a |}]
    ;;

    let%expect_test "charset, upper, lower" =
      let go' = go (charset "ab") Fmt.char in
      go' "a";
      go' "c";
      let go' = go upper Fmt.char in
      go' "a";
      go' "A";
      let go' = go lower Fmt.char in
      go' "a";
      go' "A";
      [%expect
        {|
        a
        failed parse: No progress possible
        failed parse: No progress possible
        A
        a
        failed parse: No progress possible |}]
    ;;

    let%expect_test "sexp" =
      let go' = go Sexp.sexp Sexp.pp in
      go' "()";
      go' "foo";
      go' "(foo bar)";
      [%expect {|
        ()
        foo
        (foo bar) |}]
    ;;

    let%expect_test "infixr" =
      let p = infixr (chr ',' ==> fun _ -> List.append) (lower ==> List.return) in
      let go' = go p Fmt.(brackets (list ~sep:comma char)) in
      go' "a,b,c";
      [%expect {| [a, b, c] |}]
    ;;

    let%expect_test "infixl" =
      let p = infixl (chr ',' ==> fun _ -> List.append) (lower ==> List.return) in
      let go' = go p Fmt.(brackets (list ~sep:comma char)) in
      go' "a,b,c";
      [%expect {| [a, b, c] |}]
    ;;

    (* TODO:
     * Parse more general literals
     * Without needing parens
     * Allow whitespace
     * Check stream has been consumed
     *)
    let%expect_test "arith" =
      let go' = go Arith.digits Fmt.(list char) in
      go' "12";
      let go' = go (Arith.digits ++ chr '.' ==> fst) Fmt.(list char) in
      go' "12.";
      let go' = go (chr '.' ++ Arith.digits ==> snd) Fmt.(list char) in
      go' ".12";
      let go' = go Arith.num Fmt.float in
      go' "1.2";
      go' "0.1";
      go' "1.0";
      let go' = go Arith.arith Fmt.float in
      go' "(1.0+2.0)";
      go' "(1.0*2.0)";
      go' "(2.0^2.0)";
      [%expect
        {|
        12
        12
        12
        1.2
        0.1
        1
        3
        2
        4 |}]
    ;;
  end)
;;
