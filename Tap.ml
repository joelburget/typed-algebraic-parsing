open Base
module Stream = Stdlib.Stream

module Char_set = struct
  type t = (char, Char.comparator_witness) Set.t
end

type 'a parser = char Stream.t -> 'a

let error fmt = Stdlib.Format.kasprintf failwith fmt

module Type = struct
  type t =
    { first : Char_set.t
    ; flast : Char_set.t
    ; null : bool
    ; guarded : bool
    }

  let ( = ) t1 t2 =
    Set.equal t1.first t2.first
    && Set.equal t1.flast t2.flast
    && Bool.(t1.null = t2.null && t1.guarded = t2.guarded)
  ;;

  let check b msg = if b then () else failwith msg
  let empty = Set.empty (module Char)
  let singleton = Set.singleton (module Char)
  let ( ==> ) b cs = if b then cs else empty
  let bot = { first = empty; flast = empty; null = false; guarded = true }
  let eps = { first = empty; flast = empty; null = true; guarded = true }
  let chr c = { first = singleton c; flast = empty; null = false; guarded = true }
  let separable t1 t2 = Set.(is_empty (inter t1.flast t2.first)) && not t1.null
  let apart t1 t2 = Set.(is_empty (inter t1.first t2.first)) && not (t1.null && t2.null)

  let alt t1 t2 =
    check (apart t1 t2) "alt must be apart";
    { first = Set.union t1.first t2.first
    ; flast = Set.union t1.flast t2.flast
    ; null = t1.null || t2.null
    ; guarded = t1.guarded && t2.guarded
    }
  ;;

  let seq t1 t2 =
    check (separable t1 t2) "seq must be separable";
    { first = t1.first
    ; flast = Set.union t2.flast (t2.null ==> Set.union t2.first t1.flast)
    ; null = false
    ; guarded = t1.guarded
    }
  ;;

  let star t =
    { first = t.first
    ; flast = Set.union t.flast t.first
    ; null = true
    ; guarded = t.guarded
    }
  ;;

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
    | Some c' -> if Char.(c' = c) then c else error "Unexpected char"
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
      if Set.mem tp1.first c
      then p1 s
      else if Set.mem tp2.first c
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
end

module Env (T : sig
  type 'a t
end) =
struct
  type 'ctx t =
    | Empty : unit t
    | Nonempty : 'a T.t * 'ctx t -> ('a * 'ctx) t

  let rec lookup : type ctx a. ctx t -> (ctx, a) Var.t -> a T.t =
   fun ctx v ->
    match ctx, v with
    | Nonempty (x, _), Z -> x
    | Nonempty (_, xs), S v -> lookup xs v
    | _ -> .
 ;;

  type fn = { f : 'a. 'a T.t -> 'a T.t }

  let rec map : type ctx. fn -> ctx t -> ctx t =
   fun { f } -> function
    | Empty -> Empty
    | Nonempty (x, xs) -> Nonempty (f x, map { f } xs)
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

let rec typeof : type ctx a d. ctx Type_env.t -> (ctx, a, d) Grammar.t -> Type.t =
 fun env (_, g) ->
  match g with
  | Eps -> Type.eps
  | Seq (g1, g2) ->
    let env' = Type_env.map { f = (fun ty -> { ty with guarded = true }) } env in
    Type.seq (typeof env g1) (typeof env' g2)
  | Chr c -> Type.chr c
  | Bot -> Type.bot
  | Alt (g1, g2) -> Type.alt (typeof env g1) (typeof env g2)
  | Map (_, g) -> typeof env g (* TODO: is this right? *)
  | Fix g ->
    let ty = Type.fix (fun ty -> typeof (Nonempty (ty, env)) g) in
    Type.check ty.Type.guarded "fix must be guarded";
    typeof (Nonempty (ty, env)) g
  | Star g -> Type.star (typeof env g)
  | Var v -> Type_env.lookup env v
;;

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
      | None -> ret
      | Some c -> if Set.mem first_set c then go (p s :: ret) s else List.rev ret
    in
    go []
  | Fix g ->
    let r = ref (fun _ -> assert false) in
    let p s = !r s in
    let q = parse g (Nonempty (p, env)) in
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
   fun ctx -> match ctx with Empty -> 0 | Nonempty (_, ctx) -> 1 + len ctx
 ;;

  let rec tshift' : type a i j. int -> j Ctx.t -> (a * i) Ctx.t -> (j, a) Var.t =
   fun n c1 c2 ->
    match n, c1, c2 with
    (* Both the 'assert false' and the 'Obj.magic' are safe here,
       since we know (although it's not captured in the types) that
       (a * i) is a prefix of j.
       More details: "Unembedding Domain Specific Languages" §4.4.
     *)
    | _, Empty, _ -> assert false
    | 0, Nonempty _, Nonempty _ -> Stdlib.Obj.magic Var.Z
    | n, Nonempty ((), c1), c2 -> Var.S (tshift' (n - 1) c1 c2)
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
          ( ()
          , Fix
              ((f { tdb = (fun j -> (), Var (tshift j (Nonempty ((), i)))) }).tdb
                 (Nonempty ((), i))) ))
    }
 ;;

  module Library = struct
    type assoc =
      | Left
      | Right

    let always x _ = x
    let ( ++ ) = seq
    let ( ==> ) p f = map f p
    let any gs = List.fold_left ~f:alt ~init:bot gs
    let option r = any [ eps ==> always None; (r ==> fun x -> Some x) ]

    let star g =
      fix (fun rest -> any [ eps ==> always []; (g ++ rest ==> fun (x, xs) -> x :: xs) ])
    ;;

    let plus g = g ++ star g ==> fun (x, xs) -> x :: xs
    let charset s = any (List.map ~f:chr (String.to_list s))
    let lower = charset "abcdefghijklmnopqrstuvwxyz"
    let upper = charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let word = upper ++ star lower ==> fun (c, cs) -> String.of_char_list (c :: cs)

    module Sexp = struct
      type token =
        | SYMBOL of string
        | LPAREN
        | RPAREN

      let symbol = word ==> fun s -> SYMBOL s
      let lparen = chr '(' ==> always LPAREN
      let rparen = chr '(' ==> always RPAREN
      let token = any [ symbol; lparen; rparen ]

      type sexp =
        | Sym
        | Seq of sexp list

      let paren p = lparen ++ p ++ rparen ==> fun ((_, x), _) -> x

      let sexp =
        fix (fun sexp ->
            any [ symbol ==> always Sym; (paren (star sexp) ==> fun s -> Seq s) ])
      ;;
    end

    let infixr op base =
      fix (fun g ->
          base ++ option (op ++ g) ==> function e, None -> e | e, Some (f, e') -> f e e')
    ;;

    let infixl op base =
      let reassociate (e, oes) =
        List.fold_left ~f:(fun e (f, e') -> f e e') ~init:e oes
      in
      base ++ star (op ++ base) ==> reassociate
    ;;

    let infix aos base =
      let make_level base = function
        | Left, op -> infixl op base
        | Right, op -> infixr op base
      in
      List.fold_left ~f:make_level ~init:base aos
    ;;

    module Arith = struct
      let num = failwith "TODO"

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
