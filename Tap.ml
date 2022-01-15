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

  let empty = Set.empty (module Char)
  let singleton = Set.singleton (module Char)
  let ( ==> ) b cs = if b then cs else empty
  let bot = { first = empty; flast = empty; null = false; guarded = true }
  let eps = { first = empty; flast = empty; null = true; guarded = true }
  let chr c = { first = singleton c; flast = empty; null = false; guarded = true }

  let alt t1 t2 =
    { first = Set.union t1.first t2.first
    ; flast = Set.union t1.flast t2.flast
    ; null = t1.null || t2.null
    ; guarded = t1.guarded && t2.guarded
    }
  ;;

  (* Assumes t1 not null *)
  (* TODO: check *)
  let seq t1 t2 =
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

module Type_env = struct
  type 'ctx t =
    | Empty : unit t
    | Nonempty : 'ctx t -> ('a * 'ctx) t
end

module Env = struct
  type 'ctx t =
    | Empty : unit t
    | Nonempty : 'ctx t -> ('a * 'ctx) t
end

module Grammar = struct
  type ('ctx, 'a) t =
    | Eps : ('ctx, unit) t
    | Seq : ('ctx, 'a) t * ('ctx, 'b) t -> ('ctx, 'a * 'b) t
    | Chr : char -> ('ctx, char) t
    | Bot : ('ctx, 'a) t
    | Alt : ('ctx, 'a) t * ('ctx, 'a) t -> ('ctx, 'a) t
    | Map : ('a -> 'b) * ('ctx, 'a) t -> ('ctx, 'b) t
    | Fix : ('a * 'ctx, 'a) t -> ('ctx, 'a) t
    | Var : ('ctx, 'a) Var.t -> ('ctx, 'a) t
  (* TODO: star? *)
end

let typeof : type ctx a. ctx Type_env.t -> (ctx, a) Grammar.t -> Type.t =
 fun _env -> function
  | Eps -> Type.eps
  | Seq (_g1, _g2) -> failwith "TODO"
  | Chr c -> Type.chr c
  | Bot -> Type.bot
  | Alt _ -> failwith "TODO"
  | Map _ -> failwith "TODO"
  | Fix _ -> failwith "TODO"
  | Var _ -> failwith "TODO"
;;

let rec parse : type ctx a. (ctx, a) Grammar.t -> ctx Env.t -> a parser =
 fun g env ->
  let open Parse in
  match g with
  | Eps -> eps
  | Seq (g1, g2) ->
    let p1 = parse g1 env in
    let p2 = parse g2 env in
    seq p1 p2
  | Chr c -> chr c
  | Bot -> bot
  | Alt _ -> failwith "TODO"
  | Map (f, g) -> parse g env |> map f
  | Fix _ -> failwith "TODO"
  | Var _ -> failwith "TODO"
;;

module Hoas = struct
  type 'a t = { tdb : 'ctx. 'ctx Env.t -> ('ctx, 'a) Grammar.t }

  let rec len : type n. n Env.t -> int =
   fun ctx -> match ctx with Empty -> 0 | Nonempty ctx -> 1 + len ctx
 ;;

  let rec tshift' : type a i j. int -> j Env.t -> (a * i) Env.t -> (j, a) Var.t =
   fun n c1 c2 ->
    match n, c1, c2 with
    (* Both the 'assert false' and the 'Obj.magic' are safe here,
       since we know (although it's not captured in the types) that
       (a * i) is a prefix of j.
       More details: "Unembedding Domain Specific Languages" §4.4.
     *)
    | _, Empty, _ -> assert false
    | 0, Nonempty _, Nonempty _ -> Stdlib.Obj.magic Var.Z
    | n, Nonempty c1, c2 -> Var.S (tshift' (n - 1) c1 c2)
 ;;

  let tshift : type a i j. j Env.t -> (a * i) Env.t -> (j, a) Var.t =
   fun c1 c2 -> tshift' (len c1 - len c2) c1 c2
 ;;

  let eps = { tdb = (fun _ -> Eps) }
  let chr c = { tdb = (fun _ -> Chr c) }
  let bot = { tdb = (fun _ -> Bot) }
  let seq f g = { tdb = (fun ctx -> Seq (f.tdb ctx, g.tdb ctx)) }
  let alt x y = { tdb = (fun ctx -> Alt (x.tdb ctx, y.tdb ctx)) }
  let map f a = { tdb = (fun ctx -> Map (f, a.tdb ctx)) }

  let fix : type b. (b t -> b t) -> b t =
   fun f ->
    { tdb =
        (fun i ->
          Fix ((f { tdb = (fun j -> Var (tshift j (Nonempty i))) }).tdb (Nonempty i)))
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
