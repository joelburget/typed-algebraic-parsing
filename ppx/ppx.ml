open Base
open Ppxlib

module Ir (Ast : Ast_builder.S) : sig
  type 'a t

  val return : expression -> 'a t
  val ( >>= ) : 'a t -> (expression -> 'b t) -> 'b t
  val fail : string -> 'a t
  val junk : unit t

  (* val peek_mem : Char_class.t -> ([ `Yes | `No | `Eof ] -> 'b t) -> 'b t *)
  val peek : Char_class.t -> ([ `Yes of expression | `No | `Eof ] -> 'b t) -> 'b t
  val fix : ('b t -> 'b t) -> 'b t

  module Codegen : sig
    val generate : 'a t -> expression

    val generate_ir
      :  'ctx Unstaged.String.Parse_env.t
      -> ('ctx, 'a, Unstaged.String.Type.t) Unstaged.String.Grammar.t
      -> 'a t
  end
end = struct
  let loc = Ast.loc

  type _ recid = ..

  type 'a comp =
    (* let x = r () in c[x] *)
    | Rec_call : 'a recvar * (expression -> 'b comp) -> 'b comp
    (* junk; c *)
    | Junk : 'b comp -> 'b comp
    (* match read with Eof → kEof | c when c ∈ s → kYes | _ → kNo *)
    (* | Peek_mem : Char_class.t * ([ `Eof | `Yes | `No ] -> 'b comp) -> 'b comp *)
    (* match read with Eof → kEof
         | c when hastag(c,t) → kYes[extract t c]
         | _ → kNo *)
    | Peek : Char_class.t * ([ `Eof | `Yes of expression | `No ] -> 'b comp) -> 'b comp
    (* fail s *)
    | Fail : string -> _ comp
    (* return x *)
    | Return : expression -> 'a comp

  and 'a recvar =
    { body : 'a comp -> 'a comp
    ; mutable name : 'a recid list
    }

  let rec bind : type a b. a comp -> (expression -> b comp) -> b comp =
   fun m k ->
    match m with
    (* let z = (let x = r () in c[x]) in c'[z]
         ↝ let x = r () in
           let z = c[x] in c'[z]  *)
    | Rec_call (r, k') -> Rec_call (r, fun x -> bind (k' x) k)
    (* let z = (junk; c) in c'[z]
         ↝ junk; let z = c in c'[z]  *)
    | Junk c -> Junk (bind c k)
    (* let z = (match read with Eof → kEof | c when c ∈ s → kYes | _ → kNo) in c'[z]
         ↝ match read with
           | Eof → let z = kEof in c'[z]
           | c when c ∈ s → let z = kYes in c'[z]
           | _ → let z = kNo in c'[z] *)
    (* | Peek_mem (cs, k') -> Peek_mem (cs, fun v -> bind (k' v) k) *)
    (* let z = (match read with Eof → kEof
                              | c when hastag(c,t) → kYes[extract t c]
                              | _ → kNo) in c'[z]
         ↝ match read with Eof → let z = kEof in c'[z]
                         | c when hastag(c,t) → let z = kYes[extract t c] in c'[z]
                         | _ → let z = kNo in c'[z] *)
    | Peek (tag, k') -> Peek (tag, fun v -> bind (k' v) k)
    (* let z = (fail s) in c'
         ↝ fail s  *)
    | Fail s -> Fail s
    (* let z = (return x) in c'
         ↝ c'[x]  *)
    | Return x -> k x
 ;;

  type 'a t = 'a comp

  let return v = Return v
  let ( >>= ) = bind
  let fail s = Fail s
  let junk = Junk (Return [%expr ()])

  (* let peek_mem cls k = Peek_mem (cls, k) *)
  let peek tag k = Peek (tag, k)
  let fix body = Rec_call ({ body; name = [] }, fun v -> Return v)

  module Codegen = struct
    let loc = Ast.loc
    let guard = None

    module Quote = struct
      let string str = Ast_builder.Default.estring ~loc str
      let int i = Ast_builder.Default.eint ~loc i
    end

    type stream_context =
      { index : int * expression
      ; string : expression
      ; length : expression
      }

    type context =
      { stream_context : stream_context
      ; values : Char_class.t
      ; next : [ `EOF | `Tok of expression | `Unknown ]
            (* rec_locus: Genletrec.locus_t; *)
      }

    let stream_junk ({ index = s, d; _ } as context) k =
      k { context with index = s + 1, d }
    ;;

    let context_index ctx : expression =
      match ctx.index with 0, i -> i | n, i -> [%expr [%e Quote.int n] + [%e i]]
    ;;

    let stream_peek (* { index; string; length } *) ctx f =
      let i = context_index ctx in
      [%expr
        if [%e i]
        then [%e f ctx None]
        else (
          let c = Base.String.unsafe_get [%e ctx.string] [%e i] in
          [%e f ctx (Some [%expr c])])]
    ;;

    type mkcall =
      { mkcall :
          stream_context -> (stream_context -> expression -> expression) -> expression
      }

    type 'a recid += R of mkcall

    let rec resolve = function [] -> None | R x :: _ -> Some x | _ :: xs -> resolve xs

    let ifmem
        :  expression -> (Char_class.t * expression) list -> otherwise:expression
        -> expression
      =
     fun c cases ~otherwise ->
      let open Ast in
      let branches =
        List.map cases ~f:(fun (_cls, rhs) ->
            let lhs = failwith "TODO" in
            case ~lhs ~guard ~rhs)
      in
      let branches = branches @ [ case ~lhs:[%pat? _] ~guard ~rhs:otherwise ] in
      pexp_match c branches
   ;;

    let test_tag
        :  complete:bool -> Char_class.t -> expression
        -> (expression option -> expression) -> expression
      =
     fun ~complete cls expr k ->
      if complete
      then k (Some expr)
      else ifmem expr [ cls, k (Some expr) ] ~otherwise:(k None)
   ;;

    let rec cdcomp : type a. context -> a comp -> expression =
     fun ctx -> function
      | Rec_call (({ name; body = _ } as r), k') as c ->
        (match resolve name with
        | Some { mkcall } ->
          mkcall ctx.stream_context (fun stream_context x ->
              cdcomp { stream_context; values = Char_class.any; next = `Unknown } (k' x))
        | None ->
          let () = r.name <- R (failwith "TODO") :: r.name in
          cdcomp ctx c)
      | Junk c ->
        stream_junk ctx.stream_context (fun stream_context ->
            let ctx = { stream_context; next = `Unknown; values = Char_class.any } in
            cdcomp ctx c)
      | Peek (cls, k) ->
        (match ctx.next with
        | `EOF -> cdcomp ctx (k `Eof)
        | `Tok x ->
          if Char_class.(is_empty (inter ctx.values cls))
          then cdcomp ctx (k `No)
          else (
            let complete = Char_class.is_subset ctx.values cls in
            let tags' = Char_class.inter ctx.values cls in
            test_tag ~complete tags' x (function
                | None ->
                  cdcomp { ctx with values = Char_class.Infix.(ctx.values - cls) } (k `No)
                | Some x -> cdcomp { ctx with values = tags' } (k (`Yes x))))
        | `Unknown ->
          stream_peek ctx.stream_context (fun stream_context -> function
            | None -> cdcomp { ctx with stream_context; next = `EOF } (k `Eof)
            | Some x ->
              test_tag ~complete:false cls x (function
                  | None ->
                    cdcomp
                      { ctx with values = Char_class.Infix.(ctx.values - cls) }
                      (k `No)
                  | Some x ->
                    cdcomp
                      { ctx with values = Char_class.inter ctx.values cls }
                      (k (`Yes x)))))
      | Fail s -> [%expr failwith [%e Quote.string s]]
      | Return x -> x
   ;;

    let stream_init f =
      [%expr
        fun ~index s ->
          let n = Base.String.length s in
          [%e
            let ctx =
              { index = 0, [%expr index]; string = [%expr s]; length = [%expr n] }
            in
            f ctx]]
    ;;

    let generate : type a. a comp -> expression =
     fun expr ->
      stream_init (fun stream_context ->
          let ctx = { stream_context; next = `Unknown; values = Char_class.any } in
          cdcomp ctx expr)
   ;;

    module Parser = Unstaged.String
    open Parser

    let tok c =
      peek c
      @@ function
      | `Eof -> fail "Expected tok"
      | `Yes c -> junk >>= fun _ -> return c
      | `No -> Stdlib.Printf.kprintf fail "wrong token"
    ;;

    let alt _ _ _ _ = failwith "TODO"
    let seq p1 p2 = p1 >>= fun a -> p2 >>= fun b -> return [%expr [%e a], [%e b]]
    let map f p = p >>= fun x -> return (f x)
    let star tp g = failwith "TODO"

    type _ code = expression

    let rec generate_ir : type ctx a. ctx Parse_env.t -> (ctx, a, Type.t) Grammar.t -> a t
      =
     fun ctx (_, grammar) ->
      let data (d, _) = d in
      match grammar with
      | Grammar.Eps expr -> return expr
      | Seq (g1, g2) ->
        let p1 = generate_ir ctx g1 in
        let p2 = generate_ir ctx g2 in
        seq p1 p2
      | Tok t -> tok (Char_class.singleton t)
      | Bot -> return [%expr failwith "impossible"]
      | Alt (g1, g2) ->
        let p1 = generate_ir ctx g1 in
        let p2 = generate_ir ctx g2 in
        alt (data g1) p1 (data g2) p2
      | Map (f, g) -> map f (generate_ir ctx g)
      | Fix g -> fix (fun p -> generate_ir g (p :: ctx))
      | Star g1 ->
        let p1 = generate_ir ctx g1 in
        star (data g1) p1
      | Var n -> Parse_env.lookup ctx n
   ;;
  end
end

let expand_parser ~loc ~path:_ expr =
  let module Ast =
    Ast_builder.Make (struct
      let loc = loc
    end)
  in
  let module Ir' = Ir (Ast) in
  let module Codegen = Ir'.Codegen in
  let module Parser = Unstaged.String in
  let open Parser in
  let module Expander = struct
    open Ir'

    type 'a typechecked = (unit, 'a, Type.t) Grammar.t

    (*
    let bot _ = return [%expr failwith "Impossible"]
    let eps = return
    *)

    let typecheck : 'a Construction.t -> 'a typechecked =
     fun { tdb } -> Grammar.typeof Type_env.[] (tdb Construction.Ctx.[])
   ;;

    let reflect : expression -> 'a Construction.t =
     fun { pexp_desc; _ } ->
      let open Construction in
      match pexp_desc with
      | Pexp_ident { txt; loc = _ } ->
        (match txt with
        | Lident "bot" -> bot
        | Lident "eps" -> eps
        | _ -> failwith "TODO")
      | Pexp_apply _ -> failwith "TODO"
      | _ -> failwith "TODO"
   ;;

    let ir_of_expr expr = expr |> reflect |> typecheck |> Codegen.generate_ir Parse_env.[]
  end
  in
  let ir : _ Ir'.t = Expander.ir_of_expr expr in
  Codegen.generate ir
;;

let term_extension =
  Extension.declare
    "parser"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_parser
;;

let () =
  Ppxlib.Driver.register_transformation
    "typed_algebraic_parsing"
    ~rules:[ Context_free.Rule.extension term_extension ]
;;
