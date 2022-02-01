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
        | `Tok _ ->
          if Char_class.(is_empty (inter ctx.values cls))
          then cdcomp ctx (k `No)
          else failwith "TODO"
        | `Unknown ->
          stream_peek ctx.stream_context (fun stream_context -> function
            | None -> cdcomp { ctx with stream_context; next = `EOF } (k `Eof)
            | Some x ->
              test_tag ~complete:false cls x (function
                  | None ->
                    cdcomp { ctx with values = Char_class.diff ctx.values cls } (k `No)
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
  end
end

let term_extension =
  Extension.declare
    "parser"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ _expr ->
      let module Ast =
        Ast_builder.Make (struct
          let loc = loc
        end)
      in
      let module Ir' = Ir (Ast) in
      let module Codegen = Ir'.Codegen in
      let ir : _ Ir'.t = failwith "TODO" in
      Codegen.generate ir)
;;

let () =
  Ppxlib.Driver.register_transformation
    "typed_algebraic_parsing"
    ~rules:[ Context_free.Rule.extension term_extension ]
;;
