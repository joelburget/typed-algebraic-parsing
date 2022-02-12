open Base
open Ppxlib
open Prelude

type _ code = expression

module Make (Ast : Ast_builder.S) (Token_stream : Signatures.Token_stream) :
  Staged_signatures.Parser
    with type token = Token_stream.token
     and type token_tag = Token_stream.token_tag
     and type stream = Token_stream.Stream.t
     and type 'a parser = (Token_stream.Stream.t -> 'a) code = struct
  module Token = Token_stream.Token
  module Stream = Token_stream.Stream

  type token = Token.t
  type token_tag = Token.tag
  type stream = Token_stream.stream
  type 'a parser = (Token_stream.Stream.t -> 'a) code

  module Type = Type.Make (Token)

  module Type_env = Env (struct
    type _ t = Type.t
  end)

  (* TODO: remove duplicate *)
  module Parse_env = Env (struct
    type 'a t = Stream.t -> 'a
  end)

  module Grammar = struct
    type ('ctx, 'a, 'd) t' =
      | Eps : 'a code -> ('ctx, 'a, 'd) t'
      | Seq : ('ctx, 'a, 'd) t * ('ctx, 'b, 'd) t -> ('ctx, 'a * 'b, 'd) t'
      | Tok : Token.tag list -> ('ctx, Token.t, 'd) t'
      | Bot : ('ctx, 'a, 'd) t'
      | Alt : ('ctx, 'a, 'd) t * ('ctx, 'a, 'd) t -> ('ctx, 'a, 'd) t'
      | Map : ('a code -> 'b code) * ('ctx, 'a, 'd) t -> ('ctx, 'b, 'd) t'
      | Fix : ('a * 'ctx, 'a, 'd) t -> ('ctx, 'a, 'd) t'
      | Var : ('ctx, 'a) Var.t -> ('ctx, 'a, 'd) t'
      | Star : ('ctx, 'a, 'd) t -> ('ctx, 'a list, 'd) t'

    and ('ctx, 'a, 'd) t = 'd * ('ctx, 'a, 'd) t'

    let data (d, _) = d

    let rec typeof : type ctx a d. ctx Type_env.t -> (ctx, a, d) t -> (ctx, a, Type.t) t =
     fun env (_, g) ->
      match g with
      | Eps a -> Type.eps, Eps a
      | Seq (g1, g2) ->
        let env' = Type_env.map { f = (fun ty -> { ty with guarded = true }) } env in
        let g1 = typeof env g1 in
        let g2 = typeof env' g2 in
        Type.seq (data g1) (data g2), Seq (g1, g2)
      | Tok c -> Type.tok (Token.Set.of_list c), Tok c
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
        assert' ty.Type.guarded "fix must be guarded";
        let g = typeof (ty :: env) g in
        data g, Fix g
      | Star g ->
        let g = typeof env g in
        Type.star (data g), Star g
      | Var v -> Type_env.lookup env v, Var v
   ;;
  end

  type 'a typechecked = (unit, 'a, Type.t) Grammar.t
  type 'a v = 'a code

  module Construction = struct
    module Ctx = Env (struct
      type _ t = unit
    end)

    module Tshift = Tshift.Make (Ctx)

    type 'a ctx = 'a Ctx.t
    type nonrec 'a v = 'a v
    type ('ctx, 'a, 'd) grammar = ('ctx, 'a, 'd) Grammar.t
    type token = Token.t
    type token_tag = Token.tag
    type 'a t = { tdb : 'ctx. 'ctx ctx -> ('ctx, 'a, unit) grammar }

    let crush : type ctx a x. (ctx, a, x) Grammar.t -> (ctx, a, x) Grammar.t =
     fun _ -> failwith "TODO"
   ;;

    (*
      let rec loop (toks : Token.t list) (summands : (ctx, a, x) Grammar.t list)
        = function
        | _, Grammar.Alt (l, r) ->
          let toks, summands = loop toks summands l in
          let toks, summands = loop toks summands r in
          toks, summands
        | _, Tok t -> t :: toks, summands
        | e -> toks, e :: summands
      in
      let alt e es =
        List.fold_right ~f:(fun (d, x) y -> Alt ((d, x), (d, y))) ~init:e es
      in
      fun ((d, _) as e) ->
        ( d
        , match loop [] [] e with
          | [], [] -> Bot
          | toks, [] -> Tok toks
          | [], (_, e) :: es -> alt es e
          | toks, es -> alt es (Tok toks) )
             *)

    let eps a = { tdb = (fun _ -> (), Eps a) }
    let tok tag_list = { tdb = (fun _ -> (), Tok tag_list) }
    let bot = { tdb = (fun _ -> (), Bot) }
    let seq f g = { tdb = (fun i -> (), Seq (f.tdb i, g.tdb i)) }
    let alt f g = { tdb = (fun i -> crush ((), Alt (f.tdb i, g.tdb i))) }
    let map f a = { tdb = (fun ctx -> (), Map (f, a.tdb ctx)) }

    let fix f =
      { tdb =
          (fun i ->
            let open Ctx in
            ( ()
            , Fix
                ((f { tdb = (fun j -> (), Var (Tshift.tshift j (() :: i))) }).tdb
                   (() :: i)) ))
      }
    ;;

    let star g = { tdb = (fun i -> (), Star (g.tdb i)) }
  end

  let typecheck : 'a Construction.t -> 'a typechecked =
   fun { tdb } -> Grammar.typeof Type_env.[] (tdb Construction.Ctx.[])
 ;;

  (* XXX why two typeofs? *)
  let typeof env gram = Grammar.typeof env gram |> fst

  module Compile = struct
    module Ir = Staged_ir.Make (Token_stream) (Ast)

    module Parse = struct
      open Ast
      open Ir

      let eps = return

      let tok c =
        peek c
        @@ function
        | `Eof -> fail "Expected chr"
        | `Yes c -> junk >>= fun _ -> return c
        | `No -> Caml.Printf.kprintf fail "wrong token"
      ;;

      let bot _ = return [%expr failwith "impossible"]
      let seq p1 p2 = p1 >>= fun a -> p2 >>= fun b -> return [%expr [%e a], [%e b]]

      let alt tp1 p1 tp2 p2 =
        let open Type in
        peek_mem tp1.first
        @@ function
        | `Eof when tp1.null -> p1
        | `Eof -> p2
        | `Yes -> p1
        | `No ->
          peek_mem tp2.first
          @@ (function
          | `Eof when tp1.null -> p1
          | `Eof -> p2
          | `Yes -> p2
          | `No when tp1.null -> p1
          | `No when tp2.null -> p2
          | `No -> fail "No progress possible!")
      ;;

      let map f p = p >>= fun x -> return (f x)

      let star tp g =
        fix (fun loop ->
            peek_mem Token.Set.(Infix.(any - tp.Type.first))
            @@ function
            | `Eof -> return ([%expr []] : _ list code)
            | `Yes -> return ([%expr []] : _ list code)
            | `No ->
              g
              >>= fun x ->
              loop >>= fun acc -> return ([%expr [%e x] :: [%e acc]] : _ list code))
      ;;

      let fix _ = failwith "TODO"
    end

    module Parse_env = Env (struct
      type 'a t = 'a Ir.t
    end)

    let rec parse : type ctx a. (ctx, a, Type.t) Grammar.t -> ctx Parse_env.t -> a Ir.t =
     fun (_, g) penv ->
      let open Parse in
      let data (d, _) = d in
      match g with
      | Eps v -> eps v
      | Seq (g1, g2) ->
        let p1 = parse g1 penv in
        let p2 = parse g2 penv in
        seq p1 p2
      | Tok t -> tok t
      | Bot -> bot ()
      | Alt (g1, g2) ->
        let p1 = parse g1 penv in
        let p2 = parse g2 penv in
        alt (data g1) p1 (data g2) p2
      | Map (f, g') ->
        let p = parse g' penv in
        map f p
      | Star g1 ->
        let p1 = parse g1 penv in
        star (data g1) p1
      | Var n -> Parse_env.lookup penv n
      | Fix g' -> fix (fun p -> parse g' (p :: penv))
   ;;

    let compile (g : 'a typechecked) : 'a Caml.Stream.t code =
      Ir.Codegen.generate (parse g Parse_env.[])
    ;;
  end

  let compile = Compile.compile
  let parse _ = failwith "TODO"
end
