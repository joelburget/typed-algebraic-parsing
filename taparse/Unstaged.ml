open Base
open Prelude

module Make (Token_stream : Signatures.Token_stream) : sig
  include
    Signatures.Parser
      with type token = Token_stream.token
       and type token_tag = Token_stream.token_tag
       and type stream = Token_stream.Stream.t
       and type 'a parser = Token_stream.Stream.t -> 'a
       and type 'a v = 'a

  val parse : ('ctx, 'a, Type.t) Grammar.t -> 'ctx Parse_env.t -> 'a parser
end = struct
  module Token = Token_stream.Token
  module Stream = Token_stream.Stream

  type token = Token_stream.token
  type token_tag = Token_stream.token_tag
  type stream = Token_stream.stream
  type 'a parser = Stream.t -> 'a
  type 'a v = 'a

  module Type = Type.Make (Token)

  module Parse = struct
    open Type

    let eps a _s = a

    let tok set s =
      match Stream.peek s with
      | None -> parse_error "Unexpected end of stream"
      | Some c' ->
        if Token.Set.mem set (Token.tag c')
        then (
          Stream.junk s;
          c')
        else
          parse_error "Unexpected token '%a' (expected '%a')" Token.pp c' Token.Set.pp set
    ;;

    let bot _ = parse_error "bottom"

    let seq p1 p2 s =
      let a = p1 s in
      let b = p2 s in
      a, b
    ;;

    let map f p s = f (p s)

    let alt msg tp1 p1 tp2 p2 s =
      match Stream.peek s with
      | None ->
        if tp1.null
        then p1 s
        else if tp2.null
        then p2 s
        else (
          match msg with
          | None -> parse_error "Unexpected end of stream"
          | Some msg -> parse_error "Unexpected end of stream (%s)" msg)
      | Some c ->
        let tag = Token.tag c in
        if Token.Set.mem tp1.first tag
        then p1 s
        else if Token.Set.mem tp2.first tag
        then p2 s
        else if tp1.null
        then p1 s
        else if tp2.null
        then p2 s
        else (
          match msg with
          | None -> parse_error "No progress possible"
          | Some msg -> parse_error "No progress possible (%s)" msg)
    ;;
  end

  module Type_env = Env (struct
    type _ t = Type.t
  end)

  module Parse_env = Env (struct
    type 'a t = Stream.t -> 'a
  end)

  module Grammar = struct
    type ('ctx, 'a, 'd) t' =
      | Eps : 'a -> ('ctx, 'a, 'd) t'
      | Seq : ('ctx, 'a, 'd) t * ('ctx, 'b, 'd) t -> ('ctx, 'a * 'b, 'd) t'
      | Tok : Token.tag list -> ('ctx, Token.t, 'd) t'
      | Bot : ('ctx, 'a, 'd) t'
      | Alt : string option * ('ctx, 'a, 'd) t * ('ctx, 'a, 'd) t -> ('ctx, 'a, 'd) t'
      | Map : ('a -> 'b) * ('ctx, 'a, 'd) t -> ('ctx, 'b, 'd) t'
      | Fix : ('a * 'ctx, 'a, 'd) t -> ('ctx, 'a, 'd) t'
      | Star : ('ctx, 'a, 'd) t -> ('ctx, 'a list, 'd) t'
      | Var : ('ctx, 'a) Var.t -> ('ctx, 'a, 'd) t'

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
      | Alt (msg, g1, g2) ->
        let g1 = typeof env g1 in
        let g2 = typeof env g2 in
        Type.alt (data g1) (data g2), Alt (msg, g1, g2)
      | Map (f, g) ->
        let g = typeof env g in
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

  let typeof env gram = Grammar.typeof env gram |> fst

  let rec parse : type ctx a. (ctx, a, Type.t) Grammar.t -> ctx Parse_env.t -> a parser =
   fun (_, g) env ->
    let data = Grammar.data in
    let open Parse in
    match g with
    | Eps a -> eps a
    | Seq (g1, g2) ->
      let p1 = parse g1 env in
      let p2 = parse g2 env in
      seq p1 p2
    | Tok c -> tok (Token.Set.of_list c)
    | Bot -> bot
    | Alt (msg, g1, g2) -> alt msg (data g1) (parse g1 env) (data g2) (parse g2 env)
    | Map (f, g) -> parse g env |> map f
    | Star g ->
      let p = parse g env in
      let first_set = (data g).Type.first in
      let rec go ret s =
        match Stream.peek s with
        | Some c when Token.Set.mem first_set (Token.tag c) -> go (p s :: ret) s
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

  module Construction = struct
    module Ctx = Env (struct
      type _ t = unit
    end)

    module Tshift = Tshift.Make (Ctx)

    type 'a ctx = 'a Ctx.t
    type 'a v = 'a
    type ('ctx, 'a, 'd) grammar = ('ctx, 'a, 'd) Grammar.t
    type nonrec token = token
    type nonrec token_tag = token_tag
    type 'a t = { tdb : 'ctx. 'ctx Ctx.t -> ('ctx, 'a, unit) Grammar.t }

    let eps a = { tdb = (fun _ -> (), Eps a) }
    let tok c = { tdb = (fun _ -> (), Tok c) }
    let bot = { tdb = (fun _ -> (), Bot) }
    let seq f g = { tdb = (fun ctx -> (), Seq (f.tdb ctx, g.tdb ctx)) }

    let alt ?failure_msg x y =
      { tdb = (fun ctx -> (), Alt (failure_msg, x.tdb ctx, y.tdb ctx)) }
    ;;

    let map f a = { tdb = (fun ctx -> (), Map (f, a.tdb ctx)) }

    let fix : type b. (b t -> b t) -> b t =
     fun f ->
      { tdb =
          (fun i ->
            ( ()
            , Fix
                ((f { tdb = (fun j -> (), Var (Tshift.tshift j (() :: i))) }).tdb
                   (() :: i)) ))
      }
   ;;

    let star g = { tdb = (fun p -> (), Star (g.tdb p)) }
  end
end
