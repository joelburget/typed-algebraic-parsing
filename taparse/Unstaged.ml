open Base
open Prelude

module Make (Token_stream : Signatures.Token_stream) :
  Signatures.Parser
    with type token = Token_stream.token
     and type token_tag = Token_stream.token_tag
     and type token_set = Token_stream.token_set
     and type stream = Token_stream.Stream.t
     and type 'a parser = Token_stream.Stream.t -> 'a
     and type 'a v = 'a = struct
  module Token = Token_stream.Token
  module Stream = Token_stream.Stream

  type token = Token_stream.token
  type token_tag = Token_stream.token_tag
  type token_set = Token_stream.token_set
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

    let message_with_stack msg stack =
      match stack with
      | [] -> msg
      | strs -> Fmt.(str "%s (%a)" msg (list ~sep:comma string) strs)
    ;;

    let alt msg stack tp1 p1 tp2 p2 s =
      match Stream.peek s with
      | None ->
        if tp1.null
        then p1 s
        else if tp2.null
        then p2 s
        else (
          let msg =
            match msg with
            | None -> "Unexpected end of stream"
            | Some msg -> Fmt.str "Unexpected end of stream (%s)" msg
          in
          parse_error "%s" (message_with_stack msg stack))
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
          let msg =
            match msg with
            | None -> "No progress possible"
            | Some msg -> Fmt.str "No progress possible (%s)" msg
          in
          parse_error "%s" (message_with_stack msg stack))
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
      | Seq :
          Prelude.Grammar_provenance.t * ('ctx, 'a, 'd) t * ('ctx, 'b, 'd) t
          -> ('ctx, 'a * 'b, 'd) t'
      | Tok : Token.Set.t -> ('ctx, Token.t, 'd) t'
      | Bot : ('ctx, 'a, 'd) t'
      | Alt :
          Prelude.Grammar_provenance.t
          * string option
          * ('ctx, 'a, 'd) t
          * ('ctx, 'a, 'd) t
          -> ('ctx, 'a, 'd) t'
      | Map :
          Prelude.Grammar_provenance.t * ('a -> 'b) * ('ctx, 'a, 'd) t
          -> ('ctx, 'b, 'd) t'
      | Annot : string * ('ctx, 'a, 'd) t -> ('ctx, 'a, 'd) t'
      | Fix : Prelude.Grammar_provenance.t * ('a * 'ctx, 'a, 'd) t -> ('ctx, 'a, 'd) t'
      | Star : Prelude.Grammar_provenance.t * ('ctx, 'a, 'd) t -> ('ctx, 'a list, 'd) t'
      | Var : ('ctx, 'a) Var.t -> ('ctx, 'a, 'd) t'
      | Fail : string -> ('ctx, 'a, 'd) t'

    and ('ctx, 'a, 'd) t = 'd * ('ctx, 'a, 'd) t'

    let data (d, _) = d

    let label : type ctx a d. (ctx, a, d) t' -> string option = function
      | Seq _ | Alt _ | Map _ | Fix _ | Star _ -> None
      | Annot (msg, _) -> Some msg
      | Eps _ -> Some "Eps"
      | Tok tok_set -> Some (Fmt.str "Tok %a" Token.Set.pp tok_set)
      | Bot -> Some "Bot"
      | Var v -> Some (Fmt.str "Var %a" Var.pp v)
      | Fail msg -> Some (Fmt.str "Fail %S" msg)
    ;;

    let mk_tree : type ctx a d. (ctx, a, d) t' -> Tree_fmt.t =
      let open Prelude in
      let open Grammar_provenance in
      let open Tree_fmt in
      let rec collect_children
          : type ctx a d. Tree_fmt.t list -> (ctx, a, d) t' -> Tree_fmt.t list
        =
       fun children g ->
        match g with
        | Annot (msg, (_, t)) -> mk msg (collect_children [] t) :: children
        | Eps _ -> mk "Eps" [] :: children
        | Seq (provenance, (_, a), (_, b)) -> binary provenance children "Seq" a b
        | Tok tok_set -> mk (Fmt.str "Tok %a" Token.Set.pp tok_set) [] :: children
        | Bot -> mk "Bot" [] :: children
        | Alt (provenance, msg, (_, a), (_, b)) ->
          let label =
            match msg with None -> "Alt" | Some msg -> Fmt.str "Alt ~failure_msg:%S" msg
          in
          binary provenance children label a b
        | Map (provenance, _, (_, a)) -> unary provenance children "Map" a
        | Fix (provenance, (_, a)) -> unary provenance children "Fix" a
        | Star (provenance, (_, a)) -> unary provenance children "Star" a
        | Var v -> mk (Fmt.str "Var %a" Var.pp v) [] :: children
        | Fail msg -> mk (Fmt.str "Fail %S" msg) [] :: children
      and binary : type ctx a b d. _ -> _ -> _ -> (ctx, a, d) t' -> (ctx, b, d) t' -> _ =
       fun provenance children label a b ->
        match provenance with
        | User_defined ->
          let children' = collect_children (collect_children [] a) b in
          mk label children' :: children
        | Generated -> collect_children (collect_children children a) b
      and unary : type ctx a d. _ -> _ -> _ -> (ctx, a, d) t' -> _ =
       fun provenance children label a ->
        match provenance with
        | User_defined ->
          let children' = collect_children [] a in
          mk label children' :: children
        | Generated -> collect_children children a
      in
      fun g0 ->
        match collect_children [] g0 with
        | [ child ] -> child
        | children -> mk "(root)" children
    ;;

    let pp_tree : type ctx a d. int option -> (ctx, a, d) t' Fmt.t =
     fun max_depth ppf t -> Tree_fmt.pp ?max_depth ppf (mk_tree t)
   ;;

    let pp_labels ppf labels = Fmt.(list string ~sep:(any ".")) ppf (List.rev labels)

    let typeof : type ctx a d. ctx Type_env.t -> (ctx, a, d) t -> (ctx, a, Type.t) t =
     fun env g0 ->
      let rec typeof
          : type ctx a d.
            string list -> ctx Type_env.t -> (ctx, a, d) t -> (ctx, a, Type.t) t
        =
       fun labels env (_, g) ->
        let pp_g ppf max_depth = pp_tree max_depth ppf g in
        let labels =
          match label g with None -> labels | Some label -> label :: labels
        in
        match g with
        | Eps a -> Type.eps, Eps a
        | Seq (provenance, g1, g2) ->
          let env' = Type_env.map { f = (fun ty -> { ty with guarded = true }) } env in
          let g1 = typeof labels env g1 in
          let g2 = typeof labels env' g2 in
          Type.seq labels pp_g (data g1) (data g2), Seq (provenance, g1, g2)
        | Tok set -> Type.tok set, Tok set
        | Bot -> Type.bot, Bot
        | Alt (provenance, msg, g1, g2) ->
          let g1 = typeof labels env g1 in
          let g2 = typeof labels env g2 in
          Type.alt labels pp_g (data g1) (data g2), Alt (provenance, msg, g1, g2)
        | Map (provenance, f, g) ->
          let g = typeof labels env g in
          data g, Map (provenance, f, g)
        | Fix (provenance, g') ->
          let ty = Type.fix (fun ty -> data (typeof labels (ty :: env) g')) in
          Prelude.type_assert ty.Type.guarded (fun ppf max_depth ->
              Fmt.pf
                ppf
                "fix must be guarded @[(%a@ ->@ %a)@]"
                pp_labels
                labels
                (pp_tree max_depth)
                g);
          let g' = typeof labels (ty :: env) g' in
          data g', Fix (provenance, g')
        | Star (provenance, g') ->
          let g' = typeof labels env g' in
          Type.star labels pp_g (data g'), Star (provenance, g')
        | Var v -> Type_env.lookup env v, Var v
        | Annot (msg, a) ->
          let g = typeof labels env a in
          data g, Annot (msg, g)
        | Fail msg -> Type.eps, Fail msg
      in
      typeof [] env g0
   ;;
  end

  let typeof env gram = Grammar.typeof env gram |> fst

  let rec parse
      : type ctx a.
        (ctx, a, Type.t) Grammar.t -> ctx Parse_env.t -> string list -> a parser
    =
   fun (_, g) env stack ->
    let data = Grammar.data in
    let open Parse in
    match g with
    | Eps a -> eps a
    | Seq (_provenance, g1, g2) ->
      let p1 = parse g1 env stack in
      let p2 = parse g2 env stack in
      seq p1 p2
    | Tok set -> tok set
    | Bot -> bot
    | Alt (_provenance, msg, g1, g2) ->
      alt msg stack (data g1) (parse g1 env stack) (data g2) (parse g2 env stack)
    | Map (_provenance, f, g) -> parse g env stack |> map f
    | Star (_provenance, g) ->
      let p = parse g env stack in
      let first_set = (data g).Type.first in
      let rec go ret s =
        match Stream.peek s with
        | Some c when Token.Set.mem first_set (Token.tag c) -> go (p s :: ret) s
        | _ -> List.rev ret
      in
      go []
    | Fix (_provenance, g) ->
      let r = ref (fun _ -> assert false) in
      let p s = !r s in
      let q = parse g (p :: env) stack in
      r := q;
      p
    | Var n -> Parse_env.lookup env n
    | Annot (annot, g) -> parse g env (annot :: stack)
    | Fail msg -> parse_error "%s" (message_with_stack msg stack)
 ;;

  module Construction' = struct
    module Ctx = Env (struct
      type _ t = unit
    end)

    module Tshift = Tshift.Make (Ctx)

    type 'a ctx = 'a Ctx.t
    type 'a v = 'a
    type ('ctx, 'a, 'd) grammar = ('ctx, 'a, 'd) Grammar.t
    type nonrec token = token
    type nonrec token_tag = token_tag
    type nonrec token_set = token_set
    type 'a t = { tdb : 'ctx. 'ctx Ctx.t -> ('ctx, 'a, unit) Grammar.t }

    let eps a = { tdb = (fun _ -> (), Eps a) }
    let tok c = { tdb = (fun _ -> (), Tok c) }
    let bot = { tdb = (fun _ -> (), Bot) }

    let seq ?(provenance = Prelude.Grammar_provenance.User_defined) f g =
      { tdb = (fun ctx -> (), Seq (provenance, f.tdb ctx, g.tdb ctx)) }
    ;;

    let alt ?(provenance = Prelude.Grammar_provenance.User_defined) ?failure_msg x y =
      { tdb = (fun ctx -> (), Alt (provenance, failure_msg, x.tdb ctx, y.tdb ctx)) }
    ;;

    let map ?(provenance = Prelude.Grammar_provenance.User_defined) f a =
      { tdb = (fun ctx -> (), Map (provenance, f, a.tdb ctx)) }
    ;;

    let ( <?> ) a msg = { tdb = (fun ctx -> (), Annot (msg, a.tdb ctx)) }

    let fix : type b. ?provenance:Prelude.Grammar_provenance.t -> (b t -> b t) -> b t =
     fun ?(provenance = Prelude.Grammar_provenance.User_defined) f ->
      let tdb : type c. c ctx -> (c, b, unit) grammar =
       fun i ->
        let bt : b t = f { tdb = (fun j -> (), Var (Tshift.tshift j (() :: i))) } in
        let gram : (b * c, b, unit) grammar = bt.tdb (() :: i) in
        (), Fix (provenance, gram)
      in
      { tdb }
   ;;

    let star ?(provenance = Prelude.Grammar_provenance.User_defined) g =
      { tdb = (fun p -> (), Star (provenance, g.tdb p)) }
    ;;

    let fail msg = { tdb = (fun _ -> (), Fail msg) }
  end

  module Construction = struct
    include Construction'
    include Library.Make (Construction')
  end

  let typecheck : type a. a Construction.t -> (unit, a, Type.t) Grammar.t =
   fun { tdb } -> Grammar.typeof [] (tdb [])
 ;;

  let parse'_exn grammar env = parse grammar env []
  let parse_exn construction = parse (typecheck construction) Parse_env.[] []
end
