open Base
open Ppxlib
open Prelude

type _ code = expression

module Make (Token_stream : Staged_signatures.Token_stream) (Ast : Ast_builder.S) :
  Staged_signatures.Ir
    with type token = Token_stream.token
     and type token_tag = Token_stream.token_tag
     and type token_set = Token_stream.token_set = struct
  type token = Token_stream.token
  type token_tag = Token_stream.token_tag
  type token_set = Token_stream.token_set
  type _ stream (* TODO *)

  let loc = Ast.loc

  module Todo = struct
    type 'a t (* TODO *)
  end

  module Parse_env = Env (Todo)
  module Token = Token_stream.Token

  type _ recid = ..

  type 'a comp =
    (* let x = r () in c[x] *)
    | Rec_call : 'a recvar * (expression -> 'b comp) -> 'b comp
    (* junk; c *)
    | Junk : 'b comp -> 'b comp
    (* match read with Eof → kEof | c when c ∈ s → kYes | _ → kNo *)
    | Peek_mem : Token.Set.t * ([ `Eof | `Yes | `No ] -> 'b comp) -> 'b comp
    (* match read with Eof → kEof
         | c when hastag(c,t) → kYes[extract t c]
         | _ → kNo *)
    | Peek : Token.tag list * ([ `Eof | `Yes of expression | `No ] -> 'b comp) -> 'b comp
    (* fail s *)
    | Fail : string -> _ comp
    (* return x *)
    | Return : expression -> 'a comp

  and 'a recvar =
    { body : 'a comp -> 'a comp
    ; mutable name : 'a recid list
    }

  type 'a t = 'a comp

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
    | Peek_mem (cs, k') -> Peek_mem (cs, fun v -> bind (k' v) k)
    (* let z = (match read with Eof → kEof
                              | c when hastag(c,t) → kYes[extract t c]
                              | _ → kNo) in c'[z]
         ↝ match read with Eof → let z = kEof in c'[z]
                         | c when hastag(c,t) → let z = kYes[extract t c] in c'[z]
                         | _ → let z = kNo in c'[z] *)
    | Peek (tags, k') -> Peek (tags, fun v -> bind (k' v) k)
    (* let z = (fail s) in c'
         ↝ fail s  *)
    | Fail s -> Fail s
    (* let z = (return x) in c'
         ↝ c'[x]  *)
    | Return x -> k x
 ;;

  let return v = Return v
  let ( >>= ) = bind
  let fail s = Fail s
  let junk = Junk (Return [%expr ()])
  let peek_mem cls k = Peek_mem (cls, k)
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
      ; values : Token.Set.t
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

    (*
    let ifmem'
        :  expression -> (Token.Set.t * expression) list -> otherwise:expression
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
       *)

    let ifmem
        : expression -> Token.Set.t -> expression -> otherwise:expression -> expression
      =
     fun c token_set rhs ~otherwise ->
      let open Ast in
      let intervals = Token.Set.intervals token_set in
      let pat_of_ival ival =
        let x, y = Token.Interval.to_tuple ival in
        ppat_interval (Token.quote_constant x) (Token.quote_constant y)
      in
      let lhs =
        match intervals with
        | [] -> failwith "Empty interval list not supported"
        | ival :: ivals ->
          List.fold ivals ~init:(pat_of_ival ival) ~f:(fun accum ival ->
              ppat_or accum (pat_of_ival ival))
      in
      let branch = case ~lhs ~guard ~rhs in
      let branches = [ branch; case ~lhs:[%pat? _] ~guard ~rhs:otherwise ] in
      pexp_match c branches
   ;;

    let test_tag
        :  complete:bool -> Token.Set.t -> expression -> (expression option -> expression)
        -> expression
      =
     fun ~complete cls expr k ->
      if complete
      then k (Some expr)
      else ifmem expr cls (k (Some expr)) ~otherwise:(k None)
   ;;

    module Tokens : sig
      val ifmem
        :  Token.t code
        -> Token.Set.t
        -> Token.Set.t
        -> then_:(Token.Set.t -> 'a code)
        -> else_:(Token.Set.t -> 'a code)
        -> 'a code
    end = struct
      type 'a static_dynamic =
        | Sta of 'a
        | Dyn of 'a code

      (* Build a dynamic test from an interval test *)
      let within : token code -> Token.interval -> bool code =
       fun c ival ->
        let x, y = Token.Interval.to_tuple ival in
        [%expr
          [%e Token.unquote ~loc] [%e Token.quote ~loc x] <= [%e c]
          && [%e c] < [%e Token.unquote ~loc] [%e Token.quote ~loc y]]
     ;;

      let member : token code -> token_set -> bool static_dynamic =
       fun c s ->
        match Token.Set.intervals s with
        | [] -> Sta false
        | i :: is ->
          Dyn
            (List.fold
               ~f:(fun a i -> [%expr [%e a] || [%e within c i]])
               ~init:(within c i)
               is)
     ;;

      let ntests s =
        s
        |> Token.Set.intervals
        |> List.fold
             ~f:(fun v ival ->
               let x, y = Token.Interval.to_tuple ival in
               v + if Token.compare x y = 0 then 1 else 2)
             ~init:0
      ;;

      let ifmem (c : Token.t code) (values : Token.Set.t) (cs : Token.Set.t) ~then_ ~else_
        =
        (* if c ∈ cs
       then [c is in thencs]
       else [c is in elsecs] *)
        (* let thencs, elsecs = partition (Token.Set.mem cs) values in *)
        let thencs = Token.Set.inter values cs in
        let elsecs = Token.Set.Infix.(values - cs) in
        if Token.Set.is_empty elsecs
        then then_ thencs
        else if Token.Set.is_empty thencs
        then else_ elsecs
        else (
          let thencs, elsecs, then_, else_ =
            if ntests thencs < ntests elsecs
            then thencs, elsecs, then_, else_
            else elsecs, thencs, else_, then_
          in
          match member c thencs with
          | Sta _ -> assert false (* :-( *)
          | Dyn test -> [%expr if [%e test] then [%e then_ thencs] else [%e else_ elsecs]])
      ;;
    end

    let any = Token.Set.any

    let rec cdcomp : type a. context -> a comp -> expression =
     fun ctx -> function
      | Rec_call (({ name; body = _ } as r), k') as c ->
        (match resolve name with
        | Some { mkcall } ->
          mkcall ctx.stream_context (fun stream_context x ->
              cdcomp { stream_context; values = any; next = `Unknown } (k' x))
        | None ->
          let () = r.name <- R (failwith "TODO") :: r.name in
          cdcomp ctx c)
      | Junk c ->
        stream_junk ctx.stream_context (fun stream_context ->
            let ctx = { stream_context; next = `Unknown; values = any } in
            cdcomp ctx c)
      | Peek_mem (s, k') ->
        (match ctx.next with
        | `EOF -> cdcomp ctx (k' `Eof)
        | `Tok x ->
          Tokens.ifmem
            x
            ctx.values
            s
            ~then_:(fun values -> cdcomp { ctx with values } (k' `Yes))
            ~else_:(fun values -> cdcomp { ctx with values } (k' `No))
        | `Unknown ->
          stream_peek ctx.stream_context
          @@ fun stream_context -> (function
               | None -> cdcomp { ctx with stream_context; next = `EOF } (k' `Eof)
               | Some x ->
                 let ctx = { ctx with stream_context; next = `Tok x } in
                 Tokens.ifmem
                   x
                   ctx.values
                   s
                   ~then_:(fun values -> cdcomp { ctx with values } (k' `Yes))
                   ~else_:(fun values -> cdcomp { ctx with values } (k' `No))))
      | Peek (tags, k) ->
        let tagset = Token.Set.of_list tags in
        (match ctx.next with
        | `EOF -> cdcomp ctx (k `Eof)
        | `Tok x ->
          if Token.Set.(is_empty (inter ctx.values tagset))
          then cdcomp ctx (k `No)
          else (
            let complete = Token.Set.is_subset ctx.values tagset in
            let tags' = Token.Set.inter ctx.values tagset in
            test_tag ~complete tags' x (function
                | None ->
                  cdcomp
                    { ctx with values = Token.Set.Infix.(ctx.values - tagset) }
                    (k `No)
                | Some x -> cdcomp { ctx with values = tags' } (k (`Yes x))))
        | `Unknown ->
          stream_peek ctx.stream_context (fun stream_context -> function
            | None -> cdcomp { ctx with stream_context; next = `EOF } (k `Eof)
            | Some x ->
              test_tag ~complete:false tagset x (function
                  | None ->
                    cdcomp
                      { ctx with values = Token.Set.Infix.(ctx.values - tagset) }
                      (k `No)
                  | Some x ->
                    cdcomp
                      { ctx with values = Token.Set.inter ctx.values tagset }
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
          let ctx = { stream_context; next = `Unknown; values = any } in
          cdcomp ctx expr)
   ;;
  end
end
