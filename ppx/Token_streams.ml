type _ code = Ppxlib.expression

module Char_token = struct
  include Base.Char

  type tag = char
  type interval = Char_class.interval

  module Set = struct
    include Char_class
    include Char_class.Char
  end

  module Interval = struct
    type t = interval

    let to_tuple ival =
      let x, y = Char_class.Interval.to_tuple ival in
      Uchar.to_char x, Uchar.to_char y
    ;;

    let to_pattern ~loc ival =
      let x, y = Char_class.Interval.to_tuple ival in
      let x, y = Uchar.(to_char x, to_char y) in
      Ppxlib.Ast_builder.Default.(ppat_interval ~loc (Pconst_char x) (Pconst_char y))
    ;;

    let to_pattern ~loc ivals =
      let pat =
        match ivals with
        | [] -> failwith "Empty interval list not supported"
        | ival :: ivals ->
          Base.List.fold ivals ~init:(to_pattern ~loc ival) ~f:(fun accum ival ->
              Ppxlib.Ast_builder.Default.ppat_or ~loc accum (to_pattern ~loc ival))
      in
      pat, None
    ;;
  end

  type set = Set.t

  let tag tok = tok
  let pp ppf c = Fmt.pf ppf "%C" c
  let pp_tag = pp

  open Ppxlib

  let unquote ~loc = [%expr fun x -> x]
  let quote = Ast_builder.Default.echar

  let reflect = function
    | { pexp_desc = Pexp_constant (Pconst_char c); _ } -> Some c
    | _ -> None
  ;;
end

module Char = struct
  open Ppxlib

  module Quote = struct
    let int ~loc i = Ast_builder.Default.eint ~loc i
  end

  type token = char
  type token_tag = char
  type token_set = Char_token.Set.t
  type stream = char Stdlib.Stream.t

  module Token = Char_token

  module Stream = struct
    type element = char
    type t = stream

    type context =
      { index : int * int code
      ; string : char code
      ; length : int code
      }

    let peek = Stdlib.Stream.peek
    let junk = Stdlib.Stream.junk

    let staged_junk ({ index = s, d; _ } as context) k =
      k { context with index = s + 1, d }
    ;;

    let index ~loc ctx : expression =
      match ctx.index with 0, i -> i | n, i -> [%expr [%e Quote.int ~loc n] + [%e i]]
    ;;

    let staged_peek ~loc ctx f =
      let i = index ~loc ctx in
      [%expr
        if Base.Int.([%e i] >= [%e ctx.length])
        then [%e f ctx None]
        else (
          let c = Base.String.unsafe_get [%e ctx.string] [%e i] in
          [%e f ctx (Some [%expr c])])]
    ;;

    let init ~loc f =
      [%expr
        fun ~index s ->
          let n = Base.String.length s in
          [%e
            let ctx =
              { index = 0, [%expr index]; string = [%expr s]; length = [%expr n] }
            in
            f ctx]]
    ;;

    type 'a mkcall =
      { mkcall : 'b. context -> (context -> 'a code -> 'b code) -> 'b code }

    (* val genletrec : locus_t -> ('a code -> 'a code) -> 'a code *)
    (* mkbody : (context -> 'a mkcall -> 'a code) -> unit *)
    (*
    let genfun ~loc mkbody =
      (* TODO add frame [%expr fun stream -> []] *)
      mkbody (fun ctx mkcall ->
          let call =
            { mkcall =
                (fun ctx h ->
                  [%expr
                    let x = [%e f] [%e ctx.stream] in
                    [%e h ctx [%expr x]]])
            }
          in
          mkbody ctx call)
         *)

    let genfun ~loc:_ _ = ()
  end
end
