module Uchar_token = struct
  include Base.Uchar
  module Set = Char_class
  module Interval = Char_class.Interval

  type tag = Uchar.t
  type set = Char_class.t
  type interval = Char_class.interval

  let tag tok = tok
  let pp = Char_class.pp_char
  let pp_tag = pp
end

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
  end

  type set = Set.t

  let tag tok = tok
  let pp ppf c = Fmt.pf ppf "%C" c
  let pp_tag = pp
end

module Uchar = struct
  type token = Uchar.t
  type token_tag = Uchar.t
  type token_set = Uchar_token.Set.t
  type stream = (Uutf.decoder * Uchar.t option) ref

  let of_decoder decoder = ref (decoder, None)

  module Token = Uchar_token

  module Stream = struct
    type element = Uchar.t
    type t = stream

    let decode_char decoder =
      match Uutf.decode decoder with
      | `Uchar c -> Some c
      | `Await | `End | `Malformed _ -> None
    ;;

    let peek ref =
      let decoder, char_opt = !ref in
      match char_opt with
      | Some c -> Some c
      | None ->
        decode_char decoder
        |> Base.Option.map ~f:(fun c ->
               ref := decoder, Some c;
               c)
    ;;

    let junk ref =
      let decoder, char_opt = !ref in
      match char_opt with
      | Some _ ->
        ref := decoder, None;
        ()
      | None ->
        let (_ : Uchar.t option) = decode_char decoder in
        ()
    ;;
  end
end

module Char = struct
  type token = char
  type token_tag = char
  type token_set = Char_token.Set.t
  type stream = char Stdlib.Stream.t

  module Token = Char_token

  module Stream = struct
    type element = char
    type t = stream

    let peek = Stdlib.Stream.peek
    let junk = Stdlib.Stream.junk
  end
end
