module Uchar = struct
  type token = Uchar.t
  type stream = (Uutf.decoder * Uchar.t option) ref

  let of_decoder decoder = ref (decoder, None)

  module Token = struct
    include Base.Uchar
    module Set = Char_class

    let pp = Char_class.pp_char
  end

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
  type stream = char Stdlib.Stream.t

  module Token = struct
    include Base.Char

    module Set = struct
      include Char_class
      include Char_class.Char
    end

    let pp ppf c = Fmt.pf ppf "%C" c
  end

  module Stream = struct
    type element = char
    type t = stream

    let peek = Stdlib.Stream.peek
    let junk = Stdlib.Stream.junk
  end
end
