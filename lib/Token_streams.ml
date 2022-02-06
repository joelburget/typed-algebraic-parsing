module Uchar = struct
  type token = Uchar.t
  type stream = char Stdlib.Stream.t

  module Token = struct
    include Base.Uchar
    module Set = Char_class

    let pp = Char_class.pp_char
  end

  module Stream = struct
    type element = Uchar.t
    type t = stream

    let peek t = Stdlib.Stream.peek t |> Base.Option.map ~f:Uchar.of_char
    let junk = Stdlib.Stream.junk
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
