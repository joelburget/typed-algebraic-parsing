module Uchar :
  Signatures.Token_stream with type token = Uchar.t and type stream = char Stdlib.Stream.t

module Char :
  Signatures.Token_stream with type token = char and type stream = char Stdlib.Stream.t
