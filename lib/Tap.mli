module Tap (Token_stream : Signatures.Token_stream) :
  Signatures.Parser
    with type token = Token_stream.Token.t
     and type stream = Token_stream.Stream.t

module String :
  Signatures.String_parsers
    with type token = Uchar.t
     and type stream = char Stdlib.Stream.t
