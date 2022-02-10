module Make (Token_stream : Signatures.Token_stream) :
  Signatures.Parser
    with type token = Token_stream.Token.t
     and type stream = Token_stream.Stream.t
