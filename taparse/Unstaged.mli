module Make (Token_stream : Signatures.Token_stream) :
  Signatures.Parser
    with type token = Token_stream.token
     and type token_tag = Token_stream.token_tag
     and type token_set = Token_stream.token_set
     and type stream = Token_stream.Stream.t
     and type 'a parser = Token_stream.Stream.t -> 'a
     and type 'a v = 'a
