module Make (Ast : Ppxlib.Ast_builder.S) (Token_stream : Signatures.Token_stream) :
  Staged_signatures.Parser
    with type token = Token_stream.Token.t
     and type stream = Token_stream.Stream.t
     and type 'a parser = (Token_stream.Stream.t -> 'a) Staged_signatures.code
