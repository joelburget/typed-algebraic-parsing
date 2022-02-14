type _ code = Ppxlib.expression

module Make (Ast : Ppxlib.Ast_builder.S) (Token_stream : Signatures.Token_stream) :
  Staged_signatures.Parser
    with type token = Token_stream.Token.t
     and type token_tag = Token_stream.Token.tag
     and type stream = Token_stream.Stream.t
     and type 'a parser = (Token_stream.Stream.t -> 'a) Staged_signatures.code
     and type 'a v = 'a code
