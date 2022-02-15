type _ code = Ppxlib.expression

module Make (Ast : Ppxlib.Ast_builder.S) (Token_stream : Staged_signatures.Token_stream) :
  Staged_signatures.Parser
    with type token = Token_stream.Token.t
     and type token_tag = Token_stream.Token.tag
     and type stream = Token_stream.stream
     and type 'a parser = (Token_stream.stream -> 'a) Staged_signatures.code
     and type 'a v = 'a code
