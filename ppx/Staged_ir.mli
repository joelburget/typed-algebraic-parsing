module Make (Token_stream : Staged_signatures.Token_stream) (Ast : Ppxlib.Ast_builder.S) :
  Staged_signatures.Ir
    with type token = Token_stream.token
     and type token_tag = Token_stream.token_tag
     and type token_set = Token_stream.token_set
