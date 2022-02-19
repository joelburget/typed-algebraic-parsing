module Make (Token_stream : Signatures.Token_stream) : sig
  include
    Signatures.Parser
      with type token = Token_stream.token
       and type token_tag = Token_stream.token_tag
       and type stream = Token_stream.Stream.t
       and type 'a parser = Token_stream.Stream.t -> 'a
       and type 'a v = 'a

  val parse : ('ctx, 'a, Type.t) Grammar.t -> 'ctx Parse_env.t -> 'a parser
end