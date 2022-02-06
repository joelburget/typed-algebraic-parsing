module Tap (Token_stream : Signatures.Token_stream) :
  Parser.S with type token = Token_stream.Token.t

module String : sig
  include Parser.S with type token = Uchar.t

  val charset : string -> Uchar.t Construction.t
  val lower : Uchar.t Construction.t
  val upper : Uchar.t Construction.t

  module Sexp : sig
    type sexp =
      | Sym of string
      | Seq of sexp list

    val pp : sexp Fmt.t
    val paren : 'a Construction.t -> 'a Construction.t
    val sexp : sexp Construction.t
  end

  module Arith : sig
    val num : float Construction.t
    val arith : float Construction.t
  end
end
