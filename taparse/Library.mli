module Make (Construction : Signatures.Construction with type 'a v = 'a) :
  Signatures.Library with type 'a t := 'a Construction.t
