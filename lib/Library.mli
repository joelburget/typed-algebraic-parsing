module Make (Construction : Signatures.Construction) :
  Signatures.Library with type 'a t := 'a Construction.t
