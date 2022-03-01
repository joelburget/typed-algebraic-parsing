module Free_monoid (T : sig
  type t

  val ( = ) : t -> t -> bool
end) =
struct
  type t = T.t list

  let identity = []
  let ( <> ) xs ys = xs @ ys
  let ( = ) = List.equal T.( = )
end
