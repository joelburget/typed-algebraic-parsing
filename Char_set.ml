open Base

let min_char = Char.of_int_exn 0
let max_char = Char.of_int_exn 255

module Core = struct
  module Elt = struct
    type t = char

    let compare = Char.compare
    let zero = min_char

    let succ c =
      c |> Char.to_int |> Base.Int.succ |> Char.of_int |> Option.value ~default:c
    ;;

    let pred c =
      c |> Char.to_int |> Base.Int.pred |> Char.of_int |> Option.value ~default:c
    ;;

    let sub a b =
      let a, b = Char.(to_int a, to_int b) in
      Char.of_int_exn (a - b)
    ;;

    let add a b =
      let a, b = Char.(to_int a, to_int b) in
      Char.of_int_exn (a + b)
    ;;

    let to_string c = Fmt.str "%C" c
  end

  include Diet.Make (Elt)

  let interval_sexp_of_t interval =
    let atom c = Sexp.Atom (Elt.to_string c) in
    let x, y = Interval.(x interval, y interval) in
    Sexp.List [ atom x; atom y ]
  ;;

  let sexp_of_t iset =
    Sexp.List (fold (fun ival -> ival |> interval_sexp_of_t |> List.cons) iset [])
  ;;
end

include Core
include Comparable.Make (Core)

let of_interval interval = add interval empty
let singleton c = of_interval (Interval.make c c)
let any = of_interval (Interval.make min_char max_char)
let complement = diff any

let pp ppf char_set =
  Fmt.pf ppf "@[[";
  iter
    (fun interval ->
      let x, y = Interval.(x interval, y interval) in
      if Char.(x = y) then Fmt.pf ppf "%c" x else Fmt.pf ppf "%c-%c" x y)
    char_set;
  Fmt.pf ppf "]@]"
;;
