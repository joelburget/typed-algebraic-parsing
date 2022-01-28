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
let range c1 c2 = of_interval (Interval.make c1 c2)
let singleton c = range c c
let any = of_interval (Interval.make min_char max_char)
let complement = diff any

exception Unexpected_sexp

let interval_of_sexp = function
  | Sexp.List [ Atom x; Atom y ] -> Interval.make (Char.of_string x) (Char.of_string y)
  | sexp -> raise (Sexp.Of_sexp_error (Unexpected_sexp, sexp))
;;

let t_of_sexp = function
  | Sexp.List intervals ->
    intervals
    |> List.map ~f:(fun sexp -> sexp |> interval_of_sexp |> of_interval)
    |> List.fold ~init:empty ~f:union
  | sexp -> raise (Sexp.Of_sexp_error (Unexpected_sexp, sexp))
;;

let pp_char ppf c =
  if Char.is_print c then Fmt.pf ppf "%c" c else Fmt.pf ppf "\\u%d" (Char.to_int c)
;;

let pp ppf char_set =
  Fmt.pf ppf "@[[";
  iter
    (fun interval ->
      let x, y = Interval.(x interval, y interval) in
      if Char.(x = min_char && y = max_char)
      then Fmt.pf ppf "."
      else if Char.(x = y)
      then pp_char ppf x
      else Fmt.pf ppf "%a-%a" pp_char x pp_char y)
    char_set;
  Fmt.pf ppf "]@]"
;;

let interval_to_tuple interval = Interval.(x interval, y interval)
let intervals iset = fold (fun ival -> ival |> interval_to_tuple |> List.cons) iset []

let choose iset =
  let interval = choose iset in
  Interval.(x interval, y interval)
;;
