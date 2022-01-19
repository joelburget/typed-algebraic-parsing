open Base

let min_char = Char.of_int_exn 0
let max_char = Char.of_int_exn 255

module Range = struct
  type t = char * char

  module Relation = struct
    type t =
      | Less_no_overlap (** {v
      [  ]
           [  ]
                            v} *)
      | Less_overlapping (** {v
      [   ]
        [   ]
                             v} *)
      | Equal (** {v
      [  ]
      [  ]
                  v} *)
      | Within (** {v
        [  ]
      [      ]
                   v} *)
      | Containing (** {v
      [      ]
        [  ]
                       v} *)
      | Greater_overlapping (** {v
          [  ]
        [  ]
                                v} *)
      | Greater_no_overlap (** {v
             [  ]
        [  ]
                               v} *)

    let pp ppf =
      let string = Fmt.string in
      function
      | Less_no_overlap -> string ppf "Less_no_overlap"
      | Less_overlapping -> string ppf "Less_overlapping"
      | Equal -> string ppf "Equal"
      | Within -> string ppf "Within"
      | Containing -> string ppf "Containing"
      | Greater_overlapping -> string ppf "Greater_overlapping"
      | Greater_no_overlap -> string ppf "Greater_no_overlap"
    ;;
  end

  let ( = ) (x1, y1) (x2, y2) = Char.(x1 = x2 && y1 = y2)

  let compare (x1, y1) (x2, y2) =
    let x = Char.compare x1 x2 in
    if x <> 0 then x else Char.compare y1 y2
  ;;

  let sexp_of_t (x, y) = Sexp.(List [ Atom "Range"; Char.sexp_of_t x; Char.sexp_of_t y ])

  include (val Comparator.make ~compare ~sexp_of_t)

  let pp ppf (c1, c2) =
    if Char.(c1 = c2) then Fmt.pf ppf "%C" c1 else Fmt.pf ppf "%C-%C" c1 c2
  ;;

  let compare' (x1, x2) (y1, y2) =
    let open Char in
    match compare x1 y1 with
    | z when Int.(z < 0) ->
      (match compare x2 y1 with
      | z when Int.(z < 0) -> Relation.Less_no_overlap
      | 0 -> Less_overlapping
      | _ ->
        (match compare x2 y2 with
        | z when Int.(z <= 0) -> Containing
        | _ -> Less_overlapping))
    | 0 ->
      (match compare x2 y2 with
      | z when Int.(z < 0) -> Within
      | 0 -> Equal
      | _ -> Containing)
    | _ ->
      (match compare x1 y2 with
      | z when Int.(z > 0) -> Greater_no_overlap
      | 0 -> Greater_overlapping
      | _ ->
        (match compare x2 y2 with
        | z when Int.(z <= 0) -> Within
        | _ -> Greater_overlapping))
  ;;

  let%expect_test "compare'" =
    let cvt (x, y) = Char.of_int_exn x, Char.of_int_exn y in
    let go x y = compare' (cvt x) (cvt y) |> Fmt.pr "%a@." Relation.pp in
    go (0, 0) (1, 1) (* Less_no_overlap *);
    go (0, 2) (1, 3) (* Less_overlapping *);
    go (0, 1) (0, 1) (* Equal *);
    go (0, 1) (0, 2) (* Within *);
    go (1, 3) (0, 2) (* Greater_overlapping *);
    go (0, 1) (0, 0) (* Containing *);
    go (1, 1) (0, 0) (* Greater_no_overlap *);
    [%expect
      {|
      Less_no_overlap
      Containing
      Equal
      Within
      Greater_overlapping
      Containing
      Greater_no_overlap |}]
  ;;
end

module T = struct
  type t = Range.t list

  let compare = List.compare Range.compare
  let sexp_of_t = List.sexp_of_t Range.sexp_of_t

  include (val Comparator.make ~compare ~sexp_of_t)
end

include T
include Comparable.Make (T)

let ( = ) = List.equal Range.( = )
let equal = ( = )
let pp = Fmt.(brackets (list Range.pp))

let succ c =
  match c |> Char.to_int |> Int.succ |> Char.of_int with None -> c | Some c -> c
;;

let pred c =
  match c |> Char.to_int |> Int.pred |> Char.of_int with None -> c | Some c -> c
;;

open Char

let rec union xs ys =
  match xs, ys with
  | ranges, [] | [], ranges -> ranges
  | x :: xs', y :: ys' ->
    let x1, x2 = x in
    let y1, y2 = y in
    (match Range.compare' x y with
    | Less_no_overlap -> x :: union xs' ys
    | Greater_no_overlap -> y :: union xs ys'
    | Equal -> x :: union xs ys'
    | Less_overlapping | Within -> union xs' ((min x1 y1, y2) :: ys')
    | Greater_overlapping | Containing -> union ((min x1 y1, x2) :: xs) ys')
;;

let rec inter xs ys =
  match xs, ys with
  | ranges, [] | [], ranges -> ranges
  | x :: xs', y :: ys' ->
    let x1, x2 = x in
    let y1, y2 = y in
    (match Range.compare' x y with
    | Less_no_overlap -> inter xs' ys
    | Greater_no_overlap -> inter xs ys'
    | Equal -> x :: inter xs ys'
    | Less_overlapping | Within -> (max x1 y1, x2) :: inter xs' ys
    | Greater_overlapping | Containing -> (max x1 y1, y2) :: inter xs ys')
;;

let rec diff xs ys =
  match xs, ys with
  | _, [] -> xs
  | [], _ -> []
  | x :: xs', y :: ys' ->
    let x1, x2 = x in
    let y1, y2 = y in
    (match Range.compare' x y with
    | Less_no_overlap -> x :: diff xs' ys
    | Greater_no_overlap -> diff xs ys'
    | Equal | Within -> diff xs' ys'
    | Less_overlapping -> (y1, x2) :: diff xs' ((x2, y2) :: ys')
    | Containing -> (x1, y1) :: diff ((y2, x2) :: xs') ys'
    | Greater_overlapping -> diff ((y2, x2) :: xs') ys')
;;

let complement =
  let rec go lower_bound = function
    | [] -> if lower_bound < max_char then [ lower_bound, max_char ] else []
    | (x, y) :: ranges ->
      if lower_bound < pred x
      then (lower_bound, pred x) :: go (succ y) ranges
      else go (succ y) ranges
  in
  go min_char
;;

let empty = []
let singleton c = [ c, c ]
let any = [ min_char, max_char ]
let is_empty = function [] -> true | _ -> false

let is_any = function
  | [ (a, b) ] -> Char.(a = min_char) && Char.(b = max_char)
  | _ -> false
;;

let rec mem ranges c =
  match ranges with
  | [] -> false
  | (c1, c2) :: ranges -> if c <= c2 then c >= c1 else mem ranges c
;;
