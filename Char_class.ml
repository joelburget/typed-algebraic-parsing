module Interval_set = struct
  module Elt = struct
    include Uchar

    let zero = min

    let sub a b =
      let a, b = Uchar.(to_int a, to_int b) in
      Uchar.of_int (a - b)
    ;;

    let add a b =
      let a, b = Uchar.(to_int a, to_int b) in
      Uchar.of_int (a + b)
    ;;

    let to_string c =
      if Uchar.is_char c
      then Fmt.str "%C" (Uchar.to_char c)
      else Fmt.str "\\u%d" (Uchar.to_int c)
    ;;
  end

  include Diet.Make (Elt)
  open Base

  let interval_sexp_of_t interval =
    let atom c = Sexp.Atom (Elt.to_string c) in
    let x, y = Interval.(x interval, y interval) in
    Sexp.List [ atom x; atom y ]
  ;;

  let sexp_of_t iset =
    Sexp.List (fold (fun ival -> ival |> interval_sexp_of_t |> List.cons) iset [])
  ;;

  exception Unexpected_sexp

  let of_string str = str |> Char.of_string |> Uchar.of_char

  let interval_of_sexp = function
    | Sexp.List [ Atom x; Atom y ] -> Interval.make (of_string x) (of_string y)
    | sexp -> raise (Sexp.Of_sexp_error (Unexpected_sexp, sexp))
  ;;

  let of_interval interval = add interval empty

  let t_of_sexp = function
    | Sexp.List intervals ->
      intervals
      |> List.map ~f:(fun sexp -> sexp |> interval_of_sexp |> of_interval)
      |> List.fold ~init:empty ~f:union
    | sexp -> raise (Sexp.Of_sexp_error (Unexpected_sexp, sexp))
  ;;
end

module T = struct
  type t =
    | Pos of Interval_set.t
    | Neg of Interval_set.t
  [@@deriving compare, equal, sexp]
end

include T
include Base.Comparable.Make (T)

let empty = Pos Interval_set.empty
let any = Neg Interval_set.empty
let singleton_interval x = Interval_set.Interval.make x x
let singleton c = Pos (Interval_set.of_interval (singleton_interval c))
let of_char c = singleton (Uchar.of_char c)
let range x y = Pos (Interval_set.of_interval (Interval_set.Interval.make x y))

let of_list xs =
  Pos
    (xs
    |> Base.List.map ~f:singleton_interval
    |> Base.List.fold_right ~init:Interval_set.empty ~f:Interval_set.add)
;;

let of_string str =
  str |> Base.String.to_list |> Base.List.map ~f:Uchar.of_char |> of_list
;;

let negate = function Pos iset -> Neg iset | Neg iset -> Pos iset

let union a b =
  match a, b with
  | Pos xs, Pos ys -> Pos (Interval_set.union xs ys)
  | Pos xs, Neg ys | Neg ys, Pos xs -> Neg (Interval_set.diff ys xs)
  | Neg xs, Neg ys -> Neg (Interval_set.inter xs ys)
;;

let inter a b =
  match a, b with
  | Pos xs, Pos ys -> Pos (Interval_set.inter xs ys)
  | Pos xs, Neg ys | Neg ys, Pos xs -> Pos (Interval_set.diff xs ys)
  | Neg xs, Neg ys -> Neg (Interval_set.union xs ys)
;;

let mem t c =
  match t with
  | Pos iset -> Interval_set.mem c iset
  | Neg iset -> not (Interval_set.mem c iset)
;;

let is_empty = function Pos iset when Interval_set.is_empty iset -> true | _ -> false

let choose = function
  | Pos iset ->
    (try Some (iset |> Interval_set.choose |> Interval_set.Interval.x) with _ -> None)
  | Neg iset -> Some (Interval_set.find_next_gap Uchar.min iset)
;;

let meta_chars = Base.String.to_list "\\|&!*.[]()"
let is_meta_char = Base.List.mem ~equal:Base.Char.( = ) meta_chars

let pp_char ppf c =
  if Uchar.is_char c
  then (
    let c = Uchar.to_char c in
    if Base.Char.is_alphanum c
    then Fmt.pf ppf "%c" c
    else if is_meta_char c
    then Fmt.pf ppf "\\%c" c
    else Fmt.pf ppf "\\u%d" (Base.Char.to_int c))
  else Fmt.pf ppf "\\u%d" (Uchar.to_int c)
;;

let pp ppf iset =
  let one = Uchar.(succ min) in
  let pp_single ppf iset =
    iset |> Interval_set.choose |> Interval_set.Interval.x |> pp_char ppf
  in
  let pp_ranges ppf ranges =
    Interval_set.iter
      (fun interval ->
        let x, y = Interval_set.Interval.(x interval, y interval) in
        if Uchar.equal x y then pp_char ppf x else Fmt.pf ppf "%a-%a" pp_char x pp_char y)
      ranges
  in
  match iset with
  | Pos iset when Interval_set.is_empty iset -> Fmt.pf ppf "[]"
  | Neg iset when Interval_set.is_empty iset -> Fmt.pf ppf "."
  | Pos iset when Uchar.equal (Interval_set.cardinal iset) one -> pp_single ppf iset
  | Neg iset when Uchar.equal (Interval_set.cardinal iset) one ->
    Fmt.pf ppf "[^%a]" pp_single iset
  | Pos iset -> Fmt.pf ppf "[%a]" pp_ranges iset
  | Neg iset -> Fmt.pf ppf "[^%a]" pp_ranges iset
;;

let choose_exn t =
  match choose t with
  | Some char -> char
  | None -> Fmt.failwith "failed to choose from %a" pp t
;;

let%test_module "pp" =
  (module struct
    let c = of_char 'c'
    let d = of_char 'd'
    let cd = of_list Uchar.[ of_char 'c'; of_char 'd' ]

    let%expect_test "pp" =
      let go char_class = Fmt.pr "%a@." pp char_class in
      let singleton x = Interval_set.(add (singleton_interval x) empty) in
      go (Pos Interval_set.empty);
      go (Neg Interval_set.empty);
      go c;
      go (negate c);
      go (Pos (singleton (Uchar.of_int 1234)));
      go (Neg (singleton (Uchar.of_int 1234)));
      go cd;
      go (negate cd);
      go (of_char '(');
      [%expect
        {|
        []
        .
        c
        [^c]
        \u1234
        [^\u1234]
        [c-d]
        [^c-d]
        \( |}]
    ;;

    let%expect_test "union" =
      let go a b = union a b |> Fmt.pr "%a@." pp in
      go c empty;
      go c d;
      go c (negate d);
      go (negate c) d;
      go (negate c) (negate d);
      [%expect {|
        c
        [c-d]
        [^d]
        [^c]
        . |}]
    ;;

    let%expect_test "inter" =
      let go a b = inter a b |> Fmt.pr "%a@." pp in
      go c empty;
      go c d;
      go c (negate d);
      go (negate c) d;
      go (negate c) (negate d);
      [%expect {|
        []
        []
        c
        d
        [^c-d] |}]
    ;;

    let%expect_test "choose" =
      let go a = choose a |> Fmt.pr "%a@." Fmt.(option pp_char ~none:(any "None")) in
      go empty;
      go c;
      go (negate c);
      go (negate (singleton Uchar.min));
      [%expect {|
        None
        c
        \u0
        \u1 |}]
    ;;

    let%expect_test "mem" =
      let go cls char = mem cls char |> Fmt.pr "%b@." in
      let char = Uchar.of_char 'c' in
      go empty char;
      go c char;
      go any char;
      [%expect {|
        false
        true
        true |}]
    ;;
  end)
;;
