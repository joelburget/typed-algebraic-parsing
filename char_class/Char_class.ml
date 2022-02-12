module Interval_set = struct
  module Elt = struct
    type t = Uchar.t

    let compare = Uchar.compare

    let pred c =
      let open Uchar in
      if equal c min then min else pred c
    ;;

    let succ c =
      let open Uchar in
      if equal c max then min else succ c
    ;;

    let zero = Uchar.min

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

type interval = Interval_set.interval

module Interval = Interval_set.Interval

module T = struct
  type t =
    | Pos of Interval_set.t
    | Neg of Interval_set.t
  [@@deriving compare, equal, sexp]
end

include T
module C = Base.Comparable.Make (T)
include C

type element = Uchar.t

let empty = Pos Interval_set.empty
let any = Neg Interval_set.empty
let singleton_interval x = Interval_set.Interval.make x x
let singleton c = Pos (Interval_set.of_interval (singleton_interval c))
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

let asymmetric_diff a b =
  match a, b with
  | Pos xs, Pos ys -> Pos (Interval_set.diff xs ys)
  | Neg xs, Neg ys -> Pos (Interval_set.diff ys xs)
  | Pos xs, Neg ys -> Pos (Interval_set.inter xs ys)
  | Neg xs, Pos ys -> Neg (Interval_set.union xs ys)
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
let is_subset a b = inter a b = b

let choose = function
  | Pos iset ->
    (try Some (iset |> Interval_set.choose |> Interval_set.Interval.x) with _ -> None)
  | Neg iset -> Some (Interval_set.find_next_gap Uchar.min iset)
;;

let meta_chars = Base.String.to_list "\\|&!*.[]()\""
let is_meta_char = Base.List.mem ~equal:Base.Char.( = ) meta_chars

let pp_char ppf c =
  if Uchar.is_char c
  then (
    let c = Uchar.to_char c in
    if Base.Char.(c = '\n' || c = '\t' || c = '\'')
    then Fmt.pf ppf "%s" (Base.Char.escaped c)
    else if is_meta_char c
    then Fmt.pf ppf "\\%c" c
    else if Base.Char.is_print c
    then Fmt.pf ppf "%c" c
    else Fmt.pf ppf "\\u%04X" (Base.Char.to_int c))
  else Fmt.pf ppf "\\u%04X" (Uchar.to_int c)
;;

let%test_module "pp_char" =
  (module struct
    let ugo uchar = Fmt.pr "%a@." pp_char uchar
    let go char = ugo (Uchar.of_char char)

    let%expect_test _ =
      go 'c';
      go ' ';
      go '\\';
      go '|';
      go '\n';
      go '\t';
      go '"';
      go '\'';
      ugo (Uchar.of_int 949);
      [%expect
        {|
        c

        \\
        \|
        \n
        \t
        \"
        \'
        \u03B5 |}]
    ;;
  end)
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
        if Uchar.equal x y
        then pp_char ppf x
        else if Uchar.(equal x (pred y))
        then Fmt.pf ppf "%a%a" pp_char x pp_char y
        else Fmt.pf ppf "%a-%a" pp_char x pp_char y)
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

let pos_intervals t = Interval_set.fold List.cons t []

let intervals = function
  | Pos iset -> pos_intervals iset
  | Neg iset ->
    pos_intervals
      Interval_set.(diff (of_interval (Interval.make Uchar.min Uchar.max)) iset)
;;

module Infix = struct
  include C

  let ( + ) = union
  let ( * ) = inter
  let ( - ) = asymmetric_diff
end

module Laws = Laws.Make (struct
  include T
  module Infix = Infix

  let pp = pp
  let additive_ident = empty
  let multiplicative_ident = any
  let bot = empty
  let top = any
  let negate = negate
end)

module Char = struct
  type element = char

  let singleton c = singleton (Uchar.of_char c)

  let range x y =
    Pos
      (Interval_set.of_interval
         (Interval_set.Interval.make (Uchar.of_char x) (Uchar.of_char y)))
  ;;

  let mem t c = mem t (Uchar.of_char c)
  let of_list xs = of_list (Base.List.map ~f:Uchar.of_char xs)
end

let%test_module _ =
  (module struct
    let c = Char.singleton 'c'
    let d = Char.singleton 'd'
    let cd = of_list Uchar.[ of_char 'c'; of_char 'd' ]
    let xy = of_list Uchar.[ of_char 'x'; of_char 'y' ]

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
      go (Char.singleton '(');
      [%expect
        {|
        []
        .
        c
        [^c]
        \u04D2
        [^\u04D2]
        [cd]
        [^cd]
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
        [cd]
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
        [^cd] |}]
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
        \u0000
        \u0001 |}]
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

    let%expect_test "is_subset" =
      let go a b = is_subset a b |> Fmt.pr "%b@." in
      go c empty;
      go c d;
      go cd d;
      go c (negate d);
      go (negate c) d;
      go (negate c) (negate d);
      go (negate c) (negate cd);
      [%expect
        {|
        true
        false
        true
        false
        true
        false
        true |}]
    ;;

    let%expect_test "( - )" =
      let go a b = asymmetric_diff a b |> Fmt.pr "%a@." pp in
      go c d;
      go c cd;
      go cd d;
      go c (negate d);
      go cd (negate d);
      go c (negate cd);
      go (negate c) d;
      go (negate c) cd;
      go (negate cd) d;
      go (negate c) (negate d);
      go (negate c) (negate cd);
      go (negate cd) (negate c);
      [%expect
        {|
        c
        []
        c
        []
        d
        c
        [^cd]
        [^cd]
        [^cd]
        d
        d
        [] |}]
    ;;

    let%expect_test "intervals" =
      let pp_interval ppf ival =
        let x, y = Interval.(x ival, y ival) in
        Fmt.pf ppf "%a, %a" pp_char x pp_char y
      in
      let pp = Fmt.(brackets (list ~sep:semi pp_interval)) in
      let go set = set |> intervals |> Fmt.pr "%a@." pp in
      go empty;
      go any;
      go cd;
      go (negate cd);
      go (union cd xy);
      go (negate (union cd xy));
      [%expect
        {|
        []
        [\u0000, \u10FFFF]
        [c, d]
        [e, \u10FFFF; \u0000, b]
        [x, y; c, d]
        [z, \u10FFFF; e, w; \u0000, b] |}]
    ;;
  end)
;;
