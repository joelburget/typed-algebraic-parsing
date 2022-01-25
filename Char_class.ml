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
end

type t =
  | Pos of Interval_set.t
  | Neg of Interval_set.t

let pp_char ppf c =
  if Uchar.is_char c
  then (
    let c = Uchar.to_char c in
    if Base.Char.is_alphanum c
    then Fmt.pf ppf "%c" c
    else Fmt.pf ppf "%s" (Base.Char.escaped c))
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

let%test_module "pp" =
  (module struct
    let go char_class = Fmt.pr "%a@." pp char_class
    let singleton_interval x = Interval_set.Interval.make x x

    let of_list xs =
      xs
      |> Base.List.map ~f:singleton_interval
      |> Base.List.fold_right ~init:Interval_set.empty ~f:Interval_set.add
    ;;

    let singleton x = Interval_set.(add (singleton_interval x) empty)

    let%expect_test _ =
      go (Pos Interval_set.empty);
      go (Neg Interval_set.empty);
      go (Pos (singleton (Uchar.of_char 'c')));
      go (Neg (singleton (Uchar.of_char 'c')));
      go (Pos (singleton (Uchar.of_int 1234)));
      go (Neg (singleton (Uchar.of_int 1234)));
      go (Pos (of_list Uchar.[ of_char 'c'; of_char 'd' ]));
      go (Neg (of_list Uchar.[ of_char 'c'; of_char 'd' ]));
      [%expect{|
        []
        .
        c
        [^c]
        \u1234
        [^\u1234]
        [c-d]
        [^c-d] |}]
    ;;
  end)
;;
