open Base
open Regex

module Core = struct
  module T = struct
    type t = Regex.t list [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let delta c = List.map ~f:(delta c)
  let string_delta str = List.map ~f:(string_delta str)
  let pp = Fmt.(brackets (list pp ~sep:semi))

  let class' res =
    res |> List.map ~f:Regex.class' |> Derivative_class.(List.fold ~init:trivial ~f:cross)
  ;;

  let nullable res = List.exists res ~f:nullable
end

include Core
module Dfa = Dfa.Make (Core)

let%test_module "Dfa.make" =
  (module struct
    let lexer =
      [ plus (char_class (Char_class.range (Uchar.of_char 'a') (Uchar.of_char 'z')))
      ; chr ' ' || chr '\n'
      ; chr '('
      ; chr ')'
      ]
    ;;

    open Dfa

    let go vec = vec |> make |> Fmt.pr "%a@." pp

    let%expect_test "fusing lexing and parsing figure 3" =
      go lexer;
      [%expect
        {|
          {state_numbers:
            [(2, [[]; []; []; []]); (4, [[]; []; []; []*]); (3, [[]; []; []*; []]);
             (1, [[]; []*; []; []]); (0, [[a-z][a-z]*; [\u10\u32]; \(; \)]);
             (5, [[a-z]*; []; []; []])];
           accepting: [4; 3; 1; 5];
           transitions:
            [((0, 1), [\u10\u32]); ((0, 2), [^\u10\u32\(-\)a-z]); ((0, 3), \();
             ((0, 4), \)); ((0, 5), [a-z]); ((1, 2), .); ((2, 2), .); ((3, 2), .);
             ((4, 2), .); ((5, 2), [^a-z]); ((5, 5), [a-z])]} |}]
    ;;
  end)
;;

let derivatives res =
  res
  |> List.map ~f:Regex.class'
  |> Derivative_class.(List.fold ~init:trivial ~f:cross)
  |> Set.to_list
  |> List.filter_map ~f:(fun set ->
         if Char_class.is_empty set
         then None
         else (
           let rep = Char_class.choose_exn set in
           Some (delta rep res, set)))
;;

let%test_module _ =
  (module struct
    let go res =
      let open Fmt in
      let pp_derivative =
        braces
          (record ~sep:semi [ field "res" fst pp; field "char_class" snd Char_class.pp ])
      in
      pr
        "@[<hv 2>%a ->@ %a@]@."
        pp
        res
        (hvbox (list pp_derivative ~sep:sp))
        (derivatives res)
    ;;

    let%expect_test "derivatives []" =
      go [];
      [%expect {|
          [] -> {res: [];
                 char_class: .} |}]
    ;;

    let%expect_test "derivatives ['c']" =
      go [ chr 'c' ];
      [%expect
        {|
          [c] -> {res: [[]*];
                  char_class: c} {res: [[]];
                                  char_class: [^c]} |}]
    ;;

    let%expect_test "derivatives ['c'; 'd']" =
      go [ chr 'c'; chr 'd' ];
      [%expect
        {|
          [c; d] ->
            {res: [[]*; []];
             char_class: c}
            {res: [[]; []*];
             char_class: d}
            {res: [[]; []];
             char_class: [^c-d]} |}]
    ;;

    let%expect_test "derivatives ['c'; []]" =
      go [ chr 'c'; empty ];
      [%expect
        {|
          [c; []] -> {res: [[]*; []];
                      char_class: c} {res: [[]; []];
                                      char_class: [^c]} |}]
    ;;

    (* Regex derivatives reexamined fig 4b. *)
    let%expect_test {|derivatives ["ab"; "bc"]|} =
      go [ str "ab" || str "bc" ];
      [%expect
        {|
          [ab|bc] ->
            {res: [b];
             char_class: a}
            {res: [c];
             char_class: b}
            {res: [[]];
             char_class: [^a-b]} |}]
    ;;

    let%expect_test {|derivatives ["ab"; "bc"]|} =
      go
        [ plus (char_class (Char_class.range (Uchar.of_char 'a') (Uchar.of_char 'z')))
        ; chr ' ' || chr '\n'
        ; chr '('
        ; chr ')'
        ];
      [%expect
        {|
          [[a-z][a-z]*; [\u10\u32]; \(; \)] ->
            {res: [[]; []*; []; []];
             char_class: [\u10\u32]}
            {res: [[]; []; []*; []];
             char_class: \(}
            {res: [[]; []; []; []*];
             char_class: \)}
            {res: [[a-z]*; []; []; []];
             char_class: [a-z]}
            {res: [[]; []; []; []];
             char_class: [^\u10\u32\(-\)a-z]} |}]
    ;;
  end)
;;
