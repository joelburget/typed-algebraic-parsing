open Base

type t = (Char_class.t, Char_class.comparator_witness) Set.t

let pp ppf set = Fmt.(braces (list Char_class.pp ~sep:comma)) ppf (Set.to_list set)
let trivial = Set.singleton (module Char_class) Char_class.any

let cross s1 s2 =
  Set.fold
    s1
    ~init:(Set.empty (module Char_class))
    ~f:(fun accum s1_elem ->
      Set.fold s2 ~init:accum ~f:(fun accum s2_elem ->
          Set.add accum (Char_class.inter s1_elem s2_elem)))
;;

let%test_module _ =
  (module struct
    let go t = Fmt.pr "%a@." pp t
    let example_a = Set.singleton (module Char_class) (Char_class.of_char 'a')
    let ac = Char_class.range (Uchar.of_char 'a') (Uchar.of_char 'c')
    let ab = Char_class.range (Uchar.of_char 'a') (Uchar.of_char 'b')
    let de = Char_class.range (Uchar.of_char 'd') (Uchar.of_char 'e')
    let ef = Char_class.range (Uchar.of_char 'e') (Uchar.of_char 'f')
    let example_ac = Set.singleton (module Char_class) ac
    let example_ab = Set.singleton (module Char_class) ab
    let example_ac_ef = Set.of_list (module Char_class) [ ac; ef ]
    let example_ab_de = Set.of_list (module Char_class) [ ab; de ]

    let example_ac_ef_etc =
      Set.of_list (module Char_class) [ ac; ef; Char_class.(negate (union ac ef)) ]
    ;;

    let%expect_test _ =
      go trivial;
      go (cross trivial trivial);
      go (cross trivial example_a);
      go (cross trivial example_ac);
      go (cross example_a example_ac);
      go (cross example_ac example_ab);
      go (cross example_ac example_ac_ef);
      go (cross example_ac_ef example_ac_ef_etc);
      go (cross example_ab_de example_ac_ef_etc);
      [%expect {|
        {.}
        {.}
        {a}
        {[a-c]}
        {a}
        {[ab]}
        {[], [a-c]}
        {[], [a-c], [ef]}
        {[], [ab], d, e} |}]
    ;;
  end)
;;
