let gen =
  let open Char_class in
  let open Crowbar in
  fix (fun gen ->
      choose
        [ const empty
        ; const any
        ; map [ uchar ] singleton
        ; map [ char ] of_char
          (*
        ; map [ uchar; uchar ] (fun a b ->
              if Base.Int.(Uchar.compare a b <= 0)
              then Char_class.range a b
              else Char_class.range b a)
               *)
        ; map [ char; char ] (fun a b ->
              if Base.Int.(Char.compare a b <= 0) then crange a b else crange b a)
        ; map [ list uchar ] of_list (* ; map [ bytes ] of_string *)
        ; map [ gen ] negate
        ; map [ gen; gen ] union
        ; map [ gen; gen ] inter
        ])
;;

let () =
  let open Crowbar in
  let open Char_class.Laws in
  Ring.(
    add_test ~name:"plus_associative" [ gen; gen; gen ] (fun a b c ->
        check (plus_associative a b c));
    add_test ~name:"plus_commutative" [ gen; gen ] (fun a b ->
        check (plus_commutative a b));
    add_test ~name:"plus_ident" [ gen ] (fun a -> check (plus_ident a));
    add_test ~name:"mul_inverse" [ gen ] (fun a -> check (mul_inverse a));
    add_test ~name:"mul_associative" [ gen; gen; gen ] (fun a b c ->
        check (mul_associative a b c));
    add_test ~name:"mul_commutative" [ gen; gen ] (fun a b -> check (mul_commutative a b));
    add_test ~name:"mul_ident" [ gen ] (fun a -> check (mul_ident a));
    add_test ~name:"left_distributive" [ gen; gen; gen ] (fun a b c ->
        check (left_distributive a b c));
    add_test ~name:"right_distributive" [ gen; gen; gen ] (fun a b c ->
        check (right_distributive a b c)));
  Lattice.(
    add_test ~name:"idempotent_union" [ gen ] (fun a -> check (idempotent_union a));
    add_test ~name:"idempotent_inter" [ gen ] (fun a -> check (idempotent_inter a));
    add_test ~name:"absorption_1" [ gen; gen ] (fun a b -> check (absorption_1 a b));
    add_test ~name:"absorption_2" [ gen; gen ] (fun a b -> check (absorption_2 a b));
    add_test ~name:"distribute_over_union" [ gen; gen; gen ] (fun a b c ->
        check (distribute_over_union a b c));
    add_test ~name:"distribute_over_inter" [ gen; gen; gen ] (fun a b c ->
        check (distribute_over_inter a b c)));
  add_test ~name:"double_negation" [ gen ] (fun a -> check (double_negation a))
;;
