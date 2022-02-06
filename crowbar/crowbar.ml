module type S = sig
  type t

  include Laws.S with type t := t

  val gen : t Crowbar.gen
end

let add_all (module M : S) =
  let open Crowbar in
  let gen = M.gen in
  M.Ring.(
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
  M.Lattice.(
    add_test ~name:"idempotent_union" [ gen ] (fun a -> check (idempotent_union a));
    add_test ~name:"idempotent_inter" [ gen ] (fun a -> check (idempotent_inter a));
    add_test ~name:"join_bot" [ gen ] (fun a -> check (join_bot a));
    add_test ~name:"meet_top" [ gen ] (fun a -> check (meet_top a));
    add_test ~name:"absorption_1" [ gen; gen ] (fun a b -> check (absorption_1 a b));
    add_test ~name:"absorption_2" [ gen; gen ] (fun a b -> check (absorption_2 a b));
    add_test ~name:"distribute_over_union" [ gen; gen; gen ] (fun a b c ->
        check (distribute_over_union a b c));
    add_test ~name:"distribute_over_inter" [ gen; gen; gen ] (fun a b c ->
        check (distribute_over_inter a b c)));
  add_test ~name:"double_negation" [ gen ] (fun a -> check (M.double_negation a))
;;

module Char_class' = struct
  open Char_class
  include Laws
  open Crowbar

  let gen =
    fix (fun gen ->
        choose
          [ const empty
          ; const any
          ; map [ uchar ] singleton
          ; map [ char ] Char.singleton
            (*
        ; map [ uchar; uchar ] (fun a b ->
              if Base.Int.(Uchar.compare a b <= 0)
              then Char_class.range a b
              else Char_class.range b a)
               *)
          ; map [ char; char ] (fun a b ->
                if Base.Int.(Base.Char.compare a b <= 0)
                then Char.range a b
                else Char.range b a)
          ; map [ list uchar ] of_list (* ; map [ bytes ] of_string *)
          ; map [ gen ] negate
          ; map [ gen; gen ] union
          ; map [ gen; gen ] inter
          ])
  ;;
end

module Regex' = struct
  open Regex
  include Laws
  open Crowbar

  let gen =
    choose
      [ const empty
      ; const any
      ; const eps
        (* ; map [ char ] chr *)
        (* ; map [ uchar ] Regex.uchar *)
      ]
  ;;
end

let () =
  (* add_all (module Char_class'); *)
  add_all (module Regex')
;;
