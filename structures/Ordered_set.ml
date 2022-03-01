open Ordered_set_sigs

let ( => ) a b = if a then b else true

module Preorder_laws (T : Preorder) : Preorder_laws with type t = T.t = struct
  open Util.Make (T)
  include T
  open Infix

  let reflexivity a = op1 ( <= ) a a a

  let transitivity a b c =
    let lhs = a <= b && b <= c in
    let rhs = a <= c in
    let result = lhs => rhs in
    if not result
    then (
      Fmt.pr "a = %a@." pp a;
      Fmt.pr "b = %a@." pp b;
      Fmt.pr "c = %a@." pp c;
      Fmt.pr "%b =/> %b@." lhs rhs);
    result
  ;;
end

module Partial_order_laws (T : Partial_order) : Partial_order_laws with type t = T.t =
struct
  open Util.Make (T)
  include Preorder_laws (T)
  open T
  open Infix

  let antisymmetry a b =
    let lhs = a <= b && b <= a in
    let rhs = a = b in
    let result = lhs => rhs in
    if not result
    then (
      Fmt.pr "a = %a@." pp a;
      Fmt.pr "b = %a@." pp b;
      Fmt.pr "%b =/> %b@." lhs rhs);
    result
  ;;
end

module Total_order_laws (T : Total_order) : Total_order_laws with type t = T.t = struct
  open Util.Make (T)
  include Partial_order_laws (T)
  open T
  open Infix

  let totality a b =
    let result = a <= b || b <= a in
    if not result
    then (
      Fmt.pr "a = %a@." pp a;
      Fmt.pr "b = %a@." pp b);
    result
  ;;
end
