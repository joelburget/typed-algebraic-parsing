let pr = Fmt.pr

module Make (T : sig
  type t

  val pp : t Fmt.t

  module Infix : sig
    val ( = ) : t -> t -> bool
  end
end) =
struct
  open T

  let op1 op a lhs rhs =
    let result = op lhs rhs in
    if not result
    then (
      pr "a = %a@." pp a;
      pr "%a <> %a@." pp lhs pp rhs);
    result
  ;;

  let op2 op a b lhs rhs =
    let result = op lhs rhs in
    if not result
    then (
      pr "a = %a@." pp a;
      pr "b = %a@." pp b;
      pr "%a <> %a@." pp lhs pp rhs);
    result
  ;;

  let op3 op a b c lhs rhs =
    let result = op lhs rhs in
    if not result
    then (
      pr "a = %a@." pp a;
      pr "b = %a@." pp b;
      pr "c = %a@." pp c;
      pr "%a <> %a@." pp lhs pp rhs);
    result
  ;;

  let equal1 = op1 Infix.( = )
  let equal2 = op2 Infix.( = )
  let equal3 = op3 Infix.( = )
end
