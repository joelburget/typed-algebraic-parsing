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

  let mk1 a lhs rhs =
    let result = Infix.(lhs = rhs) in
    if not result
    then (
      pr "a = %a@." pp a;
      pr "%a <> %a@." pp lhs pp rhs);
    result
  ;;

  let mk2 a b lhs rhs =
    let result = Infix.(lhs = rhs) in
    if not result
    then (
      pr "a = %a@." pp a;
      pr "b = %a@." pp b;
      pr "%a <> %a@." pp lhs pp rhs);
    result
  ;;

  let mk3 a b c lhs rhs =
    let result = Infix.(lhs = rhs) in
    if not result
    then (
      pr "a = %a@." pp a;
      pr "b = %a@." pp b;
      pr "c = %a@." pp c;
      pr "%a <> %a@." pp lhs pp rhs);
    result
  ;;
end
