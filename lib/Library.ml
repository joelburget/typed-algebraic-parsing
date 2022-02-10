open Base

module Make (Construction : Signatures.Construction) = struct
  open Construction

  type assoc =
    | Left
    | Right

  let always x _ = x
  let ( ++ ) = seq
  let ( ==> ) p f = map f p
  let choice gs = List.fold_left ~f:alt ~init:bot gs
  let option r = choice [ eps None; (r ==> fun x -> Some x) ]
  let plus g = g ++ star g ==> fun (x, xs) -> x :: xs
  let sep_by1 sep p = p ++ star (sep ++ p ==> snd) ==> fun (x, xs) -> x :: xs
  let sep_by sep p = option (sep_by1 sep p) ==> function None -> [] | Some xs -> xs
  let ( <* ) p1 p2 = p1 ++ p2 ==> fst
  let ( *> ) p1 p2 = p1 ++ p2 ==> snd

  let infixr op base =
    let process (accum, rhs) =
      match rhs with None -> accum | Some (f, e') -> f accum e'
    in
    fix (fun g -> base ++ option (op ++ g) ==> process)
  ;;

  let infixl op base =
    let reassociate (init, oes) =
      List.fold_left ~f:(fun e (op, e') -> op e e') ~init oes
    in
    base ++ star (op ++ base) ==> reassociate
  ;;

  let infix ops base =
    let make_level base (fixity, op) =
      match fixity with Left -> infixl op base | Right -> infixr op base
    in
    List.fold_left ~f:make_level ~init:base ops
  ;;
end
