open Base

module Make (Construction : Signatures.Construction with type 'a v = 'a) = struct
  open Construction

  type assoc =
    | Left
    | Right

  let always x _ = x
  let ( ++ ) = seq
  let ( ==> ) p f = map f p
  let choice ~failure_msg gs = List.fold_left ~f:(alt ~failure_msg) ~init:bot gs
  let option r = choice ~failure_msg:"option failed" [ eps None; (r ==> fun x -> Some x) ]
  let plus g = g ++ star g ==> fun (x, xs) -> x :: xs
  let sep_by1 sep p = p ++ star (sep ++ p ==> snd) ==> fun (x, xs) -> x :: xs
  let sep_by sep p = option (sep_by1 sep p) ==> function None -> [] | Some xs -> xs
  let ( <* ) p1 p2 = p1 ++ p2 ==> fst
  let ( *> ) p1 p2 = p1 ++ p2 ==> snd
  let ( <|> ) a b = alt a b
  let ( >>| ) p f = map f p
  let ( <$> ) f p = map f p
  let ( <*> ) fp p = fp ++ p ==> fun (f, a) -> f a

  module Let_syntax = struct
    let return a = eps a
    let map p ~f = map f p
    let both a b = seq a b
    let map2 a b ~f = map ~f:(fun (a, b) -> f a b) (seq a b)
    let map3 a b c ~f = map ~f:(fun (a, (b, c)) -> f a b c) (seq a (seq b c))

    let map4 a b c d ~f =
      map ~f:(fun (a, (b, (c, d))) -> f a b c d) (seq a (seq b (seq c d)))
    ;;
  end

  let ( let+ ) p f = map f p
  let ( and+ ) a b = seq a b

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
