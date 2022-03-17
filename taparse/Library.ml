open Base

module Make (Construction : Signatures.Construction with type 'a v = 'a) = struct
  open Construction
  open Prelude.Grammar_provenance

  let provenance = Generated

  type assoc =
    | Left
    | Right

  let always x _ = x
  let ( ++ ) a b = seq ~provenance a b
  let ( ==> ) p f = map ~provenance f p

  let choice ~failure_msg gs =
    match gs with
    | [] -> bot
    | g :: gs -> List.fold_left ~f:(alt ~provenance ~failure_msg) ~init:g gs
  ;;

  let option r = choice ~failure_msg:"option failed" [ eps None; (r ==> fun x -> Some x) ]
  let plus g = g ++ star g ==> (fun (x, xs) -> x :: xs) <?> "plus"

  let sep_by1 sep p =
    p ++ star (sep ++ p ==> snd) ==> (fun (x, xs) -> x :: xs) <?> "sep_by1"
  ;;

  let sep_by sep p =
    option (sep_by1 sep p) ==> (function None -> [] | Some xs -> xs) <?> "sep_by"
  ;;

  let ( <* ) p1 p2 = p1 ++ p2 ==> fst <?> "<*"
  let ( *> ) p1 p2 = p1 ++ p2 ==> snd <?> "*>"
  let ( <|> ) a b = alt a b <?> "<|>"
  let ( >>| ) p f = p ==> f <?> ">>|"
  let ( <$> ) f p = p ==> f <?> "<$>"
  let ( <*> ) fp p = fp ++ p ==> (fun (f, a) -> f a) <?> "<*>"

  module Let_syntax = struct
    let return a = eps a <?> "return"
    let both a b = a ++ b <?> "both"
    let map p ~f = p ==> f
    let map2 a b ~f = a ++ b ==> (fun (a, b) -> f a b) <?> "map2"
    let map3 a b c ~f = a ++ (b ++ c) ==> (fun (a, (b, c)) -> f a b c) <?> "map3"

    let map4 a b c d ~f =
      a ++ (b ++ (c ++ d)) ==> (fun (a, (b, (c, d))) -> f a b c d) <?> "map4"
    ;;
  end

  let ( let+ ) p f = p ==> f <?> "let+"
  let ( and+ ) a b = a ++ b <?> "and+"

  let infixr op base =
    let process (accum, rhs) =
      match rhs with None -> accum | Some (f, e') -> f accum e'
    in
    fix (fun g -> base ++ option (op ++ g) ==> process) <?> "infixr"
  ;;

  let infixl op base =
    let reassociate (init, oes) =
      List.fold_left ~f:(fun e (op, e') -> op e e') ~init oes
    in
    base ++ star (op ++ base) ==> reassociate <?> "infixl"
  ;;

  let infix ops base =
    let make_level base (fixity, op) =
      match fixity with Left -> infixl op base | Right -> infixr op base
    in
    List.fold_left ~f:make_level ~init:base ops <?> "infix"
  ;;

  (* Redefine combinators without provenance *)
  let ( ++ ) a b = seq ~provenance a b <?> "++"
  let ( ==> ) p f = map ~provenance f p <?> "==>"

  let option r =
    choice ~failure_msg:"option failed" [ eps None; (r ==> fun x -> Some x) ] <?> "option"
  ;;

  let choice ~failure_msg gs =
    (match gs with
    | [] -> bot
    | g :: gs -> List.fold_left ~f:(alt ~failure_msg) ~init:g gs)
    <?> "choice"
  ;;
end
