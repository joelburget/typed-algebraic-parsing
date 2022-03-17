exception Parse_error of string

let parse_error fmt = Stdlib.Format.kasprintf (fun str -> raise (Parse_error str)) fmt

module Var = struct
  type ('ctx, 'a) t =
    | Z : ('a * 'ctx, 'a) t
    | S : ('rest, 'a) t -> ('b * 'rest, 'a) t

  let rec count : type ctx a. (ctx, a) t -> int = function
    | Z -> 0
    | S rest -> count rest + 1
  ;;

  let pp ppf t = Fmt.pf ppf "%d" (count t)
end

module Env (T : sig
  type 'a t
end) =
struct
  type 'a elem_t = 'a T.t

  type 'ctx t =
    | [] : unit t
    | ( :: ) : 'a T.t * 'ctx t -> ('a * 'ctx) t

  let rec lookup : type ctx a. ctx t -> (ctx, a) Var.t -> a T.t =
   fun ctx v -> match ctx, v with x :: _, Z -> x | _ :: xs, S v -> lookup xs v | _ -> .
 ;;

  type fn = { f : 'a. 'a T.t -> 'a T.t }

  let rec map : type ctx. fn -> ctx t -> ctx t =
   fun { f } -> function [] -> [] | x :: xs -> f x :: map { f } xs
 ;;
end

exception Type_error of unit Fmt.t

let type_assert b msg = if not b then raise (Type_error msg)

module Tree : sig
  type t =
    { label : string
    ; children : t list
    }

  val mk : string -> t list -> t
  val pp : t Fmt.t
end = struct
  type t =
    { label : string
    ; children : t list
    }

  let rec pp ppf { label; children } =
    match children with
    | [] -> Fmt.string ppf label
    | _ -> Fmt.(vbox ~indent:2 (pair string (list pp))) ppf (label, children)
  ;;

  let mk label children = { label; children }
  let go = Fmt.pr "%a@." pp

  let%expect_test _ =
    go (mk "root" []);
    [%expect {| root |}]
  ;;

  let%expect_test _ =
    let t =
      mk
        "root"
        [ mk "a" [ mk "c" []; mk "d" []; mk "e" [] ]; mk "b" [ mk "f" []; mk "g" [] ] ]
    in
    go t;
    [%expect
      {|
      root
        a
          c
          d
          e
        b
          f
          g |}]
  ;;
end
