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

module Grammar_provenance = struct
  type t =
    | Generated
    | User_defined
end
