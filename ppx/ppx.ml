open Ppxlib

let expand_parser ~loc ~path:_ expr =
  let module Ast =
    Ast_builder.Make (struct
      let loc = loc
    end)
  in
  let open Staged.Make (Ast) (Token_streams.Char) in
  let rec reflect : 'a Staged_signatures.code -> 'a Construction.t =
    let open Construction in
    function
    | [%expr bot] -> bot
    | { pexp_desc; _ } ->
      (match pexp_desc with
      | Pexp_apply ([%expr eps], [ (_label, arg) ]) -> eps arg
      | Pexp_apply ([%expr seq], [ (_label, p1); (_, p2) ]) ->
        (* XXX *) Stdlib.Obj.magic (seq (reflect p1) (reflect p2))
      | Pexp_apply ([%expr star], [ (_, p) ]) -> Stdlib.Obj.magic (star (reflect p))
      | Pexp_apply ([%expr tok], [ (_, t) ]) ->
        (match Token.reflect t with
        | None -> failwith "failed to reflect token"
        | Some t -> tok [ t ])
      | Pexp_apply ([%expr alt], [ (_, p1); (_, p2) ])
      | Pexp_apply ({ pexp_desc = Pexp_apply ([%expr alt], [ (_, p1) ]); _ }, [ (_, p2) ])
        ->
        alt ~failure_msg:"TODO" (reflect p1) (reflect p2)
      | _ -> failwith "TODO")
  in
  expr |> reflect |> typecheck |> compile
;;

let term_extension =
  Extension.declare
    "parser"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_parser
;;

let () =
  Driver.register_transformation
    "typed_algebraic_parsing"
    ~rules:[ Context_free.Rule.extension term_extension ]
;;
