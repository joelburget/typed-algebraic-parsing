open Ppxlib

let expand_parser ~loc ~path:_ expr =
  let module Ast =
    Ast_builder.Make (struct
      let loc = loc
    end)
  in
  let open Staged.Make (Ast) (Token_streams.Uchar) in
  let reflect : 'a Staged_signatures.code -> 'a Construction.t =
   fun { pexp_desc; _ } ->
    let open Construction in
    match pexp_desc with
    | Pexp_ident { txt; loc = _ } ->
      (match txt with
      | Lident "bot" -> bot
      (* | Lident "eps" -> eps *)
      | _ -> failwith "TODO")
    | Pexp_apply _ -> failwith "TODO"
    | _ -> failwith "TODO"
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
