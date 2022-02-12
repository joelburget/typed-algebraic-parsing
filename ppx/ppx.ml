(* open Base *)

let expand_parser ~loc ~path:_ expr =
  let module Ast =
    Ppxlib.Ast_builder.Make (struct
      let loc = loc
    end)
  in
  let module Ir = Staged_ir.Make (Ast) in
  let module Codegen = Ir.Codegen in
  let module Parser = Staged.Make (Token_streams.Uchar) in
  let open Parser in
  let module Compile = Compile (Ast) in
  let module Expander = struct
    open Ir

    (*
    let bot _ = return [%expr failwith "Impossible"]
    let eps = return
    *)

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
   ;;
  end
  in
  expr |> Expander.reflect |> typecheck |> Compile.compile
;;

open Ppxlib

let term_extension =
  Extension.declare
    "parser"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_parser
;;

let () =
  Ppxlib.Driver.register_transformation
    "typed_algebraic_parsing"
    ~rules:[ Context_free.Rule.extension term_extension ]
;;
