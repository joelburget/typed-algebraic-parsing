open Base
module Format = Stdlib.Format

type t =
  { label : string
  ; children : t list
  }

let snoc x xs = List.append xs [ x ]

type line_printing_command =
  { spanning_levels : int list
  ; indents : int
  ; content : string
  }

module Flat_pp_command = struct
  type flat_pp_command =
    | Open_box
    | Close_box
    | Line_printing_command of line_printing_command

  let pp ppf = function
    | Open_box -> Fmt.pf ppf "Open_box"
    | Close_box -> Fmt.pf ppf "Close_box"
    | Line_printing_command { spanning_levels; indents; content } ->
      Fmt.(
        pf
          ppf
          "Line_printing_command @[{ spanning_levels = %a;@ indents = %i;@ content = %S \
           }@]"
          (brackets (list ~sep:semi int))
          spanning_levels
          indents
          content)
  ;;

  let pp_line spanning_levels indents ppf content =
    let (_ : int) =
      spanning_levels
      |> List.map ~f:(fun level -> level, "\u{2502} " (* | char *))
      |> snoc (indents, content)
      |> List.fold ~init:(-1) ~f:(fun prev_level (level, content) ->
             let levels = level - prev_level in
             let spaces = (levels * 2) - 2 in
             Fmt.pf ppf "%s%s" (String.make spaces ' ') content;
             level)
    in
    Format.pp_print_cut ppf ()
  ;;

  let pp_line_command ppf { spanning_levels; indents; content } =
    pp_line spanning_levels indents ppf content
  ;;

  let run ppf = function
    | Open_box -> Format.pp_open_vbox ppf 0
    | Close_box -> Format.close_box ()
    | Line_printing_command cmd -> pp_line_command ppf cmd
  ;;

  let%expect_test "pp_line" =
    let go levels indents content = Fmt.pr "%a" (pp_line levels indents) content in
    Format.open_vbox 0;
    go [] 0 "a";
    go [] 1 "a";
    go [] 2 "a";
    go [ 0 ] 1 "a";
    go [ 0 ] 2 "a";
    go [ 1 ] 2 "a";
    go [ 0; 1 ] 2 "a";
    Format.close_box ();
    [%expect
      {|
      a
        a
          a
      │ a
      │   a
        │ a
      │ │ a
          |}]
  ;;

  let%expect_test "pp_line" =
    let go levels indents content = Fmt.pr "%a" (pp_line levels indents) content in
    let open_vbox, close_box = Format.(open_vbox, close_box) in
    open_vbox 0;
    go [] 0 "root";
    open_vbox 0;
    go [] 1 "├ a";
    open_vbox 0;
    go [ 1 ] 2 "├ c";
    go [ 1 ] 2 "├ d";
    go [ 1 ] 2 "└ e";
    close_box ();
    go [] 1 "└ b";
    open_vbox 0;
    go [] 2 "├ f";
    go [] 2 "└ g";
    close_box ();
    close_box ();
    close_box ();
    [%expect
      {|
      root
        ├ a
        │ ├ c
        │ ├ d
        │ └ e
        └ b
          ├ f
          └ g |}]
  ;;
end

module Pp_command = struct
  type t =
    | Box of t list
    | Line_printing_command of line_printing_command

  let flatten cmd =
    let queue = Queue.create () in
    let enqueue = Queue.enqueue queue in
    let rec go = function
      | Box cmds ->
        enqueue Flat_pp_command.Open_box;
        List.iter cmds ~f:go;
        enqueue Close_box
      | Line_printing_command cmd -> enqueue (Flat_pp_command.Line_printing_command cmd)
    in
    go cmd;
    Queue.to_list queue
  ;;
end

type status =
  | Is_root
  | Is_inner_child
  | Is_last_child

let connector = function
  | Is_root -> ""
  | Is_inner_child -> (* right-facing tee *) "\u{251C} "
  | Is_last_child -> (* right-facing l *) "\u{2514} "
;;

let rec pp_command status max_depth current_depth spanning_levels { label; children } =
  let next_depth = current_depth + 1 in
  let connector = connector status in
  let pp_line content =
    let spanning_levels' =
      match status, spanning_levels with
      (* (connect to parent with a tee -- don't show a bar) *)
      | Is_inner_child, _ :: levels -> levels
      | _, levels -> levels
    in
    Pp_command.Line_printing_command
      { spanning_levels = List.rev spanning_levels' (* TODO: better data structure *)
      ; indents = current_depth
      ; content
      }
  in
  match children, max_depth with
  | [], _ -> pp_line (connector ^ label)
  | _, Some n when n <= current_depth -> pp_line (Fmt.str "%s%s ..." connector label)
  | _ ->
    (* TODO: better data structure *)
    let last_child = List.last_exn children in
    let pp_command child_status = pp_command child_status max_depth next_depth in
    let inner_child_cmds =
      children
      |> List.drop_last_exn
      |> List.map ~f:(pp_command Is_inner_child (next_depth :: spanning_levels))
    in
    let cmds =
      [ pp_line (connector ^ label) ]
      @ inner_child_cmds
      @ [ pp_command Is_last_child spanning_levels last_child ]
    in
    Box cmds
;;

let pp status max_depth current_depth spanning_levels ppf tree =
  pp_command status max_depth current_depth spanning_levels tree
  |> Pp_command.flatten
  |> List.iter ~f:(Flat_pp_command.run ppf)
;;

let pp ?max_depth ppf t = pp Is_root max_depth 0 [] ppf t
let mk label children = { label; children }
let go ?max_depth = Fmt.pr "%a@." (pp ?max_depth)

let%expect_test "pp" =
  go (mk "root" []);
  [%expect {| root |}]
;;

let t =
  mk
    "root"
    [ mk "a" [ mk "c" []; mk "d" []; mk "e" [] ]; mk "b" [ mk "f" []; mk "g" [] ] ]
;;

let%expect_test "pp_command" =
  let go tree =
    tree
    |> pp_command Is_root None 0 []
    |> Pp_command.flatten
    |> List.iter ~f:(Fmt.pr "%a@." Flat_pp_command.pp)
  in
  go t;
  [%expect
    {|
      Open_box
      Line_printing_command { spanning_levels = []; indents = 0; content = "root" }
      Open_box
      Line_printing_command { spanning_levels = []; indents = 1;
                            content = "\226\148\156 a" }
      Line_printing_command { spanning_levels = [1]; indents = 2;
                            content = "\226\148\156 c" }
      Line_printing_command { spanning_levels = [1]; indents = 2;
                            content = "\226\148\156 d" }
      Line_printing_command { spanning_levels = [1]; indents = 2;
                            content = "\226\148\148 e" }
      Close_box
      Open_box
      Line_printing_command { spanning_levels = []; indents = 1;
                            content = "\226\148\148 b" }
      Line_printing_command { spanning_levels = []; indents = 2;
                            content = "\226\148\156 f" }
      Line_printing_command { spanning_levels = []; indents = 2;
                            content = "\226\148\148 g" }
      Close_box
      Close_box |}]
;;

let%expect_test "pp" =
  go t;
  [%expect
    {|
      root
        ├ a
        │ ├ c
        │ ├ d
        │ └ e
        └ b
          ├ f
          └ g |}]
;;

let%expect_test "pp" =
  go ~max_depth:1 t;
  [%expect {|
      root
        ├ a ...
        └ b ... |}]
;;
