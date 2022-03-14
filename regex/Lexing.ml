open Base

type 'a action =
  | Skip
  | Error of string
  | Return of 'a

type 'a t = (Regex.t * 'a action) list

let lex rules str =
  let len = String.length str in
  let rec loop actions i =
    let matches =
      List.filter_map rules ~f:(fun (re, action) ->
          Regex.match_prefix' str i re |> Option.map ~f:(fun i -> i, action))
    in
    match matches with
    | [] -> Result.Error i
    | _ ->
      let i, action =
        List.fold
          matches
          ~init:(-1, Skip)
          ~f:(fun ((prev_len, _) as accum) ((len, _) as candidate) ->
            if Int.(len > prev_len) then candidate else accum)
      in
      let actions = action :: actions in
      if Int.(i >= len) then Ok actions else loop actions i
  in
  loop [] 0 |> Result.map ~f:List.rev
;;

let%test_module "lex" =
  (module struct
    let%expect_test _ =
      let pp_action ppf action =
        let open Fmt in
        match action with
        | Skip -> pf ppf "Skip"
        | Error msg -> pf ppf "Error %S" msg
        | Return x -> pf ppf "Return %d" x
      in
      let pp ppf result =
        match result with
        | Ok actions -> Fmt.(brackets (list pp_action ~sep:semi)) ppf actions
        | Error pos -> Fmt.pf ppf "Error %d" pos
      in
      let go rules str = Fmt.pr "%a@." pp (lex rules str) in
      go [] "abc";
      let rules = Regex.[ str "abc", Return 3; str "ab", Return 2; str "a", Return 1 ] in
      go rules "a";
      go rules "ab";
      go rules "abc";
      go rules "abcaba";
      [%expect
        {|
        Error 0
        [Return 1]
        [Return 2]
        [Return 3]
        [Return 3; Return 2; Return 1] |}]
    ;;
  end)
;;
