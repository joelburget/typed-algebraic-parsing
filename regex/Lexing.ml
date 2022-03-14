open Base

type 'a action =
  | Skip
  | Error of string
  | Return of 'a

type 'a t = (Regex.t * 'a action) list

type match_location =
  { start : int
  ; finish : int
  }

type 'a match_ = match_location * 'a action

let lex' rules str start0 =
  let len = String.length str in
  let rec loop actions start =
    let matches =
      List.filter_map rules ~f:(fun (re, action) ->
          Regex.match_prefix' str start re |> Option.map ~f:(fun finish -> finish, action))
    in
    match matches with
    | [] -> Result.Error start
    | _ ->
      let finish, action =
        List.fold
          matches
          ~init:(-1, Skip)
          ~f:(fun ((prev_len, _) as accum) ((len, _) as candidate) ->
            if Int.(len > prev_len) then candidate else accum)
      in
      let actions = ({ start; finish }, action) :: actions in
      if Int.(finish >= len) then Ok actions else loop actions finish
  in
  loop [] start0 |> Result.map ~f:List.rev
;;

let lex rules str = lex' rules str 0

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
      let pp_match_location =
        Fmt.(
          braces
            (record
               ~sep:semi
               [ field "start" (fun loc -> loc.start) int
               ; field "finish" (fun loc -> loc.finish) int
               ]))
      in
      let pp_token = Fmt.(parens (pair ~sep:semi pp_match_location pp_action)) in
      let pp ppf result =
        match result with
        | Ok actions -> Fmt.(brackets (list pp_token ~sep:semi)) ppf actions
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
        [({start: 0;
           finish: 1}; Return 1)]
        [({start: 0;
           finish: 2}; Return 2)]
        [({start: 0;
           finish: 3}; Return 3)]
        [({start: 0;
           finish: 3}; Return 3);
         ({start: 3;
           finish: 5}; Return 2);
         ({start: 5;
           finish: 6}; Return 1)] |}]
    ;;
  end)
;;
