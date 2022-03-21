open Base
open Prelude

type string_stream = (Uutf.decoder * Uchar.t option) ref

module Parser :
  Signatures.Parser
    with type token = Uchar.t
     and type token_tag = Uchar.t
     and type token_set = Char_class.t
     and type stream = string_stream
     and type 'a v = 'a
     and type 'a parser = string_stream -> 'a =
  Unstaged.Make (Token_streams.Uchar)

module Library = Library.Make (Parser.Construction)
include Parser
include Library
open Construction

module Stream = struct
  include Stream

  let of_string str = Token_streams.Uchar.of_decoder (Uutf.decoder (`String str))
end

let ctok c = tok (Char_class.Char.singleton c)
let charset str = tok (Char_class.of_string str)
let lower = charset "abcdefghijklmnopqrstuvwxyz"
let upper = charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let string_of_uchars chars = chars |> List.map ~f:Uchar.to_char_exn |> String.of_char_list
let make_paren_parser c1 c2 p = ctok c1 ++ p ++ ctok c2 ==> fun ((_, x), _) -> x
let braces p = make_paren_parser '{' '}' p
let parens p = make_paren_parser '(' ')' p
let brackets p = make_paren_parser '[' ']' p

module Sexp = struct
  type sexp =
    | Sym of string
    | Seq of sexp list

  let rec pp ppf =
    let open Fmt in
    function Sym str -> string ppf str | Seq sexps -> parens (list pp ~sep:sp) ppf sexps
  ;;

  let word = plus lower ==> string_of_uchars

  let sexp =
    fix (fun sexp ->
        choice
          ~failure_msg:"word or parens"
          [ (word ==> fun sym -> Sym sym)
          ; (parens (sep_by ~sep:(ctok ' ') sexp) ==> fun s -> Seq s)
          ])
  ;;
end

module Arith = struct
  let digits = plus (charset "0123456789")
  let whitespace = star (charset " \t\n")

  let num : float t =
    digits ++ ctok '.' ++ option digits
    ==> fun ((digits1, _), digits2) ->
    let digits2 = Base.Option.value digits2 ~default:[] in
    Float.of_string (string_of_uchars digits1 ^ "." ^ string_of_uchars digits2)
  ;;

  let chr' c = ctok c <* whitespace

  let arith =
    fix (fun arith ->
        infix
          [ Right, chr' '^' ==> always Stdlib.Float.pow
          ; Left, chr' '*' ==> always Float.( * )
          ; Left, chr' '+' ==> always Float.( + )
          ]
          (choice ~failure_msg:"num or parenthesized expression" [ num; parens arith ]
          <* whitespace))
  ;;
end

let%test_module _ =
  (module struct
    open Construction
    open Library

    let mk_gram p = p.tdb []
    let typeof' p = typeof [] (mk_gram p)

    let go ty =
      try Fmt.pr "%a@." Type.pp (typeof' ty) with
      | Type_error fmt -> Fmt.pr "%a@." fmt (Some 8)
    ;;

    let%expect_test "typechecking" =
      go (eps ());
      go (ctok 'a');
      go bot;
      go upper;
      go (seq (star (ctok 'a')) (ctok 'b'));
      go
        (alt
           (plus (ctok 'a'))
           (seq (ctok 'a') (star (ctok 'b')) ==> fun (x, xs) -> x :: xs));
      [%expect
        {|
        {first: [];
         flast: [];
         null: true;
         guarded: true}
        {first: a;
         flast: [];
         null: false;
         guarded: true}
        {first: [];
         flast: [];
         null: false;
         guarded: true}
        {first: [A-Z];
         flast: [];
         null: false;
         guarded: true}
        {first: a;
         flast: [];
         null: false;
         guarded: true}
        alt must be apart
          ({first: a;
            flast: a;
            null: false;
            guarded: true}
          vs {first: a;
              flast: b;
              null: false;
              guarded: true})
          conditions:
            (is_empty (inter t1.first t2.first)): false
            not (t1.null && t2.null):true
          parser:
            Alt
              ==>
                Seq
                  Star
                    Tok b
                  Tok a
              plus
                Star
                  Tok a
                Tok a |}]
    ;;

    let go p pp str =
      let stream = Stream.of_string str in
      try
        let parsed = parse_exn p stream in
        match Stream.peek stream with
        | None -> Fmt.pr "@[%a@]@." pp parsed
        | Some _ -> Fmt.pr "@[parsed with leftovers:@ %a@]@." pp parsed
      with
      | Parse_error msg -> Fmt.pr "Parse error: %s@." msg
      | Type_error msg -> Fmt.pr "Type error: %a@." msg None
    ;;

    let%expect_test "tok" =
      go (ctok 'c') Token.pp "c";
      go (ctok 'c') Token.pp "d";
      [%expect {|
        c
        Parse error: Unexpected token 'd' (expected 'c') |}]
    ;;

    let%expect_test "unicode tok" =
      go (tok (Char_class.singleton (Stdlib.Uchar.of_int 0x1F3C1))) Token.pp "🏁";
      [%expect {| \u1F3C1 |}]
    ;;

    let%expect_test "eps" =
      go (eps ()) (Fmt.any "()") "";
      go (eps ()) (Fmt.any "()") "c";
      [%expect {|
        ()
        parsed with leftovers: () |}]
    ;;

    let%expect_test "bot" =
      go bot (Fmt.any "()") "c";
      [%expect {| Parse error: bottom |}]
    ;;

    let%expect_test "seq" =
      go (seq (ctok 'a') (ctok 'b')) Fmt.(pair ~sep:comma Token.pp Token.pp) "ab";
      [%expect {| a, b |}]
    ;;

    let%expect_test "alt" =
      go (ctok 'a' <|> ctok 'b') Token.pp "a";
      go (ctok 'a' <|> ctok 'b') Token.pp "b";
      go (ctok 'a' <|> ctok 'b') Token.pp "c";
      [%expect {|
        a
        b
        Parse error: No progress possible (<|>) |}]
    ;;

    let%expect_test "star, plus, map" =
      go (star (ctok 'a')) (Fmt.list Token.pp) "";
      go (star (ctok 'a')) (Fmt.list Token.pp) "aaa";
      go (plus (ctok 'a')) (Fmt.list Token.pp) "";
      go (plus (ctok 'a')) (Fmt.list Token.pp) "aaa";
      let uppercase uchar =
        uchar |> Uchar.to_char_exn |> Char.uppercase |> Uchar.of_char
      in
      go (map (List.map ~f:uppercase) (plus (ctok 'a'))) (Fmt.list Token.pp) "aaa";
      [%expect
        {|

        aaa
        Parse error: Unexpected end of stream
        aaa
        AAA |}]
    ;;

    let%expect_test "star again" =
      go (star (charset "ab")) (Fmt.list Token.pp) "abbb";
      [%expect "abbb"]
    ;;

    let%expect_test "choice, option" =
      let go' = go (choice ~failure_msg:"'a' or 'b'" [ ctok 'a'; ctok 'b' ]) Token.pp in
      go' "a";
      go' "a";
      let go' = go (option (ctok 'a')) (Fmt.option Token.pp) in
      go' "a";
      go' "";
      [%expect {|
        a
        a
        a |}]
    ;;

    let%expect_test "charset, upper, lower" =
      let go' = go (charset "ab") Token.pp in
      go' "a";
      go' "c";
      let go' = go upper Token.pp in
      go' "a";
      go' "A";
      let go' = go lower Token.pp in
      go' "a";
      go' "A";
      let go' = go (lower <?> "foo" <?> "bar") Token.pp in
      go' "A";
      [%expect
        {|
        a
        Parse error: Unexpected token 'c' (expected '[ab]')
        Parse error: Unexpected token 'a' (expected '[A-Z]')
        A
        a
        Parse error: Unexpected token 'A' (expected '[a-z]')
        Parse error: Unexpected token 'A' (expected '[a-z]') |}]
    ;;

    let%expect_test "fail" =
      let go' = go (fail "msg" <?> "foo" <?> "bar") Token.pp in
      go' "A";
      [%expect {|
      Parse error: msg (foo,
      bar) |}]
    ;;

    let%expect_test "sep_by, sep_by1" =
      let go' p = go p (Fmt.list Token.pp) in
      let p = sep_by ~sep:(ctok 'a') (ctok 'b') in
      go' p "";
      go' p "b";
      go' p "bab";
      go' p "babab";
      let p1 = sep_by1 ~sep:(ctok 'a') (ctok 'b') in
      go' p1 "";
      go' p1 "b";
      go' p1 "bab";
      go' p1 "babab";
      [%expect
        {|
        b
        bb
        bbb
        Parse error: Unexpected end of stream
        b
        bb
        bbb |}]
    ;;

    (*
    let%expect_test "sep_end_by, sep_end_by1" =
      let go' p = go p (Fmt.list Token.pp) in
      let p = sep_end_by ~sep:(ctok 'a') (ctok 'b') in
      go' p "";
      (*
      go' p "b";
      go' p "ba";
      go' p "baba";
      let p1 = sep_end_by1 ~sep:(ctok 'a') (ctok 'b') in
      go' p1 "";
      go' p1 "b";
      go' p1 "ba";
      go' p1 "baab";
         *)
      [%expect
        {|
        Type error: seq must be separable
                      ({first: b;
                        flast: a;
                        null: true;
                        guarded: true}
                      vs {first: a;
                          flast: [];
                          null: true;
                          guarded: true})
                      conditions:
                        (is_empty (inter t1.flast t2.first)): false
                        not (t1.null && t2.null): false
                      parser (sep_end_by.<*):
                          (root)
                            ├ Tok a
                            ├ Eps
                            └ sep_by
                              ├ sep_by1
                            │   ├ Star
                            │ │   ├ Tok b
                            │     └ Tok a
                                └ Tok b
                              └ Eps |}]
    ;;
    *)

    let%expect_test "sexp" =
      let go' = go Sexp.sexp Sexp.pp in
      go' "()";
      go' "foo";
      go' "(foo bar)";
      [%expect {|
        ()
        foo
        (foo bar) |}]
    ;;

    let%expect_test "infixr" =
      let p = infixr (ctok ',' ==> fun _ -> List.append) (lower ==> List.return) in
      let go' = go p Fmt.(brackets (list ~sep:comma Token.pp)) in
      go' "a,b,c";
      [%expect {| [a, b, c] |}]
    ;;

    let%expect_test "infixl" =
      let p = infixl (ctok ',' ==> fun _ -> List.append) (lower ==> List.return) in
      let go' = go p Fmt.(brackets (list ~sep:comma Token.pp)) in
      go' "a,b,c";
      [%expect {| [a, b, c] |}]
    ;;

    let%expect_test "arith" =
      let go' = go Arith.num Fmt.float in
      go' "1.2";
      go' "0.1";
      go' "1.0";
      go' "1.";
      let go' = go Arith.arith Fmt.float in
      go' "1.0 + 2.0";
      go' "1.0 * 2.0";
      go' "2.0 ^ 2.0";
      go' "1.0 ^ 2.0 + 1.0 * 2.0 ^ 2.0";
      go' "1.0 ^ (2.0 + 1.0 * 2.0 ^ 2.0)";
      go' "1.0 ^ 2.0 + (1.0 * 2.0) ^ 2.0";
      [%expect
        {|
        1.2
        0.1
        1
        1
        3
        2
        4
        5
        1
        5 |}]
    ;;
  end)
;;
