open Base
open Prelude

type string_stream = (Uutf.decoder * Uchar.t option) ref

module Parser :
  Signatures.Parser
    with type token = Uchar.t
     and type token_tag = Uchar.t
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

let ctok c = tok (Uchar.of_char c)
let charset s = s |> String.to_list |> List.map ~f:ctok |> choice
let lower = charset "abcdefghijklmnopqrstuvwxyz"
let upper = charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let string_of_uchars chars = chars |> List.map ~f:Uchar.to_char_exn |> String.of_char_list

module Sexp = struct
  type sexp =
    | Sym of string
    | Seq of sexp list

  let rec pp ppf =
    let open Fmt in
    function Sym str -> string ppf str | Seq sexps -> parens (list pp ~sep:sp) ppf sexps
  ;;

  let paren p = ctok '(' ++ p ++ ctok ')' ==> fun ((_, x), _) -> x
  let word = plus lower ==> string_of_uchars

  let sexp =
    fix (fun sexp ->
        choice
          [ (word ==> fun sym -> Sym sym)
          ; (paren (sep_by (ctok ' ') sexp) ==> fun s -> Seq s)
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
          (choice [ num; Sexp.paren arith ] <* whitespace))
  ;;
end

let%test_module _ =
  (module struct
    open Construction
    open Library

    let mk_gram p = p.tdb []
    let typeof' p = typeof [] (mk_gram p)
    let go ty = Fmt.pr "%a@." Type.pp (typeof' ty)

    let%expect_test "typechecking" =
      go (eps ());
      go (ctok 'a');
      go bot;
      go upper;
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
         guarded: true} |}]
    ;;

    let typecheck : type a. a Construction.t -> (unit, a, Type.t) Grammar.t =
     fun { tdb } -> Grammar.typeof [] (tdb [])
   ;;

    let parse p = parse (typecheck p) Parse_env.[]

    let go p pp str =
      let stream = Stream.of_string str in
      try
        let parsed = parse p stream in
        match Stream.peek stream with
        | None -> Fmt.pr "@[%a@]@." pp parsed
        | Some _ -> Fmt.pr "@[parsed with leftovers:@ %a@]@." pp parsed
      with
      | Parse_error msg -> Fmt.pr "failed parse: %s@." msg
    ;;

    let%expect_test "tok" =
      go (ctok 'c') Token.pp "c";
      go (ctok 'c') Token.pp "d";
      [%expect {|
        c
        failed parse: Unexpected token 'd' (expected 'c') |}]
    ;;

    let%expect_test "unicode tok" =
      go (tok (Stdlib.Uchar.of_int 0x1F3C1)) Token.pp "🏁";
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
      [%expect {| failed parse: bottom |}]
    ;;

    let%expect_test "seq" =
      go (seq (ctok 'a') (ctok 'b')) Fmt.(pair ~sep:comma Token.pp Token.pp) "ab";
      [%expect {| a, b |}]
    ;;

    let%expect_test "alt" =
      go (alt (ctok 'a') (ctok 'b')) Token.pp "a";
      go (alt (ctok 'a') (ctok 'b')) Token.pp "b";
      go (alt (ctok 'a') (ctok 'b')) Token.pp "c";
      [%expect {|
        a
        b
        failed parse: No progress possible |}]
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
        failed parse: Unexpected end of stream
        aaa
        AAA |}]
    ;;

    let%expect_test "star again" =
      go (star (charset "ab")) (Fmt.list Token.pp) "abbb";
      [%expect "abbb"]
    ;;

    let%expect_test "choice, option" =
      let go' = go (choice [ ctok 'a'; ctok 'b' ]) Token.pp in
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
      [%expect
        {|
        a
        failed parse: No progress possible
        failed parse: No progress possible
        A
        a
        failed parse: No progress possible |}]
    ;;

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
