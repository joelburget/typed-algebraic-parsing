module Make (Token : Signatures.Token) : Signatures.Type with module Token = Token =
struct
  module Token = Token

  type t =
    { first : Token.Set.t (** The set of characters which can start a string in [L]. *)
    ; flast : Token.Set.t
          (** The set of tokens which fan follow the last character of a string in [L]. *)
    ; null : bool (** Is the empty string in [L]? *)
    ; guarded : bool
          (** Tracks whether a variable is in the guarded or unguarded context. *)
    }

  let pp_set ppf set = Token.Set.pp ppf set

  let pp =
    let open Fmt in
    braces
      (record
         ~sep:semi
         [ field "first" (fun t -> t.first) pp_set
         ; field "flast" (fun t -> t.flast) pp_set
         ; field "null" (fun t -> t.null) bool
         ; field "guarded" (fun t -> t.guarded) bool
         ])
  ;;

  let ( = ) t1 t2 =
    Token.Set.(t1.first = t2.first)
    && Token.Set.(t1.flast = t2.flast)
    && Bool.(t1.null = t2.null && t1.guarded = t2.guarded)
  ;;

  let empty = Token.Set.empty
  let ( ==> ) b cs = if b then cs else empty
  let bot = { first = empty; flast = empty; null = false; guarded = true }
  let eps = { first = empty; flast = empty; null = true; guarded = true }
  let tok set = { first = set; flast = empty; null = false; guarded = true }

  let separable t1 t2 =
    Token.Set.(is_empty (inter t1.flast t2.first)) && not (t1.null && t2.null)
  ;;

  let apart t1 t2 =
    Token.Set.(is_empty (inter t1.first t2.first)) && not (t1.null && t2.null)
  ;;

  let pp_labels ppf labels = Fmt.(list string ~sep:(any ".")) ppf (List.rev labels)

  let pp_labels ppf = function
    | [] -> Fmt.nop ppf ()
    | labels -> Fmt.pf ppf " %a" (Fmt.parens pp_labels) labels
  ;;

  let alt labels pp_g t1 t2 =
    Prelude.type_assert (apart t1 t2) (fun ppf () ->
        Fmt.pf
          ppf
          "@[<v 2>alt must be apart@;\
           @[(%a@ vs@ %a)@]@;\
           conditions:@;\
           <0 2>(is_empty (inter t1.first t2.first)): %b@;\
           <0 2>not (t1.null && t2.null):%b@;\
           @[<v 2>parser%a:@ %a@]@;\
           @]"
          pp
          t1
          pp
          t2
          Token.Set.(is_empty (inter t1.first t2.first))
          (not (t1.null && t2.null))
          pp_labels
          labels
          pp_g
          ());
    { first = Token.Set.union t1.first t2.first
    ; flast = Token.Set.union t1.flast t2.flast
    ; null = t1.null || t2.null
    ; guarded = t1.guarded && t2.guarded
    }
  ;;

  let seq labels pp_g t1 t2 =
    Prelude.type_assert (separable t1 t2) (fun ppf () ->
        Fmt.pf
          ppf
          "@[<v 2>seq must be separable@;\
           @[(%a@ vs@ %a)@]@;\
           conditions:@;\
           <0 2>(is_empty (inter t1.flast t2.first)): %b@;\
           <0 2>not (t1.null && t2.null): %b@;\
           @[<v 2>parser%a:@ %a@]@;\
           @]"
          pp
          t1
          pp
          t2
          Token.Set.(is_empty (inter t1.flast t2.first))
          (not (t1.null && t2.null))
          pp_labels
          labels
          pp_g
          ());
    { first = t1.first
    ; flast = Token.Set.union t2.flast (t2.null ==> Token.Set.union t2.first t1.flast)
    ; null = false
    ; guarded = t1.guarded
    }
  ;;

  let star labels pp_g t =
    { (seq labels pp_g t t) with null = true; flast = Token.Set.union t.flast t.first }
  ;;

  let min = { first = empty; flast = empty; null = false; guarded = false }

  let fix f =
    let rec loop tp =
      let tp' = f tp in
      if tp = tp' then tp else loop tp'
    in
    loop min
  ;;
end
