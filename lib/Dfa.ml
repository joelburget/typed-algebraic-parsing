open Base

module Int_pair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module type S = sig
  type t

  include Base.Comparable.S with type t := t

  val pp : t Fmt.t
  val class' : t -> (Char_class.t, Char_class.comparator_witness) Set.t
  val nullable : t -> bool
  val delta : Uchar.t -> t -> t
end

module Make (T : S) = struct
  type t =
    { state_numbers : (int * T.t) list
    ; accepting : int list
    ; transitions : ((int * int) * Char_class.t) list
    }

  let pp =
    let open Fmt in
    let pair x y = parens (pair ~sep:comma x y) in
    let list elem = brackets (list elem ~sep:semi) in
    braces
      (record
         ~sep:semi
         [ field "state_numbers" (fun t -> t.state_numbers) (list (pair int T.pp))
         ; field "accepting" (fun t -> t.accepting) (list int)
         ; field
             "transitions"
             (fun t -> t.transitions)
             (list (pair (pair int int) Char_class.pp))
         ])
  ;;

  type re_map = (T.t, int, T.comparator_witness) Map.t
  type connection_alist = ((int * int) * Char_class.t) list
  type graph_state = re_map * connection_alist

  let make vec =
    let rec explore graph (state : T.t) : graph_state =
      state
      |> T.class'
      |> Set.filter ~f:(fun cls -> not (Char_class.is_empty cls))
      |> Set.to_list
      |> List.fold_left ~init:graph ~f:(goto state)
    and goto (vec : T.t) (states, edges) char_class : graph_state =
      let char = Char_class.choose_exn char_class in
      let derived_re = T.delta char vec in
      let mk_edge dest = ((Map.find_exn states vec, dest), char_class) :: edges in
      match Map.find states derived_re with
      | Some w -> states, mk_edge w
      | None ->
        let size = Map.length states in
        explore (Map.add_exn states ~key:derived_re ~data:size, mk_edge size) derived_re
    in
    let graph : graph_state = Map.singleton (module T) vec 0, [] in
    let states, edges = explore graph vec in
    let transitions =
      edges |> Map.of_alist_reduce (module Int_pair) ~f:Char_class.union |> Map.to_alist
    in
    let state_numbers = states |> Map.to_alist |> List.map ~f:(fun (vec, i) -> i, vec) in
    let accepting = states |> Map.filter_keys ~f:T.nullable |> Map.data in
    { state_numbers; accepting; transitions }
  ;;
end
