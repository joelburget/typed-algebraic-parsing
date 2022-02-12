open Prelude

module Make (Ctx : Signatures.Env) = struct
  let rec len : type n. n Ctx.t -> int =
   fun ctx -> match ctx with [] -> 0 | _ :: ctx -> 1 + len ctx
 ;;

  let rec tshift' : type a i j. int -> j Ctx.t -> (a * i) Ctx.t -> (j, a) Var.t =
   fun n c1 c2 ->
    match n, c1, c2 with
    (* Both the 'assert false' and the 'Obj.magic' are safe here,
       since we know (although it's not captured in the types) that
       (a * i) is a prefix of j.
       More details: "Unembedding Domain Specific Languages" §4.4.
     *)
    | _, [], _ -> assert false
    | 0, _ :: _, _ :: _ -> Stdlib.Obj.magic Var.Z
    | n, _ :: c1, c2 -> Var.S (tshift' (n - 1) c1 c2)
 ;;

  let tshift : type a i j. j Ctx.t -> (a * i) Ctx.t -> (j, a) Var.t =
   fun c1 c2 -> tshift' (len c1 - len c2) c1 c2
 ;;
end
