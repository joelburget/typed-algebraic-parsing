[@@@warning "-26-27"]

let p1 = [%parser bot]
let p2 = [%parser seq bot bot]
let p3 = [%parser alt bot bot]
let p4 = [%parser eps 1]
let p5 = [%parser tok 'c']
(* let p5 = [%parser star (tok 'c')] *)
