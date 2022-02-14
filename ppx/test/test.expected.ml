[@@@warning "-26-27"]
let p1 ~index  s = let n = Base.String.length s in failwith "impossible"
let p2 ~index  s =
  let n = Base.String.length s in
  ((failwith "impossible"), (failwith "impossible"))
let p3 ~index  s =
  let n = Base.String.length s in
  if let open Base.Int in index >= n
  then failwith "impossible"
  else
    (let c = Base.String.unsafe_get s index in
     failwith "No progress possible!")
let p4 ~index  s = let n = Base.String.length s in 1
