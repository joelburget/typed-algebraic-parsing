(*
   RE - A regular expression library
   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

open Base

type c = char
type t = (c * c) list

let compare =
  let compare_tup (x1, y1) (x2, y2) =
    let x = Char.compare x1 x2 in
    if x <> 0 then x else Char.compare y1 y2
  in
  List.compare compare_tup
;;

let ( = ) =
  let equal_tup (x1, y1) (x2, y2) = Char.(x1 = x2 && y1 = y2) in
  List.equal equal_tup
;;

let equal = ( = )

let succ c =
  match c |> Char.to_int |> Int.succ |> Char.of_int with None -> c | Some c -> c
;;

let pred c =
  match c |> Char.to_int |> Int.pred |> Char.of_int with None -> c | Some c -> c
;;

let rec union l l' =
  match l, l' with
  | _, [] -> l
  | [], _ -> l'
  | (c1, c2) :: r, (c1', c2') :: r' ->
    if Char.(succ c2 < c1')
    then (c1, c2) :: union r l'
    else if Char.(succ c2' < c1)
    then (c1', c2') :: union l r'
    else if Char.(c2 < c2')
    then union r ((Char.min c1 c1', c2') :: r')
    else union ((Char.min c1 c1', c2) :: r) r'
;;

let rec inter l l' =
  match l, l' with
  | _, [] -> []
  | [], _ -> []
  | (c1, c2) :: r, (c1', c2') :: r' ->
    if Char.(c2 < c1')
    then inter r l'
    else if Char.(c2' < c1)
    then inter l r'
    else if Char.(c2 < c2')
    then (Char.max c1 c1', c2) :: inter r l'
    else (Char.max c1 c1', c2') :: inter l r'
;;

let rec diff l l' =
  match l, l' with
  | _, [] -> l
  | [], _ -> []
  | (c1, c2) :: r, (c1', c2') :: r' ->
    if Char.(c2 < c1')
    then (c1, c2) :: diff r l'
    else if Char.(c2' < c1)
    then diff l r'
    else (
      let r'' = if Char.(c2' < c2) then (succ c2', c2) :: r else r in
      if Char.(c1 < c1') then (c1, pred c1') :: diff r'' r' else diff r'' r')
;;

let single c = [ c, c ]
let add c l = union (single c) l
let seq c c' = if Char.(c <= c') then [ c, c' ] else [ c', c ]
let empty = []

let rec mem c s =
  match s with
  | [] -> false
  | (c1, c2) :: rem -> if Char.(c <= c2) then Char.(c >= c1) else mem c rem
;;

let print_one ppf (c1, c2) =
  if Char.(c1 = c2) then Fmt.pf ppf "%C" c1 else Fmt.pf ppf "%C-%C" c1 c2
;;

let pp = Fmt.(brackets (list print_one))

let rec iter t ~f =
  match t with
  | [] -> ()
  | (x, y) :: xs ->
    f x y;
    iter xs ~f
;;

let one_char = function [ (i, j) ] when Char.(i = j) -> Some i | _ -> None
let fold_right t ~init ~f = List.fold_right ~f ~init t
let csingle c = single c
let cany = [ Char.of_int_exn 0, Char.of_int_exn 255 ]
let is_empty = function [] -> true | _ -> false

let is_any = function
  | [ (a, b) ] -> Char.(a = Char.of_int_exn 0) && Char.(b = Char.of_int_exn 255)
  | _ -> false
;;

let rec prepend s x l =
  match s, l with
  | [], _ -> l
  | _r, [] -> []
  | (_c, c') :: r, ([ (d, _d') ], _x') :: _r' when Char.(c' < d) -> prepend r x l
  | (c, c') :: r, ([ (d, d') ], x') :: r' ->
    if Char.(c <= d)
    then
      if Char.(c' < d')
      then ([ d, c' ], x @ x') :: prepend r x (([ succ c', d' ], x') :: r')
      else ([ d, d' ], x @ x') :: prepend s x r'
    else if Char.(c > d')
    then ([ d, d' ], x') :: prepend s x r'
    else ([ d, pred c ], x') :: prepend s x (([ c, d' ], x') :: r')
  | _ -> assert false
;;

let pick = function [] -> invalid_arg "Re_cset.pick" | (x, _) :: _ -> x
