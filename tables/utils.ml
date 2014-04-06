(*
 * Copyright (c) 2013, David Rufino <david.rufino@gmail.com>
 * All rights reserved. See LICENSE details.
 *)

open Array

(*external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply";;*)

let (|>) f g = (fun x -> g (f x))

let rec range i j = if i >= j then [] else i :: (range (i+1) j)

let bxor x y =
  if (x && y) then false 
  else if ((not x) && (not y)) then false
  else true

let findrev pred arr =
  let rec findrev_helper i pred arr =
    if i == -1 then
      raise (Failure "error")
    else
      if (pred arr.(i)) then
        i
      else
        findrev_helper (i-1) pred arr
  in
  findrev_helper ((length arr) - 1) pred arr

