(* Some abstract algebra nonsense to facilitate field extensions
 *
 * Copyright (c) 2013, David Rufino <david.rufino@gmail.com>
 * All rights reserved. See LICENSE for details.
 *)

module type Set =
sig
  type elem

  val (==) : elem -> elem -> bool

  val to_string : elem -> string

  val to_int : elem -> int
  val of_int : int -> elem
end

(* Ring *)
module type Ring =
sig
  include Set

  val zero : elem
  val one : elem
  val iZ : int -> elem
  val ( ** ) : elem -> elem -> elem
  val ( ++ ) : elem -> elem -> elem
  val opp : elem -> elem 
end

(* Ring with multiplicative inverses *)
module type Field =
sig
  include Ring

  val inv : elem -> elem
end 

module type One_element =
sig
  type elem
  val elt : elem
end

let gen_pow (one : 'a) (mul : 'a -> 'a -> 'a) (f : 'a) (n : int) =
  let rec gen_pow_helper mul acc n xi =
    if n = 0 then
      acc
    else
      let acc = if ((n land 1) = 1) then (mul acc xi) else acc in
      let n = n lsr 1 in
      gen_pow_helper mul acc n (mul xi xi)
  in
  gen_pow_helper mul one n f

