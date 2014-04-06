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

(* Generic power function.
 * Supply multiplication operator and multiplicative identity to get a fast-ish power function
 * 
 * usage: let my_pow = gen_pow my_one my_multiply  
 *)
val gen_pow : 'a -> ('a -> 'a -> 'a) -> 'a -> int -> 'a
