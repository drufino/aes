(* A simple implementation of arithmetic over Galois fields of characteristic 2 
 *
 * Copyright david rufino (C) Nov 2013
 *)

open Array

module type GF2nModulus =
sig
  type elem = bool array

  val modulus : elem

  val var_name : string
end

module GF2n
  (Modulus : GF2nModulus) :
  sig
    type elem

    type poly = elem

    (* the modulus, should be monic irreducible *)
    val modulus : bool array

    (* number of bits *)
    val n_bits : int

    (* modulus excluding highest order term *)
    val mod_poly : poly
 
    (* embed Z *)
    val iZ : int -> poly

    (* from binary *)
    val of_int : int -> poly

    (* to binary *)
    val to_int : poly -> int

    (* zero *)
    val zero : poly

    (* one *)
    val one : poly

    (* negate *)
    val opp : poly -> poly

    (* coeffs *)
    val deg : poly -> int

    (* add *)
    val ( ++ ) : poly -> poly -> poly

    (* multiply *)
    val ( ** ) : poly -> poly -> poly

    (* equals *)
    val ( == ) : poly -> poly -> bool

    (* power *)
    val ( *** ) : poly -> int -> poly

    (* inverse *)
    val inv : poly -> poly

    val to_string : poly -> string

    val xtime : poly -> poly

    (* convert to coefficients *)
    val to_coeffs : poly -> bool array
    
    (* convert from coefficient s*)
    val of_array : bool array -> poly
  end
