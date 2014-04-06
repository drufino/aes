(* Simple implementation of finite field arithmetic over characteristic 2 
 *
 * Copyright (c) 2013, David Rufino <david.rufino@gmail.com>
 * All rights reserved. See COPYING for details.
 *)
open Array
open Utils

module type GF2nModulus =
sig
  type elem = bool array

  val modulus : elem

  val var_name : string
end

module GF2n = 
functor (Modulus : GF2nModulus) -> 
struct
    type elem = Coeffs of bool array

    type poly = elem 
    
    (* the modulus, should be monic irreducible *)
    let modulus = Modulus.modulus 

    (* number of bits *)
    let n_bits = (length modulus) - 1

    (* modulus excluding highest order term *)
    let mod_poly = Coeffs (sub modulus 0 n_bits)
 
    (* embed Z *)
    let iZ (n : int) =
      let init_fn i =
        if ((i = 0) && (n land 1 = 1)) then
           true
        else
          false 
      in
      Coeffs (Array.init n_bits init_fn)

    (* from binary *)
    let of_int (x : int)  =
      let coeff_fn i = (x land (1 lsl i)) != 0 in
      let coeffs = Array.init n_bits coeff_fn in 
      Coeffs coeffs 

    (* to binary *)
    let to_int (f : poly) =
      match f with
      Coeffs coeffs ->
        let terms = Array.mapi (fun i x -> if x then (1 lsl i) else 0) coeffs in
        Array.fold_left (lor) 0 terms 

    (* zero *)
    let zero = iZ 0

    (* one *)
    let one = iZ 1

    (* negate *)
    let opp f =
      match f with Coeffs f -> Coeffs (map not f) 

    (* coeffs *)
    let deg f = 
      match f with Coeffs f -> findrev ((=) true) f

    (* add *)
    let ( ++ ) f g =
      match f,g with Coeffs f, Coeffs g ->
        Coeffs (Array.init n_bits (fun i -> (Utils.bxor f.(i) g.(i))))

    let to_string (f : poly) =
      let variable = Modulus.var_name in
      match f with Coeffs f ->
        let coeff_fn (n : int) (coeff : bool) =
          if coeff then
            match n with
            0 -> "1"
            | 1 -> variable 
            | _ -> variable ^ "^" ^ (string_of_int n)
          else
            ""
        in    
        let terms = to_list (mapi coeff_fn f) in
        let terms = List.filter (fun x -> (String.length x) != 0) terms in
        String.concat " + " terms

    let xtime f =
      match f with Coeffs f ->
      (* work out if the leading coefficient is 1 *)
      let hibit = f.(n_bits - 1) in
      (* left shift the bits to multiply by x *)
      let shifted_f = Coeffs (Array.init n_bits (fun i -> if i = 0 then false else f.(i-1))) in
      (* have to add the modulus if high bit was 1 *)
      let xf = if hibit then (shifted_f ++ mod_poly) else shifted_f in
      xf

    let to_coeffs f = match f with Coeffs f -> f

    (* multiply *)
    let ( ** ) f g =
      let rec mult_helper (i : int) (acc : poly) (f : bool array) (xig : poly)  =
        if i = n_bits then
          acc
        else  
          let acc = acc ++ (if f.(i) then xig else zero) in
          let xig = xtime xig in
          mult_helper (i+1) acc f xig
      in
      let f = to_coeffs f in 
      let acc = if f.(0) then g else zero in
      mult_helper 1 acc f (xtime g) 

   (* power *)
    let ( *** ) = Algebra.gen_pow one ( ** )
        
    (* inverse *)
    let inv f = 
      f  *** ((1 lsl n_bits) - 2)

    let is_zero (f : poly) = not (Array.fold_left (||) false (to_coeffs f))

    let of_array coeffs = 
      if Array.length coeffs = n_bits then
        Coeffs coeffs
      else
        raise (Failure "error")

    let (==) f g = is_zero (f ++ g)
end
