(*
 * Some AES field arithmetic over GF(2^8) along with isomorphism to composite field extension
 * GF(2^8)/GF(2^4)/GF(2) required to implement
 *
 * Hamburg, Mike "Accelerating AES with vector permute instructions"
 * http://shiftleft.org/papers/vector_aes/vector_aes.pdf
 *
 * Copyright (c) 2013, David Rufino <david.rufino@gmail.com>
 * All rights reserved. See LICENSE for details.
 *)

open Array
open Utils
open GF2n

module AesModulus =
struct
  type elem = bool array

  (* 1 + X  + X^3 + X^4 + X^8 *)
  let modulus = [| true; true; false; true; true; false; false; false; true |]

  let var_name = "X"
end

(* This is the modulus to construct 4-th order cyclotomic extension *)
module GF24Modulus =
struct
  type elem = bool array

  (* 1 + X + X^2 + X^3 *)
  let modulus = [| true; true; true; true; true |]

  let var_name = "z"
end

(* GF(2^8) as in AES *)
module GFAES = GF2n(AesModulus)

(* GF(2^4) using cyclotomic extension *)
module GF24  = GF2n(GF24Modulus)

module QuadTower =
  functor (F : Algebra.Field) ->
  functor (Z : Algebra.One_element with type elem = F.elem) ->
struct
  type base_elem = F.elem

  type elem = base_elem * base_elem

  (* single element *)
  let zeta = Z.elt

  let lift fn z =
    match z with 
    x,y -> (fn x),(fn y) 

  (* operations on the base field *)
  let ( ++. ) = F.( ++ )
  let ( **. ) = F.( ** )
  let ( ==. ) = F.( == )

  let ( ++ ) x y = 
    match x,y with
    (x1,y1),(x2,y2) -> ((x1 ++. x2),(y1 ++. y2))

  let to_string w =
    match w with
    x,y ->
      let xs = F.to_string x in
      let ys = F.to_string y in
      Printf.sprintf "(%s)t + (%s) tbar" xs ys

  (* binary conversion *)
  let to_int z =
    match z with
    x,y -> (F.to_int x) lor ((F.to_int y) lsl 4)

  (* binary conversion *)
  let of_int i =
    (F.of_int (i land 0x0f)),(F.of_int ((i land 0xf0) lsr 4))

  (* embed base field *)
  let iBase x = (x,x)

  let iZ n = iBase (F.iZ n)

  let zero = iZ 0
  let one = iZ 1

  (* basis elements *)
  let t = (F.one, F.zero)
  let tbar = (F.zero, F.one)

  let opp z = 
    match z with
    x,y -> (F.opp x),(F.opp y)

  let ( == ) z1 z2 =
    match z1,z2 with
    (x1,y1),(x2,y2) ->
      (x1 ==. x2) && (y1 ==. y2)

  let ( ** ) z1 z2 =
    match z1,z2 with
    (x1,y1),(x2,y2) ->
      let tmp = ((x1 ++. y1) **. (x2 ++. y2)) **. zeta in
      let x1x2 = (x1 **. x2) in
      let y1y2 = (y1 **. y2) in
      (x1x2 ++. tmp),(y1y2 ++. tmp)

  let ( *** ) = Algebra.gen_pow one ( ** )

  let inv z =
    match z with
    x,y -> 
      let x2 = x **. x in
      let y2 = y **. y in
      let xy = x **. y in
      let tmp = (xy ++. (zeta **. (x2 ++. y2))) in
      let tmp = F.inv tmp in
      (tmp **. y),(x **. tmp)

  (* adjoin an infinity flag *)
  type base_inf = base_elem * bool

  (* use the nested inversion method, need to keep track of infinity flags too
   * otherwise doesnt always work
   *)
  let inv2 z = 
    match z with
    x,y ->
     (* define an operation on the expanded set *)
     let ( ++. ) x y = 
        match x,y with
        (x,xinf),(y,yinf) -> (F.(++) x y),(bxor xinf yinf)
      in
      let inv x =
        match x with
        (* 1/oo = 0 *)
        | _,true -> (F.zero,false)
        | x,false -> if (x ==. F.zero) then (F.zero,true) else ((F.inv x),false)
      in
      (* embed GF(2^4) -> GF(2^8) *)
      let iBase2 x =
      match x with
      _,true -> failwith "cannot embed oo";
      | x,false -> iBase x
      in 
      (* arithmetic on the base field *)
      let zxPy = zeta **. (F.(++) x y) in
      let x = x,false in
      let y = y,false in
      let zxPy_inv = inv (zxPy,false) in
      let x_inv = inv x in
      let y_inv = inv y in
      let tmp1 = inv (y_inv ++. zxPy_inv) in
      let tmp1 = inv (tmp1 ++. x) in
      let tmp1 = iBase2 tmp1 in
      let tmp2 = inv (x_inv ++. zxPy_inv) in
      let tmp2 = inv (tmp2 ++. y) in
      let tmp2 = iBase2 tmp2 in
      let zeta = iBase zeta in
      ((t ++ zeta) ** tmp1) ++ ((tbar ++ zeta) ** tmp2) 

end

module ZetaElt = 
struct
  type elem = GF24.elem
  (* this is \zeta *)
  let elt = GF24.of_int 2
end
module GF28 = QuadTower(GF24)(ZetaElt)

(* homomorphism GFAES -> GF28 *)
let phi_ phi_X f =
  let phi_Xi i = GF28.( *** ) phi_X i in
  let bits = GFAES.to_int f in
  let terms = Array.init 8 (fun i -> if (bits land (1 lsl i)) = 0 then GF28.zero else phi_Xi i) in
  Array.fold_left GF28.( ++ ) GF28.zero terms

(* embed GF24 in GFAES *)
let embed_GF24 zeta f =
  let coeffs = GF24.to_coeffs f in
  let zeta_n = Array.init 4 (GFAES.( *** ) zeta) in
  let terms = Array.init 4 (fun i -> if coeffs.(i) then zeta_n.(i) else GFAES.zero) in
  Array.fold_left GFAES.( ++ ) GFAES.zero terms

(* homomorphism GF28 -> GFAES *)
let psi_ t zeta z =
  match z with
  x,y -> 
    let ( ** ) = GFAES.( ** ) in
    let ( ++ ) = GFAES.( ++ ) in
    let tbar = t ++ GFAES.one in
    let x = embed_GF24 zeta x in
    let y = embed_GF24 zeta y in
    (t ** x) ++ (tbar ** y)

(* all of the elements *)
let gfaes_elts = List.map GFAES.of_int (Utils.range 1 256)

(* basis change form tower rep to polynomial rep *)
let inv_phi_ phi_X =
    (* brute force inversion function *)
    let invert phi x =
        List.hd (List.filter (fun z -> (GF28.( == ) x (phi z))) gfaes_elts)
    in
    (* just invert t, zeta which generate the field *)
    let t = invert (phi_ phi_X) GF28.t in
    let zeta = invert (phi_ phi_X) (GF28.iBase GF28.zeta) in 
    (* simpler inversion function *)
    psi_ t zeta

(* pick a specific isomorphism *)
let phi = phi_ (GF28.of_int 0x0a)
let inv_phi = inv_phi_ (GF28.of_int 0x0a)


let lookup_hword_to_byte2 fn =
  for i = 0 to 15
  do
    let j = fn i in
    Printf.printf "0x%.2x," j ;
  done;
  print_string "\n";
  ()

let lookup_hword_to_byte fn =
  for i = 15 downto 0
  do
    let j = fn i in
    let tmp = ((i) mod 4) in
    match tmp with 
      3 -> Printf.printf ",0x%.2x" j;
      (*| 1 -> (Printf.printf ",";);*)
      |_ -> Printf.printf "%.2x" j; 
  done;
  print_string "\n";
  ()

let lookup_gf24_to_byte fn =
  lookup_hword_to_byte2 (fun z -> fn (GF24.of_int z))

module Int32Ops =
struct
  let ( ^ ) = Int32.logxor 
  let ( << ) = Int32.shift_left
  let ( >> ) = Int32.shift_right_logical 
  let ( & ) = Int32.logand
end

(* S-Box consists of inversion followed by affine transformation *)
(* this is the linear component, implementation taken from Brian Gladman's code *) 
let affine_forward1 (x : GFAES.elem) : GFAES.elem =
  let open Int32Ops in
  let w = Int32.of_int (GFAES.to_int x) in
  let w = w ^ (w << 1) ^ (w << 2) ^ (w << 3) ^ (w << 4) in
  GFAES.of_int (Int32.to_int ((w ^ (w >> 8)) & 255l))

(* this is the inverse linear component *)
let affine_backward1 (x : GFAES.elem) : GFAES.elem = 
  let open Int32Ops in
  let w = Int32.of_int (GFAES.to_int x) in
  let w = (w << 1) ^ (w << 3) ^ (w << 6) in
  let w = (w ^ (w >> 8)) & 255l in
  GFAES.of_int (Int32.to_int w)

(* this is the shift component *)
let affine_shift x =
  GFAES.of_int ((GFAES.to_int x) lxor 0x63)

let sbox x =
 let xinv = GFAES.inv x in
 let k = affine_shift (affine_forward1 xinv) in
 k

let invsbox i =
  let x = affine_backward1 (affine_shift i) in
  let xinv = GFAES.inv x in
  xinv
