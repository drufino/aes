(*
 * Helper program for generating tables required
 * for implementing AES S-Box algorithm as described in 
 *
 * Hamburg, Mike "Accelerating AES with vector permute instructions"
 * http://shiftleft.org/papers/vector_aes/vector_aes.pdf
 *
 * Copyright (c) 2013, David Rufino <david.rufino@gmail.com>
 * All rights reserved. See LICENSE for details.
 *)
open Aes
open Utils
open List

let main _ =
  print_string "finding primitive_roots\n";
  let ( *** ) = GF28.( *** ) in
  let ( ++ ) = GF28.( ++ ) in
  let ( ** ) = GF28.( ** ) in
  let ( **. ) = GF24.( ** ) in
  let eval_poly z = z *** 8 ++ z *** 4 ++ z *** 3 ++ z ++ GF28.one in
  let check_root z = GF28.( == ) (eval_poly z) GF28.zero in
  let gf28_elts = map GF28.of_int (range 1 256) in
  (* roots of AES poly in tower field *)
  let roots = filter check_root gf28_elts in
  (* the corresponding inverse isomorphisms *)
  let inv_phis = map inv_phi_ roots in
  let inv_t    = map (fun psi -> psi GF28.t) inv_phis in
  let inv_zeta = map (fun psi -> psi (GF28.iBase GF28.zeta)) inv_phis in 
  let debug foo =
    match foo with (x,t),zeta ->
      Printf.printf "(0x%.2x) \tX\t->\t %s\n" (GF28.to_int x) (GF28.to_string x);
      (* Printf.printf "t -> %s (0x%.2x)\n" (GFAES.to_string t) (GFAES.to_int t);
      Printf.printf "\\zeta -> %s (0x%.2x)\n" (GFAES.to_string zeta) (GFAES.to_int zeta); *)
  in   
  iter debug (combine (combine roots inv_t) inv_zeta);
  let my_inv z =
    if GF24.(==) z GF24.zero then
      0x80
    else
      GF24.to_int (GF24.inv z)
  in
  print_string "[*] basis change\n";
  let basis_change x = GF28.to_int (phi x) in
  lookup_hword_to_byte2 (fun i -> basis_change (GFAES.of_int i));
  lookup_hword_to_byte2 (fun i -> basis_change (GFAES.of_int (i lsl 4)));
  print_string "[*] inverse basis change\n";
  let inv_basis_change x = GFAES.to_int (inv_phi x) in
  lookup_hword_to_byte2 (fun i -> inv_basis_change (GF28.of_int i));
  lookup_hword_to_byte2 (fun i -> inv_basis_change (GF28.of_int (i lsl 4)));
  print_string "[*] inverse affine in tower basis\n";
  lookup_hword_to_byte2 (fun i -> basis_change (affine_backward1 (inv_phi (GF28.of_int i))));
  lookup_hword_to_byte2 (fun i -> basis_change (affine_backward1 (inv_phi (GF28.of_int (i lsl 4)))));
  print_string "[*] inverse affine from polynomial to tower basis\n"; 
  (* note only have shift once, as not linear! *)
  lookup_hword_to_byte2 (fun i -> basis_change (affine_backward1 (affine_shift (GFAES.of_int i))));
  lookup_hword_to_byte2 (fun i -> basis_change (affine_backward1 (GFAES.of_int (i lsl 4))));
  print_string "[*] 1/x lookup\n";
  lookup_gf24_to_byte my_inv;
  lookup_gf24_to_byte (fun z -> my_inv (GF28.zeta **. z));
  let tzinv t z =
    let z = (GF28.iBase z) in
    let zeta = GF28.iBase (GF28.zeta) in
    (t ++ zeta) ** (GF28.inv z)
  in
  print_string "[*] (t+\\zeta)/x plus from gf24 basis to polynomial basis\n";
  lookup_gf24_to_byte (fun z -> GFAES.to_int (inv_phi (tzinv GF28.t z)));
  lookup_gf24_to_byte (fun z -> GFAES.to_int (inv_phi (tzinv GF28.tbar z)));
  print_string "[*] affine_linear((t+\\zeta)/x) from gf24 basis to polynomial basis\n";
  let fn x = GFAES.to_int (affine_forward1 (inv_phi x)) in
  lookup_gf24_to_byte (fun z ->  fn (tzinv GF28.t z));
  lookup_gf24_to_byte (fun z ->  fn (tzinv GF28.tbar z));
  print_string "[*] affine_linear((t+\\zeta)/x) from gf24 basis to tower basis\n";
  let fn x = GF28.to_int (phi (affine_forward1 (inv_phi x))) in
  lookup_gf24_to_byte (fun z ->  fn (tzinv GF28.t z));
  lookup_gf24_to_byte (fun z ->  fn (tzinv GF28.tbar z));
  print_string "[*] 2*affine_linear((t+\\zeta)/x) from gf24 basis to tower basis\n";
  let fn x = GF28.to_int (phi (GFAES.( ** ) (GFAES.of_int 2) (affine_forward1 (inv_phi x)))) in
  lookup_gf24_to_byte (fun z ->  0x00 lxor fn (tzinv GF28.t z));
  lookup_gf24_to_byte (fun z ->  fn (tzinv GF28.tbar z));
  let test n =
    lookup_gf24_to_byte ((tzinv GF28.t)     |> (( ** ) (phi (GFAES.of_int n))) |> GF28.to_int);
    lookup_gf24_to_byte ((tzinv GF28.tbar)  |> (( ** ) (phi (GFAES.of_int n))) |> GF28.to_int);
  in
  print_string "[*] (t+\\zeta)/x from gf24 basis to tower basis\n";
  test 1;
  print_string "[*] 2*(t+\\zeta)/x from gf24 basis to tower basis\n";
  test 2;
  print_string "[*] 4*(t+\\zeta)/x from gf24 basis to tower basis\n";
  test 4;
  print_string "[*] 9*(t+\\zeta)/x from gf24 basis to tower basis\n";
  test 9;
  print_string "[*] x -> n * x  (n=2,4,9) in tower basis\n";
  let mult n i =
    let two = phi (GFAES.of_int n) in
    let ( ** ) = GF28.( ** ) in
    GF28.to_int (two ** (GF28.of_int i))
  in  
  lookup_hword_to_byte2 (fun i -> mult 2 i);
  lookup_hword_to_byte2 (fun i -> mult 2 (i lsl 4));
  lookup_hword_to_byte2 (fun i -> mult 4 (i lsl 0));
  lookup_hword_to_byte2 (fun i -> mult 4 (i lsl 4));
  lookup_hword_to_byte2 (fun i -> mult 9 (i lsl 0));
  lookup_hword_to_byte2 (fun i -> mult 9 (i lsl 4));
  print_string "[*] x -> inverse_affine_linear(n * x)  (n=2,4,8) in tower basis\n";
  let mult n i =
    let x = inv_phi (GF28.of_int i) in
    let two = (GFAES.of_int n) in
    let ( ** ) = GFAES.( ** ) in
    let y = affine_backward1 (two ** x) in
    let y = phi y in
    GF28.to_int y
 in
  lookup_hword_to_byte2 (fun i -> mult 2 i);
  lookup_hword_to_byte2 (fun i -> mult 2 (i lsl 4));
  lookup_hword_to_byte2 (fun i -> mult 4 (i lsl 0));
  lookup_hword_to_byte2 (fun i -> mult 4 (i lsl 4));
  lookup_hword_to_byte2 (fun i -> mult 9 (i lsl 0));
  lookup_hword_to_byte2 (fun i -> mult 9 (i lsl 4));
  print_string "[*] S-Box\n";
  for i = 0 to 255
  do
    Printf.printf "%.2x " (GFAES.to_int (sbox (GFAES.of_int i)));
    if ((i+1) mod 16) = 0 then
      (Printf.printf "\n";)
    else
      ();
  done;
  ;;
  
if !Sys.interactive then () else main ();;
