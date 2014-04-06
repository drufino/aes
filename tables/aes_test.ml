(* Some tests for the finite field and aes functionality.
 *
 * Copyright (c) 2013, David Rufino <david.rufino@gmail.com>
 * All rights reserved. See LICENSE for details.
 *)
open Array
open Utils
open GF2n
open Aes
open Printf

let dbg s =
  (print_string s;
  flush stdout;)

let main3 _ =
  dbg "[*] Performing tower field tests\n";
  let module F = GF28 in
  let module Base = GF24 in
  let of_int x = F.of_int x in
  (* let ( *** ) = F.( *** ) in *)
  let ( ** ) = F.( ** ) in
  let ( ++ ) = F.( ++ ) in
  let ( == ) = F.( == ) in
  dbg "  [-] Comparison check\n";
  for i = 0 to 255 do
    for j = 0 to 255 do
      let x = of_int i in
      let y = of_int j in 
      let test = (bxor (i = j) (x == y)) in
      if not test then ()
      else (dbg "epic fail";)
    done
  done;
  dbg "  [-] Inverse check \n";
  for i = 1 to 255 do
      let z = of_int i in
      let zinv = F.inv z in
      let zzinv = (z ** zinv) in
      let test = (zzinv == F.one) in
      if test then ()
      else (dbg (Printf.sprintf "%s -> %s" (F.to_string z) (F.to_string zinv));)
  done;
  dbg "  [-] Tower inverse check\n";
  for i = 1 to 255 do
    let z = of_int i in
    let zinv = F.inv2 z in
    let zzinv = (z ** zinv) in
    let test = (zzinv == F.one) in
    if test then ()
    else (dbg "failed\n";)
  done;
  dbg "  [-] Check homomorphism\n";
  let phi = Aes.phi in
  for i = 0 to 255 do
    for j = 0 to 255 do
      let x = GFAES.of_int i in
      let y = GFAES.of_int j in
      let phi_x = phi x in
      let phi_y = phi y in
      let phi_xy = phi (GFAES.( ++ ) x y) in
      let phi_x_phi_y = (phi_x ++ phi_y) in
      let test = (phi_x_phi_y == phi_xy) in
      if test then ()
      else (dbg "failed\n";)
    done
  done;
  dbg "  [-] Check homomorphism multiplication\n";
  for i = 0 to 255 do
    for j = 0 to 255 do
      let x = GFAES.of_int i in
      let y = GFAES.of_int j in
      let phi_x = phi x in
      let phi_y = phi y in
      let phi_xy = phi (GFAES.( ** ) x y) in
      let phi_x_phi_y = (phi_x ** phi_y) in
      let test = (phi_x_phi_y == phi_xy) in
      if test then ()
      else (Printf.printf "%s x %s\n" (GFAES.to_string x) (GFAES.to_string y);)
    done
  done;
  dbg "  [-] check inverse homomorphism\n";
  for i = 0 to 255 do
    let x = GF28.of_int i in
    let y = inv_phi x in
    let x_ = (phi y) in
    let test = (GF28.(==) x x_) in
    if test then ()
    else (Printf.printf " %.2x\n" i; flush stdout;)
  done; 
  dbg "  [-] check inverse homomorphism\n";
  for i = 0 to 255 do
    let x = GFAES.of_int i in
    let y = phi x in
    let x_ = (inv_phi y) in
    let test = (GFAES.(==) x x_) in
    if test then ()
    else (Printf.printf " %.2x\n" i; flush stdout;)
  done; 
  dbg "  [-] check can conjugate inverse\n";
  for i = 0 to 255 do
    let x = GFAES.of_int i in
    let xinv = GFAES.inv x in
    let xinv_ = inv_phi (GF28.inv (phi x)) in
    let test = (GFAES.(==) xinv xinv_) in
    if test then()
    else (dbg "failed\n";)
  done;
  dbg "  [-] Associativity check \n";
  for i = 0 to 80 do
    for j = 0 to 80 do
      for k = 0 to 80 do
        let w = of_int i in
        let x = of_int j in
        let y = of_int k in
        let a1 = w ** (x ++ y) in
        let a2 = (w ** x) ++ (w ** y) in
        let test = (a1 == a2) in
        if test then ()
        else (dbg "epic fail";)
      done
    done
  done;
 
  ;;
    
let main2 _ =
  dbg "[*] Performing GF(2^4) field tests\n";
  let module F = GF24 in
  dbg "  [-] Power check\n";
  for i = 0 to 15 do
    for n = 0 to 30 do
      for m = 0 to 30 do
        let f = F.of_int i in
        let fn = F.( *** ) f n in
        let fm = F.( *** ) f m in
        let fnm = F.( *** ) f (n+m) in
        let fnm2 = F.( ** ) fn fm in
        let test = (F.( == ) fnm fnm2) in
        if test then ()
        else (dbg "epic fail";)
      done 
    done
  done;
  dbg "  [-] Inverse check\n";
  for i = 1 to 15 do
    let f = F.of_int i in
    let ffinv = F.( ** ) f (F.inv f) in
    let test = (F.( == ) ffinv F.one) in
    if test then ()
    else (dbg "epic_fail\n";)
  done;
  dbg "  [-] Identity Check\n";
  for i = 1 to 15 do
    let f = F.of_int i in
    let f1 = F.( ** ) f F.one in
    let test = (F.( == ) f f1) in
    if test then ()
    else (dbg "epic fail\n";)
  done;
  dbg "  [-] Associativity check\n";
  for i = 0 to 15 do
    for j = 0 to 15 do
      for k = 0 to 15 do
        let f = F.of_int i in
        let g = F.of_int j in
        let h = F.of_int k in
        let a1 = F.( ** ) f (F.(++) g h) in
        let a2 = F.( ++ ) (F.( ** ) f g) (F.( ** ) f h) in
        let test = (F.( == ) a1 a2) in
        if test then ()
        else (dbg "epic fail";)
      done
    done
  done;
  dbg "  [-] Commutativity check\n";
  for i = 0 to 15 do
    for j = 0 to 15 do
      let f = F.of_int i in
      let g = F.of_int j in 
      let a1 = F.( ** ) f g in
      let a2 = F.( ** ) g f in
      let test = (F.( == ) a1 a2) in
      if test then ()
      else (dbg "epic fail";)
    done
  done;
  for i = 0 to 15 do
    for j = 0 to 15 do
      let f = F.of_int i in
      let g = F.of_int j in 
      let a1 = F.( ++ ) f g in
      let a2 = F.( ++ ) g f in
      let test = (F.( == ) a1 a2) in
      if test then ()
      else (dbg "epic fail";)
    done
  done
  ;;

let main _ =
  dbg "[*] Performing AES field tests\n";
  let module F = GFAES in
  let ( == ) = F.( == ) in
  dbg "  [-] Inverse check\n";
  for i = 1 to 255 do
    let f = F.of_int i in
    let ffinv = F.( ** ) f (F.inv f) in
    let test = (F.( == ) ffinv F.one) in
    if test then ()
    else (dbg "epic_fail";)
  done;
  dbg "  [-] Identity Check\n";
  for i = 1 to 255 do
    let f = F.of_int i in
    let f1 = F.( ** ) f F.one in
    let test = (F.( == ) f f1) in
    if test then ()
    else (dbg "epic fail";)
  done;
  dbg "  [-] Binary conversion\n";
  for i = 1 to 255 do
    let f = F.of_int i in
    let j = F.to_int f in
    let test = (j = i) in
    if test then ()
    else (dbg "epic fail\n";)
  done;
  dbg "  [-] Associativity check\n";
  (*
  for i = 0 to 255 do
    for j = 0 to 255 do
      for k = 0 to 255 do
        let f = F.of_int i in
        let g = F.of_int j in
        let h = F.of_int k in
        let a1 = F.( ** ) f (F.(++) g h) in
        let a2 = F.( ++ ) (F.( ** ) f g) (F.( ** ) f h) in
        let test = (F.( == ) a1 a2) in
        if test then ()
        else (dbg "epic fail";)
      done
    done
  done;
  *)
  dbg "  [-] Commutativity check\n";
  for i = 0 to 255 do
    for j = 0 to 255 do
      let f = F.of_int i in
      let g = F.of_int j in 
      let a1 = F.( ** ) f g in
      let a2 = F.( ** ) g f in
      let test = (F.( == ) a1 a2) in
      if test then ()
      else (dbg "epic fail";)
    done
  done;
  for i = 0 to 255 do
    for j = 0 to 255 do
      let f = F.of_int i in
      let g = F.of_int j in 
      let a1 = F.( ++ ) f g in
      let a2 = F.( ++ ) g f in
      let test = (F.( == ) a1 a2) in
      if test then ()
      else (dbg "epic fail";)
    done
  done;
  dbg "  [-] Power check\n";
  let x = F.of_int 0x2 in
  let x8 = F.( *** ) x 8 in
  (* 1 + x + x^3 + x^4 *)
  let x8_ = F.of_int 0x1b in
  let test = x8_ == x8 in
  if test then ()
  else (Printf.printf "X^8 != %s\n" (F.to_string x8_);)
  ;
  for i = 0 to 30 do
    for n = 0 to 30 do
      for m = 0 to 30 do
        let f = F.of_int i in
        let fn = F.( *** ) f n in
        let fm = F.( *** ) f m in
        let fnm = F.( *** ) f (n+m) in
        let fnm2 = F.( ** ) fn fm in
        let test = (F.( == ) fnm fnm2) in
        if test then ()
        else (dbg "epic fail";)
      done 
    done
  done;
  dbg "  [-] Check S-Box is self-inverse\n";
  for i = 0 to 255 do
    let i_ = GFAES.to_int (sbox (invsbox (GFAES.of_int i))) in
    let i__ = GFAES.to_int (invsbox (sbox (GFAES.of_int i))) in 
    (* check sbox is self-inverse *)
    if (i = i_) && (i = i__) then ()
    else (dbg "epic fail";)
  done;
  ;;

main () ;;
main2 () ;;
main3 () ;;
