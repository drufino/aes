libtinfoil
==========
Copyright David Rufino (c), 2013. See LICENSE for details.

It's well known that it's non-trivial to implement AES in software in a way
which is both fast and free of side-channel attacks. There are a few strategies
to achieve this

   -  Bitslice
      
   -  AES-NI instructions

      These are dedicated CPU instructions for the core AES algorithm. This is
      the preferred method, but is by no means universally available. 

   -  Vector Permute instructions
    
      Make use of SSSE3 ASM extensions. This is the subject of Hamburg, Mike "Accelerating AES with vector permute instructions"

        http://shiftleft.org/papers/vector_aes/vector_aes.pdf
        
      and we provide a simplified reusable implementation here.

The package is organised as follows

 /include  Public interface

 /src      A simple resuable implementation based on intrinsics

 /docs     Some mathematical details required to implement the methodology

 /tables   OCaml code to perform the finite field arithmetic necessary to calculate the magic constants.
