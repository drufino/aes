/* Constant time implementation of AES encryption algorithm using 
 *
 * Hamburg, Mike "Accelerating AES with vector permute instructions"
 * http://shiftleft.org/papers/vector_aes/vector_aes.pdf
 *
 * Copyright (c) 2013, David Rufino <david.rufino@gmail.com>
 * All rights reserved. See LICENSE for details.
 */
  
#include "aes_ssse3.h"
#include "aes.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "sse_helpers.h"

// Some helper functions
unsigned char   convert_to_tower_basis_b(unsigned char byte);
unsigned char   convert_to_polynomial_basis_b(unsigned char byte);
__m128i         aes_inv_mix_columns(__m128i x);

#define Nb          4  // BlockSize / 4

#define AES256_Nr 14   // Number of rounds for AES-256
#define AES192_Nr 12   // Number of rounds for AES-192
#define AES128_Nr 10   // Number of rounds for AES-128

typedef unsigned char uint8_t;
typedef unsigned int uint32_t;

// binary representation of minimal polynomial m(x) (excluding leading term)
static unsigned char const aes_min_poly = 0x1b;

// multiply by '00000010' (i.e. X)
#define mult2(x) ((x)<<1)^(((x)>>7)*aes_min_poly)
// multiply by '00000011' (i.e.X+1)
#define mult3(x) mult2((x))^(x)

unsigned char aes_sbox[256] ={0x0};

// Lookup table for SubBytes transformation
#define SubByte(x) aes_sbox[(x)]

// The affine part of the SubBytes transformation
#define affine(x) 0x63 ^ x ^ ((x)<<1) ^((x)<<2)^((x)<<3)^((x)<<4)^((x)>>7)^((x)>>6)^((x)>>5)^((x)>>4)

void aes_init_tables()
{
    // Initialize the sbox by noting that 3 is a primitive root over GF(2^8)
    unsigned char log[256];
    unsigned char exp[256];

    unsigned i = 0;
    unsigned char c = 0x1;
    for (i = 0; i <256; ++i)
    {
        log[c] = (unsigned char)i;
        exp[i] = c;
        c = mult3(c);
    }

    aes_sbox[0] = 0x63;
    for (c = 1; c != 0; ++c)
    {
        aes_sbox[c] = exp[255-log[c]];
        aes_sbox[c] = affine(aes_sbox[c]);
    }
}

unsigned aes_rounds(size_t key_sz)
{
    switch (key_sz)
    {
    case 256:
    case 32:
        return AES256_Nr;
    case 192:
    case 24:
        return AES192_Nr;
    case 128:
    case 16:
        return AES128_Nr;
    default:
        return -1;
    }
}

// Follows (4.3.1 [1])
void aes_key_expansion_ssse3(
    size_t              key_sz,
    unsigned char *     key_expansion,
    unsigned char const*key,
    int const           direction
)
{
    // Initialize the AES tables
    if (aes_sbox[0] != 0x63) aes_init_tables();

    // Work out the number of rounds
    unsigned Nr = aes_rounds(key_sz);
    if (Nr == -1) return;

    // Convert key_sz into number of bytes, rather than bits
    if (key_sz > 32) key_sz /= 8;

    // Number of words, matches up with the AES specification
    unsigned Nk = key_sz / 4;

    // First round is just the key
    memcpy(key_expansion,key,Nk*4);

    uint8_t temp[4];
    uint8_t rc=1;
    unsigned i = Nk;
    for (i = Nk; i < Nb * (Nr+1); ++i)
    {
        ((uint32_t*)temp)[0] = ((uint32_t *)key_expansion)[i-1];
        
        if (i % Nk == 0)
        {
            // temp = SubByte(RotByte(temp)) ^ Rcon[i / Nk]
            uint8_t x = SubByte(temp[1]);
            temp[1] = SubByte(temp[2]);
            temp[2] = SubByte(temp[3]);
            temp[3] = SubByte(temp[0]);
            temp[0] = x ^ rc;
            rc = mult2(rc);
        }
        else if ((Nk > 6) && ((i % Nk) == 4))
        {
            temp[0] = SubByte(temp[0]);
            temp[1] = SubByte(temp[1]);
            temp[2] = SubByte(temp[2]);
            temp[3] = SubByte(temp[3]);
        }
        
        ((uint32_t *)key_expansion)[i] = *(uint32_t*)temp ^ ((uint32_t *)key_expansion)[i-Nk];
    }

    // convert to tower basis
    for (i = 0; i < 4*Nb*(Nr+1); ++i)
    {
        key_expansion[i] = convert_to_tower_basis_b(key_expansion[i]);
    }

    // reverse key expansion and apply inverse mix columns transformation when decrypting
    if (direction < 0)
    {
        uint32_t * keyw = (uint32_t *)key_expansion;
        unsigned j;
        for (i = 0,j=(4*Nr); i < j; i+= 4, j -= 4)
        {
            uint32_t temp;
            #define swap(x,y) temp=(x);(x)=(y);(y)=temp;
            swap(keyw[i  ],keyw[j  ]);
            swap(keyw[i+1],keyw[j+1]);
            swap(keyw[i+2],keyw[j+2]);
            swap(keyw[i+3],keyw[j+3]);
            #undef swap
        }

        // Apply InvMixColumns to all but first and last rounds
        for (i = 1; i < Nr; ++i)
        {
            __m128i x = loadu(key_expansion+i*16);
            x = aes_inv_mix_columns(x);
            storeu(key_expansion+i*16,x);
        }
    }
}

// Convert from tower representation to polynomial basis
static const unsigned char basis_lo_table3[16] = {0x00,0xa2,0xdb,0x79,0x63,0xc1,0xb8,0x1a,0xd9,0x7b,0x02,0xa0,0xba,0x18,0x61,0xc3};
static const unsigned char basis_hi_table3[16] = {0x00,0xa3,0x8b,0x28,0xd3,0x70,0x58,0xfb,0xd5,0x76,0x5e,0xfd,0x06,0xa5,0x8d,0x2e};


// Maps AES polynomial basis to the tower representation GF(2^8) as twisted sum of GF(2^4)
static const unsigned char basis_lo_table1[16] = {0x00,0x11,0x0a,0x1b,0xca,0xdb,0xc0,0xd1,0x42,0x53,0x48,0x59,0x88,0x99,0x82,0x93};
static const unsigned char basis_hi_table1[16] = {0x00,0x4f,0x72,0x3d,0x6d,0x22,0x1f,0x50,0x79,0x36,0x0b,0x44,0x14,0x5b,0x66,0x29};

unsigned char convert_to_tower_basis_b(unsigned char byte)
{
    return basis_lo_table1[byte & 0x0f] ^ basis_hi_table1[byte>>4];
}

unsigned char convert_to_polynomial_basis_b(unsigned char byte)
{
    return basis_lo_table3[byte & 0x0f] ^ basis_hi_table3[byte>>4];  
}

__m128i convert_to_tower_basis(__m128i data)
{
    return generic_shuffle(data,basis_lo_table1,basis_hi_table1);
}

__m128i convert_to_polynomial_basis(__m128i data)
{
    return generic_shuffle(data,basis_lo_table3,basis_hi_table3);
}


// PSHUFB lookups corresponding to 1/x and \zeta / x in GF(2^4) respectively. NB 0x00 -> 0x80 
static const unsigned char inv_lookup_table[16] = {0x80,0x01,0x0f,0x0a,0x08,0x06,0x05,0x09,0x04,0x07,0x03,0x0e,0x0d,0x0c,0x0b,0x02};
static const unsigned char inv_lookup_table2[16] = {0x80,0x0f,0x08,0x05,0x04,0x03,0x0d,0x0b,0x02,0x0c,0x0e,0x07,0x09,0x06,0x0a,0x01};

// tower basis multiplication by two
static const unsigned char two_lookup_table_lo[] = {0x00,0xb1,0x92,0x23,0xd4,0x65,0x46,0xf7,0x58,0xe9,0xca,0x7b,0x8c,0x3d,0x1e,0xaf};
static const unsigned char two_lookup_table_hi[] = {0x00,0xbb,0x99,0x22,0xdd,0x66,0x44,0xff,0x55,0xee,0xcc,0x77,0x88,0x33,0x11,0xaa};
static const unsigned char four_lookup_table_lo[] = {0x00,0xc6,0x7c,0xba,0xe7,0x21,0x9b,0x5d,0x3e,0xf8,0x42,0x84,0xd9,0x1f,0xa5,0x63};
static const unsigned char four_lookup_table_hi[] = {0x00,0x0c,0x07,0x0b,0x0e,0x02,0x09,0x05,0x03,0x0f,0x04,0x08,0x0d,0x01,0x0a,0x06};
static const unsigned char nine_lookup_table_lo[] = {0x00,0xcf,0x71,0xbe,0xe2,0x2d,0x93,0x5c,0x34,0xfb,0x45,0x8a,0xd6,0x19,0xa7,0x68};
static const unsigned char nine_lookup_table_hi[] = {0x00,0x9c,0xd7,0x4b,0x5e,0xc2,0x89,0x15,0xa3,0x3f,0x74,0xe8,0xfd,0x61,0x2a,0xb6};
const unsigned char Ptable_[]={1,2,3,0,5,6,7,4,9,10,11,8,13,14,15,12};
const unsigned char P2table_[]={2,3,0,1,6,7,4,5,10,11,8,9,14,15,12,13};
const unsigned char P3table_[]={3,0,1,2,7,4,5,6,11,8,9,10,15,12,13,14};
static const unsigned char _inv_shift_row_table[16] = {0x00,0x0d,0x0a,0x07,0x04,0x01,0x0e,0x0b,0x08,0x05,0x02,0x0f,12,0x09,0x06,0x03};


// InvMixColumns

// Circulant matrix, as in the encryption round, but with coefficients
//    (14  11  13   9)
// expressed as elements of GF(2^8). Can be written mildly simpler as
//
// (I+P^2)[(I+P).9 + I.4] + (I+P).2+I
//
// where P is an elementary circulant matrix
__m128i aes_inv_mix_columns(
    __m128i                        x                    // (I) State
)
{
    __m128i mask = _mm_set1_epi8(0x0f);

    __m128i x_lo = and(mask,x);
    __m128i x_hi = srl(nand(mask,x),4);

    __m128i four_x  =
        xor(
            shuffle(load(four_lookup_table_lo),x_lo),
            shuffle(load(four_lookup_table_hi),x_hi)
        );
    __m128i two_x   =
        xor(
            shuffle(load(two_lookup_table_lo),x_lo),
            shuffle(load(two_lookup_table_hi),x_hi)
        );
    __m128i nine_x  =
        xor(
            shuffle(load(nine_lookup_table_lo),x_lo),
            shuffle(load(nine_lookup_table_hi),x_hi)
        );

    // Circulant matrices on the state
    #define P(x)    shuffle(x,load(Ptable_))
    #define P2(x)   shuffle(x,load(P2table_))

    nine_x = xor(nine_x,P(nine_x));
    two_x  = xor(two_x, P(two_x));
    nine_x = xor(nine_x,four_x);
    nine_x = xor(nine_x,P2(nine_x));
    x      = xor(x,xor(two_x,nine_x));

    #undef P
    #undef P2
    return x;
}

__m128i aes_inv_round(
    __m128i                        x,                   // (I) State      
    __m128i                        key_expansion,       // (I) Key Expansion
    int                            final                // (I) Flag indicating it's the final round
)
{


    __m128i mask = _mm_set1_epi8(0x0f);


    // Inverse Affine Map
    {
        __m128i x_lo = and(mask,x);
        __m128i x_hi = srl(nand(mask,x),4);

        // Lookup tables in tower basis
        static unsigned char basis_lo_table2[16] = {0xdb,0xb0,0x7e,0x15,0x00,0x6b,0xa5,0xce,0x82,0xe9,0x27,0x4c,0x59,0x32,0xfc,0x97};
        static unsigned char basis_hi_table2[16] = {0x00,0x4e,0x58,0x16,0x8d,0xc3,0xd5,0x9b,0x50,0x1e,0x08,0x46,0xdd,0x93,0x85,0xcb};

        x = xor(shuffle(load(basis_lo_table2),x_lo),shuffle(load(basis_hi_table2),x_hi));
    }

    // Prepare for the mix columns stage
    __m128i two_x, four_x, nine_x;

    // Inversion in GF(2^8) over tower basis
    {
        __m128i inv_lookup = load(inv_lookup_table); 
        __m128i inv_lookup2 = load(inv_lookup_table2); 
        {
            // Do the nested inversion algorithm 
            __m128i y = srl(nand(mask,x),4);
            __m128i inv_y = shuffle(inv_lookup,y);
            x = and(mask,x);
            __m128i inv_xpy = shuffle(inv_lookup2,xor(x,y));

            inv_y = shuffle(inv_lookup, xor(inv_y,inv_xpy));
          
            __m128i inv_x = shuffle(inv_lookup,x);
            inv_x = shuffle(inv_lookup, xor(inv_x,inv_xpy));

            inv_x = xor(inv_x, y);
            inv_y = xor(inv_y, x);

            // PSHUFB lookups for (t+\zeta)/x and (\bar{t} + \zeta)/x resp.
            static unsigned char inv_lookup_table5[16] = {0x00,0x23,0x1e,0xb1,0xf7,0xca,0xaf,0xd4,0x8c,0xe9,0x65,0x3d,0x58,0x7b,0x92,0x46};
            static unsigned char inv_lookup_table6[16] = {0x00,0x32,0xe1,0x1b,0x7f,0xac,0xfa,0x4d,0xc8,0x9e,0x56,0xd3,0x85,0xb7,0x29,0x64};

            inv_lookup = load(inv_lookup_table5);
            inv_lookup2 = load(inv_lookup_table6); 
            x =
            xor(
                shuffle(inv_lookup2, inv_x),
                shuffle(inv_lookup,  inv_y)
            );
            if (!final)
            {
                // Multiply by two
                static unsigned char inv_lookup_table7[16] = {0x00,0xba,0xa5,0xc6,0x5d,0x42,0x63,0xe7,0xd9,0xf8,0x21,0x1f,0x3e,0x84,0x7c,0x9b};
                static unsigned char inv_lookup_table8[16] = {0x00,0xb0,0xa0,0xc0,0x50,0x40,0x60,0xe0,0xd0,0xf0,0x20,0x10,0x30,0x80,0x70,0x90};
                inv_lookup =    load(inv_lookup_table7);
                inv_lookup2=    load(inv_lookup_table8);
                two_x = 
                xor(
                    shuffle(inv_lookup2,    inv_x),
                    shuffle(inv_lookup,     inv_y)
                );

                // Multiply by four
                static unsigned char inv_lookup_table9[16] = {0x00,0xbd,0xa9,0xce,0x5b,0x4f,0x67,0xe6,0xda,0xf2,0x28,0x14,0x3c,0x81,0x73,0x95};
                static unsigned char inv_lookup_table10[16] = {0x00,0x77,0xcc,0x88,0x66,0xdd,0x44,0x11,0x33,0xaa,0x99,0xbb,0x22,0x55,0xff,0xee};
                inv_lookup =    load(inv_lookup_table9);
                inv_lookup2=    load(inv_lookup_table10);
                four_x = 
                xor(
                    shuffle(inv_lookup2,    inv_x),
                    shuffle(inv_lookup,     inv_y)
                );
                // Multiply by nine
                static unsigned char inv_lookup_table11[16] = {0x00,0x69,0x3b,0x27,0xea,0xb8,0x1c,0x83,0x75,0xd1,0xa4,0x52,0xf6,0x9f,0x4e,0xcd};
                static unsigned char inv_lookup_table12[16] = {0x00,0x3a,0xe5,0x16,0x7d,0xa2,0xf3,0x47,0xc9,0x98,0x51,0xdf,0x8e,0xb4,0x2c,0x6b};
                inv_lookup =    load(inv_lookup_table11);
                inv_lookup2=    load(inv_lookup_table12);
                nine_x = 
                xor(
                    shuffle(inv_lookup2,    inv_x),
                    shuffle(inv_lookup,     inv_y)
                );
            }
        }
    }

    // AddRoundKey
    if (final)
    {
        x = xor(x, key_expansion);
        return x;
    }

    // InvMixColumns

    // Circulant matrix, as in the encryption round, but with coefficients
    //    (14  11  13   9)
    // expressed as elements of GF(2^8). Can be written mildly simpler as
    //
    // (I+P^2)[(I+P).9 + I.4] + (I+P).2+I
    mask = load(Ptable_);
    #define P(x) shuffle(x,mask)
    nine_x = xor(nine_x,P(nine_x));
    two_x  = xor(two_x, P(two_x));   
    #undef P
    nine_x = xor(nine_x,four_x);   
    mask   = load(P2table_);
    #define P2(x) shuffle(x,mask)
    nine_x = xor(nine_x,P2(nine_x));
    #undef P2

    x      = xor(xor(x,two_x),nine_x);

    // AddRoundKey - have already applied invmixcolumns in the key expansion
    x      = xor(x, key_expansion);

    // InvShiftRows
    mask = load(_inv_shift_row_table);
    x = shuffle(x,mask);

    return x;
}

__m128i aes_round(
    __m128i                        state_reg,      // (I) State
    __m128i                        key_expansion,
    int                            final
)
{
    __m128i mask = _mm_set1_epi8(0x0f);

        // ByteSub
    __m128i state_reg_2;
    {
        __m128i inv_lookup = load(inv_lookup_table); 
        __m128i inv_lookup2 = load(inv_lookup_table2); 
        {
            // Do the nested inversion algorithm 
            __m128i y = srl(nand(mask,state_reg),4);
            __m128i inv_y = shuffle(inv_lookup,y);
            __m128i x = and(mask,state_reg);
            __m128i inv_xpy = shuffle(inv_lookup2,xor(x,y));

            inv_y = shuffle(inv_lookup, xor(inv_y,inv_xpy));
          
            __m128i inv_x = shuffle(inv_lookup,x);
            inv_x = shuffle(inv_lookup, xor(inv_x,inv_xpy));

            inv_x = xor(inv_x, y);
            inv_y = xor(inv_y, x);

            // PSHUFB lookups for (t+\zeta)/x and (\bar{t} + \zeta)/x resp.
            // Also baked in is the linear part of the forward affine transformation
            // NB GF(2^4) -> GF(2^8) resp.
            static const unsigned char inv_lookup_table3[16] = {0x00,0xd5,0x90,0x2d,0x9d,0xd8,0xbd,0x48,0x68,0x0d,0x65,0x45,0x20,0xf5,0xf8,0xb0};
            static const unsigned char inv_lookup_table4[16] = {0x00,0x09,0xad,0x63,0x2e,0x8a,0xce,0x27,0xc7,0x83,0x44,0xa4,0xe0,0xe9,0x6a,0x4d};

            // Final step including the affine map 
            // NB 0x04 is 0x63 in the tower basis
            inv_lookup = load(inv_lookup_table3);
            inv_lookup2 = load(inv_lookup_table4); 
            state_reg = xor(
                xor(
                    shuffle(inv_lookup2, inv_x),
                    shuffle(inv_lookup,  inv_y)
                ),
                _mm_set1_epi8(0x04)
            );


            // Also include state_reg * 2, for use in mix columns
            // NB 0xd4 is 2 * 0x63 in the tower basis
            if (!final)
            {
                static const unsigned char inv_lookup_table3b[16] = {0x00,0x56,0xee,0xa4,0xd3,0x6b,0x4a,0x85,0x1c,0x3d,0x21,0xb8,0x99,0xcf,0xf2,0x77};
                static const unsigned char inv_lookup_table4b[16] = {0x00,0xe9,0xf1,0x67,0x87,0x9f,0x96,0x6e,0x7f,0x76,0x09,0x18,0x11,0xf8,0x8e,0xe0};

                inv_lookup  = load(inv_lookup_table3b);
                inv_lookup2 = load(inv_lookup_table4b);

                state_reg_2 =
                xor(
                    xor(
                        shuffle(inv_lookup2, inv_x),
                        shuffle(inv_lookup,  inv_y)
                    ),
                    _mm_set1_epi8(0xd4)
                );    
            }
        } 
    }

    static const unsigned char _shift_row_table[16] = {0x00,0x05,0x0a,0x0f,0x04,0x09,0x0e,0x03,0x08,0x0d,0x02,0x07,0x0c,0x01,0x06,0x0b};
    // Shift Rows and Mix Columns
    if (final)
    {
        state_reg = shuffle(state_reg,load(_shift_row_table));
    }
    else
    {
        // Shift Rows
        {
            __m128i shift_row_table = load(_shift_row_table);
            state_reg = shuffle(state_reg,shift_row_table);
            state_reg_2 = shuffle(state_reg_2,shift_row_table);
        }

        // mix columns corresponds to circulant matrix with coefficients
        //      (2, 3, 1, 1)
        // where numbers are binary representations of GF(2^8) elements
        //   (i.e. 2 = X , 3 = X+1, 1 = 1 )
        // this equals
        //       2 I + 3 P + P^2 + P^3
        //     = (I+P)(I.2 + P)+ P^3
        //
        // where P is circulant matrix
        //
        //    ( 0 1 0 0 )
        //    ( 0 0 1 0 )
        //    ( 0 0 0 1 )
        //    ( 1 0 0 0 )
        //

        #define P(x) shuffle(x,Ptable)
        #define P3(x) shuffle(x,load(P3table_))
        __m128i Ptable = load(Ptable_);
        __m128i y = xor(P(state_reg),state_reg_2);
        state_reg = xor(xor(P(y),y),P3(state_reg));
        #undef P
        #undef P3
    }

    return xor(state_reg,key_expansion);
}

// Descrypt a single block
void aes_decrypt_ssse3(
    size_t                      key_sz,         // (I) Key Size
    unsigned char const*        key_expansion,  // (I) Key Expansion
    unsigned char *             plaintext,      // (O) Plaintext (16-Bytes)
    unsigned char const *       ciphertext      // (I) Cipher Block (16-Bytes)
)
{
    unsigned Nr = aes_rounds(key_sz);
    if (Nr == -1) return;

    // Load the state
    __m128i state = loadu(ciphertext);

    // Work in a different basis
    state = convert_to_tower_basis(state);

    // AddRoundKey
    state = xor(state,load(key_expansion)); key_expansion += Nb*4;

    // InvShiftRows
    // This is here so that the InvRound has InvMix after InvSub
    state = shuffle(state,load(_inv_shift_row_table));

    int i;
    for (i = Nr; i >= 1; --i,key_expansion += Nb*4)
    {
        state = aes_inv_round(state,load(key_expansion),(i==1)?1:0);
    }

    state = convert_to_polynomial_basis(state);

    storeu(plaintext,state);
}


// Encryption single block
void aes_encrypt_ssse3(
    size_t                      key_sz,         // (I) Key Size
    unsigned char const*        key_expansion,  // (I) Key Expansion
    unsigned char const*        plaintext,      // (I) Plaintext (16-Bytes)
    unsigned char *             ciphertext      // (O) Cipher Block (16-Bytes)
)
{
    unsigned Nr = aes_rounds(key_sz);
    if (Nr == -1) return;

    __m128i state = loadu(plaintext);

    // Work in different basis 
    state = convert_to_tower_basis(state);

    // AddRoundKey
    state = xor(state,load(key_expansion)); key_expansion += Nb*4;

    unsigned i = 1;
    for (i = 1; i <= Nr; ++i,key_expansion += Nb*4)
    {
        state = aes_round(state,load(key_expansion),(i==Nr) ? 1 : 0);
    }

    // Convert back to standard basis
    state = convert_to_polynomial_basis(state);

    storeu(ciphertext,state);
}
