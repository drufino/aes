/* Public interface to AES algorithms 
 *
 * Copyright (c) 2013, David Rufino <david.rufino@gmail.com>
 * All rights reserved. See LICENSE for details.
 */
  
#include "aes.h"
#include "aes_ssse3.h"
#include "aes_simple.h"
#include "cpuid.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define Nb 4

#define AES256_Nr 14   // Number of rounds for AES-256
#define AES192_Nr 12   // Number of rounds for AES-192
#define AES128_Nr 10   // Number of rounds for AES-128

enum aes_method_t
{
    AES_DEFAULT=0,
    AES_SIMPLE=1,
    AES_SSSE3=2
};

static enum aes_method_t s_aes_method = AES_DEFAULT;

enum aes_method_t inline get_aes_method()
{
    if (s_aes_method == AES_DEFAULT)
    {
        if (cpu_supports_ssse3())
        {
            s_aes_method = AES_SSSE3;
        }
        else
        {
            s_aes_method = AES_SIMPLE;
        }
    }
    return s_aes_method;
}

// Follows (4.3.1 [1])
void aes_key_expansion(
    size_t              key_sz,
    unsigned char *     key_expansion,
    unsigned char const*key,
    int                 direction
)
{
    if (key_sz <= 32) key_sz *= 8;

    switch (get_aes_method())
    {
    default:
    case AES_SIMPLE:
        if (direction >= 0)
        {
            rijndaelKeySetupEnc(key_expansion, key, key_sz);
        }
        else
        {
            rijndaelKeySetupDec(key_expansion, key, key_sz);
        }
        break;
    case AES_SSSE3:
        aes_key_expansion_ssse3(key_sz, key_expansion, key,direction);
    }
}

size_t aes_ctx_len(size_t key_sz)
{
    unsigned Nr = aes_rounds(key_sz);
    if (Nr == -1)
    {
        return -1;
    }
    else
    {
        return 4*(Nr+1);
    }
}

// Descrypt a single block
void aes_decrypt(
    size_t                      key_sz,         // (I) Key Size
    unsigned char const*        key_expansion,  // (I) Key Expansion
    unsigned char *             plaintext,      // (O) Plaintext (16-Bytes)
    unsigned char const *       ciphertext      // (I) Cipher Block (16-Bytes)
)
{
    switch (get_aes_method())
    {
    default:
    case AES_SIMPLE:
        {
            unsigned int const Nr = aes_rounds(key_sz);
            rijndaelDecrypt(key_expansion, Nr, ciphertext, plaintext);
            break;
        }
    case AES_SSSE3:
        aes_decrypt_ssse3(key_sz, key_expansion, plaintext, ciphertext);
    }
}


// Encryption single block
void aes_encrypt(
    size_t                      key_sz,         // (I) Key Size
    unsigned char const*        key_expansion,  // (I) Key Expansion
    unsigned char const*        plaintext,      // (I) Plaintext (16-Bytes)
    unsigned char *             ciphertext      // (O) Cipher Block (16-Bytes)
)
{
    switch (get_aes_method())
    {
    default:
    case AES_SIMPLE:
        {
            unsigned int const Nr = aes_rounds(key_sz);
            rijndaelEncrypt(key_expansion, Nr, plaintext, ciphertext);
            break;
        }
    case AES_SSSE3:
        aes_encrypt_ssse3(key_sz, key_expansion, plaintext, ciphertext);
    }
}
