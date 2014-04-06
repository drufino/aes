/* Constant time implementation of AES encryption algorithm using 
 *
 * Hamburg, Mike "Accelerating AES with vector permute instructions"
 * http://shiftleft.org/papers/vector_aes/vector_aes.pdf
 *
 * Copyright (c) 2013, David Rufino <david.rufino@gmail.com>
 * All rights reserved. See LICENSE for details.
 */

#ifndef aes_ssse3_h 
#define aes_ssse3_h 
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

// Compute the key expansion required for encryption and decryption
void aes_key_expansion_ssse3(
    size_t                  key_sz, // (I) Key Size
    unsigned char *         ctx,    // (O) Key Expansion 4*(Nr+1)
    unsigned char const *   key,    // (I) Key
    int const               direction//(I) Direction (>=0 encryption < 0 decryption)
);

// Decryption of a single 16-byte block
void aes_decrypt_ssse3(
    size_t                      key_sz,         // (I) Key Size
    unsigned char const*        key_expansion,  // (I) Key Expansion
    unsigned char *             plaintext,      // (O) Plaintext (16-Bytes)
    unsigned char const *       ciphertext      // (I) Cipher Block (16-Bytes)
);

// Encryption single 16-byte block
void aes_encrypt_ssse3(
    size_t                      key_sz,         // (I) Key Size
    unsigned char const*        kr,             // (I) Key Expansion
    unsigned char const*        plaintext,      // (I) Plaintext (16-Bytes)
    unsigned char *             ciphertext      // (O) Cipher Block (16-Bytes)
);

#ifdef __cplusplus
}
#endif

#endif
