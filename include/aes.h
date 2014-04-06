/* Public interface to AES algorithms
 *
 * Copyright (c) 2013, David Rufino <david.rufino@gmail.com>
 * All rights reserved. See LICENSE for details.
 */

#ifndef aes_h 
#define aes_h 
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

// Get the number of rounds
unsigned aes_rounds(size_t key_sz);

// Calculate size of key expansion
size_t aes_ctx_len(size_t key_sz);

// Compute the key expansion required for encryption and decryption
void aes_key_expansion(
    size_t                  key_sz, // (I) Key Size
    unsigned char *         ctx,    // (O) Key Expansion 4*(Nr+1)
    unsigned char const *   key,    // (I) Key
    int                     direction // (I) Direction (>=0 encrypt, <0 decrypt)
);

// Decryption of a single 16-byte block
void aes_decrypt(
    size_t                      key_sz,         // (I) Key Size
    unsigned char const*        key_expansion,  // (I) Key Expansion
    unsigned char *             plaintext,      // (O) Plaintext (16-Bytes)
    unsigned char const *       ciphertext      // (I) Cipher Block (16-Bytes)
);

// Encryption single 16-byte block
void aes_encrypt(
    size_t                      key_sz,         // (I) Key Size
    unsigned char const*        kr,             // (I) Key Expansion
    unsigned char const*        plaintext,      // (I) Plaintext (16-Bytes)
    unsigned char *             ciphertext      // (O) Cipher Block (16-Bytes)
);

#ifdef __cplusplus
}
#endif

#endif
