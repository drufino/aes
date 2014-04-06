/* Demonstration of the constant-time AES S-Box calculation described in
 *
 * Hamburg, Mike "Accelerating AES with vector permute instructions"
 * http://shiftleft.org/papers/vector_aes/vector_aes.pdf
 *
 * Copyright (c) 2013, David Rufino <david.rufino@gmail.com>
 * All rights reserved. See LICENSE for details.
 */

#include <cstdio>
#include <iostream>
#include <fstream>
#include <string.h>
#include <stdlib.h>
 #include <vector>
#include "aes.h"

void print_hex(unsigned char const *p, int len)
{
    for (int i=len-1;i>=0;--i)
    {
        printf("%.2x",p[i]);
    }
} 

bool is_hex(char p)
{
    if (p >= 'a' && p <= 'f')
        return true;
    if (p >= 'A' && p <= 'F')
        return true;
    if (p >= '0' && p <= '9')
        return true;
    return false;
}

//XXX wrong order
size_t convert_from_hex(unsigned char *out, char const *in)
{
    unsigned numdigits = 0;
    {
        char const *p = in;
        while (is_hex(*p))
        {
            ++p;
            ++numdigits;
        }
    }

    unsigned n = numdigits/2;
    char hex_byte[3];
    for (unsigned i = 0; i < n; ++i)
    {
        hex_byte[2] = '\0';
        hex_byte[0] = *in++;
        hex_byte[1] = *in++;
        out[i] = (unsigned char)(strtoul(hex_byte,NULL,16)&0xff);
    }

    return n;
}

bool aes_enc_test(
    bool                    bQuiet,
    size_t                  key_sz,
    unsigned char const *   key,
    unsigned char const *   pt,
    unsigned char const *   ct
)
{
    unsigned char key_expansion[(14+1)*4*4];
    unsigned char key_expansion_dec[(14+1)*4*4];

    aes_key_expansion(
        key_sz,
        key_expansion,
        key,
        1
    );

    aes_key_expansion(
        key_sz,
        key_expansion_dec,
        key,
        -1
    );

    unsigned char ct_test[16];
    aes_encrypt(
        key_sz,
        key_expansion,
        pt,
        ct_test
    );

    unsigned char pt_test[16];
    aes_decrypt(
      key_sz,
      key_expansion_dec,
      pt_test,
      ct_test
    );

    bool bEqual(false);

    bEqual = !memcmp(ct,ct_test,16) && !memcmp(pt,pt_test,16);
    if (!bEqual || !bQuiet)
    {
        printf("\nKey:                    ");
        print_hex(key, key_sz); 
        printf("\nPlaintext:              ");
        print_hex(pt, 16);
        printf("\nOur plaintext:          ");
        print_hex(pt_test, 16);
        printf("\nCiphertext:             ");
        print_hex(ct_test,16);
        printf("\nOur ciphertext:         ");
        print_hex(ct, 16);

       printf ("\n");
    }
    return bEqual;
}

static inline uint64_t get_cycles()
{
uint64_t hi, lo;
asm volatile ("rdtsc" : "=a"(lo), "=d"(hi));
return lo | (hi << 32);
}


void aes_speed_test()
{
   unsigned int key_sz=128;//256;
   unsigned char pt[16]; memcpy(pt,"0123456789ABCDEF",16);
   unsigned char ct[16];
   unsigned char *key=(unsigned char *)"0123456789ABCDEF01234566789ABCDEF";
   unsigned char key_expansion[(14+1)*4*4];

    aes_key_expansion(
      key_sz,
      key_expansion,
      key,
      1
    );
    unsigned const nTimes=100;
    unsigned const nBlocks=100;
    uint64_t cycles_start, cycles_end;
    std::vector<uint64_t> runtimes(nTimes);
    for (unsigned j = 0; j < nTimes; ++j)
   {
    cycles_start = get_cycles();
    for (unsigned i = 0; i < nBlocks; ++i)
    {
      aes_encrypt(
        key_sz,
        key_expansion,
        pt,
        ct
      );
    }
    cycles_end = get_cycles();
    runtimes[j] = cycles_end - cycles_start;
    }
    std::sort(runtimes.begin(),runtimes.end());
    printf("AES-128 Encryption Cycles per byte: %.2f\n",double(runtimes[nTimes/2])/double(nBlocks*16));

    for (unsigned j = 0; j < nTimes; ++j)
    {
      cycles_start = get_cycles();
      for (unsigned i = 0; i < nBlocks; ++i)
      {
        aes_decrypt(
          key_sz,
          key_expansion,
          pt,
          ct
        );
      }
      cycles_end = get_cycles();
      runtimes[j] = cycles_end - cycles_start;
    }
    std::sort(runtimes.begin(), runtimes.end());
    printf("AES-128 Decryption Cycles per byte: %.2f\n",double(runtimes[nTimes/2])/double(nBlocks*16));
}

bool aes_test(char const *fn,bool bQuiet)
{
    std::ifstream in_file(fn);
    char buf[500];
    buf[499] = '\0';

    bool bPass(true);

    bool bEncrypt(true), bKey(false), bPt(false), bCt(false);
    unsigned char key[64];  size_t key_len(0);
    unsigned char pt[64];   size_t pt_len(0);
    unsigned char ct[64];   size_t ct_len(0);
    unsigned idx;
    for (;;)
    {
        buf[0] = '\0';
        in_file.getline(buf,sizeof(buf)-1,'\n');
        if (strlen(buf) == 0) break;
        if (buf[0] == '#' || buf[0] == '\r' || buf[0] == '\n')
            continue;

        if (!memcmp(buf,"[ENCRYPT]",9))
        {
            bEncrypt = true; 
        }
        else if (!memcmp(buf,"COUNT = ",8))
        {
            bPt = bCt = bKey = false;
            idx = strtoul(buf+8,NULL,10);
        }
        else if (!memcmp(buf,"KEY = ",6))
        {
            key_len = convert_from_hex(key,buf+6);
            switch (key_len*8)
            {
            case 256:
            case 192:
            case 128:
               bKey = true;
               break;
            default:
               fprintf(stderr, "Got key of length %u\n", (unsigned)key_len);    
            }
        }
        else if (!memcmp(buf,"PLAINTEXT = ",12))
        {
            ct_len = pt_len = convert_from_hex(pt,buf+12);
            if (pt_len != 16)
            {
                fprintf(stderr, "Got plaintext of length %u\n", (unsigned)pt_len);
            }
            else
            {
                bPt = true;
            }
        }
        else if (!memcmp(buf,"CIPHERTEXT = ",13))
        {
            ct_len = pt_len = convert_from_hex(ct,buf+13);
            if (ct_len != 16)
            {
                fprintf(stderr, "Got ciphertext of length %u\n", (unsigned)ct_len);
            }
            else
            {
                bCt = true;
            }
        }

        if (bKey && bPt && bCt)
        {
            if (!aes_enc_test( bQuiet, key_len, key, pt, ct))
            {
                bPass = false;
            }
        }
    }
    return bPass;
}
int main(int argc, char *argv[])
{
    if (argc < 2)
    {
        fprintf(stderr,
"  Usage: aes_test [mode] [filename]\n"
"\n"
"     mode can be one of aesecb,...\n"
"" 
       );
       return (-1);
    }
    else
    {
       if (!strcasecmp(argv[1], "aesecb"))
       {
           printf("Testing %s: \n", argv[2]);
           bool bRes = aes_test(argv[2],false);
           if (bRes)
           {
              printf("Passed\n"); return 0;
           }
           else
           {
              printf("Failed\n"); return -1;
           }
       }
       else if (!strcasecmp(argv[1], "aesecbquiet"))
       {
           bool bRes = aes_test(argv[2],true);
           if (bRes)
           {
              return 0;
           }
           else
           {
              return -1;
           }
       }
       else if (!strcasecmp(argv[1], "aesspeedtest"))
       {
           aes_speed_test();
           return 0;
       }
       else
       {
           //fprintf(stderr, "");
       }
    }

    return(0);
}
    
