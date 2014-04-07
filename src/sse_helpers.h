#ifndef sse_helpers_h
#define sse_helpers_h

/* Some helpers macros and functions for using SSE intrinsics
 *
 * Copyright (c) 2013, David Rufino <david.rufino@gmail.com>
 * All rights reserved. See LICENSE for details.
 */


#include <tmmintrin.h> // SSSE3

#define shuffle(x,y) _mm_shuffle_epi8(x,y)
#define xor(x,y) _mm_xor_si128(x,y)
#define or(x,y) _mm_or_si128(x,y)
#define not(x) _mm_not_si128(x);
#define nand(x,y) _mm_andnot_si128(x,y)
#define and(x,y) _mm_and_si128(x,y)
#define srl(x,y) _mm_srli_epi32((x),(y))
#define store(p,x) _mm_store_si128((__m128i *)(p),x)
#define storeu(p,x) _mm_storeu_si128((__m128i *)(p),x)
#define load(p) _mm_load_si128((__m128i *)(p))
#define loadu(p) _mm_loadu_si128((__m128i *)(p))

#ifdef __cplusplus
extern "C" {
#endif

#ifndef tf_inline
#define tf_inline __inline__
#endif

__m128i tf_inline generic_shuffle(__m128i data, unsigned char const *lo_table, unsigned char const *hi_table)
{
    __m128i mask = _mm_set1_epi8(0x0f);
    __m128i basis_lo = _mm_load_si128((__m128i *)lo_table);
    __m128i basis_hi = _mm_load_si128((__m128i *)hi_table);

    __m128i tmp = shuffle(basis_lo,and(mask,data));
    __m128i tmp2 = srl(nand(mask,data),4);
    data = shuffle(basis_hi,tmp2);
    data = xor(data,tmp);
    return data; 
}

#undef tf_inline

#ifdef __cplusplus
}
#endif

#endif /* sse_helpers_h */