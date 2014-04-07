#ifndef cpuid_h
#define cpuid_h
/* Some helper functions for x86 CPUID instructions
 *
 * Copyright (c) 2013, David Rufino <david.rufino@gmail.com>
 * All rights reserved. See LICENSE for details.
 */
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

void cpuid(
    uint32_t const  eax,        // (I) %eax register
    uint32_t        cpuid[4]    // (O) CPUID results (eax-edx)
);

// 1 if true, 0 otherwise
int cpu_supports_ssse3();

// 1 if supports AES-NI, 0 otherwise
int cpu_supports_aesni();

#ifdef __cplusplus
}
#endif

#endif