/* Some helper functions for x86 CPUID instructions
 *
 * http://en.wikipedia.org/wiki/CPUID
 * Copyright (c) 2013, David Rufino <david.rufino@gmail.com>
 * All rights reserved. See LICENSE for details.
 */

#include "cpuid.h"
#include <cpuid.h>
#include <stdio.h>

#define CPUID_FEATURE_SSE3          0x000000001
#define CPUID_FEATURE_PCLMULQDQ     0x000000002
#define CPUID_FEATURE_SSSE3         0x000000200
#define CPUID_FEATURE_SSE41         0x000080000
#define CPUID_FEATURE_SSE42         0x000100000
#define CPUID_FEATURE_AESNI         0x002000000
#define CPUID_FEATURE_AVX           0x010000000
#define CPUID_FEATURE_RDRND         0x040000000

/*
void cpuid(
    uint32_t const  eax,        // (I) %eax register
    uint32_t        cpuid[4]    // (O) CPUID results (eax-edx)
)
{
    cpuid[0] = eax;
    cpuid[1] = 0;
    cpuid[2] = 0;
    cpuid[3] = 0;
    __asm __volatile(
#ifdef __x86_64__
        "       mov  %0,  %%rdi\n"
#else
#error 32-bit not supported
#endif
        "       push      %%rbx\n"
        "       push      %%rcx\n"
        "       push      %%rdx\n"

        "       mov       (%%rdi),      %%eax\n"
        "       mov       4(%%rdi),     %%ebx\n"
        "       mov       8(%%rdi),     %%ecx\n"
        "       mov       12(%%rdi),    %%edx\n"
        "       cpuid\n"
        "       movl      %%eax,        (%%rdi)\n"
        "       movl      %%ebx,        4(%%rdi)\n"
        "       movl      %%ecx,        8(%%rdi)\n"
        "       movl      %%edx,        12(%%rdi)\n"
        "       pop       %%rdx\n"
        "       pop       %%rcx\n"
        "       pop       %%rbx\n"
        :
        : "m"(cpuid) // the input
        : "memory","eax","rdi"
    );
}
*/

int cpu_supports_ssse3()
{
    uint32_t regs[4];

    // Process Info
    __get_cpuid(1,&regs[0],&regs[1],&regs[2],&regs[3]);

    // Check ECX bits
    if (regs[1] & (CPUID_FEATURE_SSSE3|CPUID_FEATURE_SSE42|CPUID_FEATURE_SSE41))
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

int cpu_supports_aesni()
{
    uint32_t regs[4];

    // Process Info
    __get_cpuid(1,&regs[0],&regs[1],&regs[2],&regs[3]);

    // Check ECX bits
    if (regs[1] & CPUID_FEATURE_AESNI)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}