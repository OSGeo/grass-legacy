/* lia/base.h  ---  Internal macros and constants for Lia package. */

#ifndef __LIA_BASE_H__  /* Include this file only once! */
#define __LIA_BASE_H__ 

/*--------------------------------------------------------------------------*/

#include "geom/basic.h"
#include "geom/lia.h"

/*---------------------------------------------------------------------------*/

/* Define "length" of Lia digits.
   (Usually, BETA is 15 since Lia is 32 bits wide and we use 2 bits
   for overflow */

#define BETA       13  /* ((bitsof (Lia) - 2) / 2) */

#define BASE       __powerof2 (BETA)
#define DBASE      __powerof2 (2 * BETA)

#define LIA_MAGIC  130862003

/*--------------------------------------------------------------------------*/

/* fast mod, /, and * operations */

#define mod_BASE    & (BASE - 1) /* a mod_BASE == a (bitwise AND) (BASE - 1) */
#define div_BASE   >> BETA       /* a div_BASE == a (shift right) log2 BASE  */
#define times_BASE << BETA
#define mod_DBASE   & (DBASE - 1)
#define div_DBASE  >> (2 * BETA)

#define div_2    >> 1
#define mod_2     & 1
#define times_2  << 1

/*--------------------------------------------------------------------------*/

#define is_non_zero(LONGI)  ((LONGI[1] != 0) or ((LONGI[0] div_2) > 1))

#define is_negative(LONGI)  (LONGI[0] mod_2)

#define chs(LONGI) \
do { \
     if (is_negative (LONGI)) \
       LONGI[0] --; \
     else \
       LONGI[0] ++; \
   } once
   /* NOTE: This macro changes sign bit *even* if LONGI is zero! */

/*--------------------------------------------------------------------------*/

extern Lia_info lia_common;  /* from lia/auxiliary.c */

/*--------------------------------------------------------------------------*/

#endif  /* #ifdef __LIA_BASE_H__ */
