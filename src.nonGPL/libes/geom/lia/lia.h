/* lia/lia.h  ---  Headerfile to the Lia (Long-integer arithmetic) package. */

#ifndef __LIA_H__  /* Include only once! */
#define __LIA_H__

/*--------------------------------------------------------------------------*/

#include "basic.h"

/*--------------------------------------------------------------------------*/

typedef unsigned long Lia;

typedef Lia* Lia_obj;
typedef Lia_obj Lia_ptr;  /* Synonyms! */
        /* Note: "typedef Lia[] Lia_obj" would be more consistent
           with this implementation but it produced syntax error. */

typedef struct Lia_info_type
{
  Basic_counter mul_calls, mul_elops;
  Basic_counter padd_calls, padd_elops;
  Basic_counter psub_calls, psub_elops;
  int maximum_digit;
  /* Below fields are private. */
  int max, length;
  int last;   /* A lia-object has the form: Lia longi[0..last], last < max. */
} Lia_info;

#define Lia_DIGITS(D)  ((D) / 8 + 1 + 1)

/*--------------------------------------------------------------------------*/

/* lia/lia.c */

void lia_assign();
void lia_chs();
int  lia_sign();

Lia_ptr lia_neg();

#define lia_copy(A,B)  lia_assign (B, A)

int lia_eq();
int lia_le();
int lia_leq();

void lia_add();
void lia_sub();
void lia_mul();

/*--------------------------------------------------------------------------*/

/* lia/auxiliary.c */

void lia_maximum();
void lia_length();

int  lia_get_maximum();
int  lia_get_length();
int  lia_get_beta();

Lia_info* lia_info();

double lia_real();
void   lia_fput();

int lia_high();
Lia lia_digit();

void lia_load();
void lia_strload();
void lia_ffpload();

Lia_ptr lia_const();

void lia_sdiv();

/*--------------------------------------------------------------------------*/

/* lia/chars.c */

void   lia_clear();
char * lia_chars();

#define lia_deci(L)  lia_chars (L, TRUE)
#define lia_hexa(L)  lia_chars (L, FALSE)

/*--------------------------------------------------------------------------*/

/* lia/stack.c */

#define LIA_NULL  ((Lia_ptr) NULL)

void    lia_stack_limit();
int     lia_stack_empty();
int     lia_stack_size();
void    lia_push();
Lia_ptr lia_pushf();
void    lia_pushtop();
void    lia_pop();
Lia_ptr lia_popf();
void    lia_times();
void    lia_plus();
void    lia_minus();
void    lia_power();
void    lia_ipower();
void    lia_negtop();

/*---------------------------------------------------------------------------*/

/* lia/pool.c */

typedef char* Lia_pool_adt;  /* Abstract data type! */

typedef struct lia_pool_info_record
{
  int longs, digits, blocks, bytes;
} Lia_pool_info;

Lia_pool_adt   lia_pool_open();
void           lia_pool_close();
void           lia_pool_kill();
Lia_ptr        lia_pool_store();
Lia_pool_info* lia_pool_info();

/*--------------------------------------------------------------------------*/

/* lia/det.c */

void lia_det();
void lia_det2();
void lia_det3();
void lia_det4();

/*--------------------------------------------------------------------------*/

/* lia/sdet.c */

void lia_sdet();
int  lia_sdet2();
int  lia_sdet3();
int  lia_sdet4();

/*--------------------------------------------------------------------------*/

/* lia/sdet_one.c */

void lia_sdet_one();
int  lia_sdet2_one();
int  lia_sdet3_one();
int  lia_sdet4_one();
int  lia_sdet5_one();

/*--------------------------------------------------------------------------*/

/*           ..............................................................
             [Seventh Commandment for C Programmers]   Thou shalt study thy
             libraries and strive not to re-invent them without cause, that
             thy code may be short and readable and thy days  pleasant  and
             productive.                                    --Henry Spencer
             .............................................................. */

#endif  /* #ifndef __LIA_H__ */
