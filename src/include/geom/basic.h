/* basic/basic.h --- Headerfile for The Basic C Library & Macro Extensions. */

/* NOTE: Use "cc -D__DEBUG__" when debugging! */

#ifndef __BASIC_H__  /* Include this file only once! */
#define __BASIC_H__

#include "config.h"

/*--------------------------------------------------------------------------*/

/* Include some standard header files; see Unix manual pages. */

#include <stdlib.h>     /* standard lib: abort(), system(), getenv(), ... */
#include <stdio.h>      /* standard IO: FILE, NULL (*), EOF, ... */
#include <ctype.h>      /* char macros: isalpha(), ... */
#include <math.h>       /* math lib: sin(), ...; use with -lm */
#include <sys/param.h>  /* constants like MAXPATHLEN, hopefully */
#include <strings.h>
#include <stdarg.h>

#ifdef HAVE_VALUES_H
# include <values.h>   /* for MAXINT, MAXFLOAT, BITS() ... */
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>  /* system types definitions: time_t, ... */
#endif

/*            (*) NOTE: The constant NULL designates a nonexistent pointer. */

/*--------------------------------------------------------------------------*/

/* Redefine some definitions to calm down lint. */

/* Unfortunately, there's a bug in lint on the SGIs concerning MAXINT. */
#if defined (lint) && defined (sgi) 
# undef  MAXINT
# define MAXINT  0  /* or whatever */
#endif

/* Some systems do bcopy() etc w/ (char *) parameters instead of (void *)
   as in the man pages.  What do we care?  It only matters with lint.  */
#if defined (lint)
# define bcopy  basic_relax
# define bcmp   basic_relaxf
# define bzero  basic_relax
#endif

/*--------------------------------------------------------------------------*/

/* Substitutes for missing definitions in some systems' <values.h> file. */

#ifndef BITSPERBYTE
# if gcos
#  define BITSPERBYTE     9
# else
#  define BITSPERBYTE     8
# endif
#endif

#ifndef  BITS
# define BITS(type)      (BITSPERBYTE * (int)sizeof(type))
#endif

#ifndef  HIBITI
# define HIBITI  (1 << BITS(int) - 1)
#endif

#ifndef MAXINT
# define MAXINT  (~HIBITI)
#endif

#ifndef MAXFLOAT
# include <float.h>
# define MAXFLOAT FLT_MAX
# define MINFLOAT FLT_MIN
#endif

/*--------------------------------------------------------------------------*/

/* Substitutes for missing definitions in some systems' <sys/param.h> file. */
#ifndef HZ
# define HZ  60  /* Just a guess! */
#endif

#ifndef  MAXPATHLEN  
# define MAXPATHLEN  256  /* Or even 1024? */
#endif

/*---------------------------------------------------------------------------*/

/* Standard references with possibly non-standard prototypes; see man pages. */

extern int end, etext, edata;
extern int errno;

#ifdef sun    /* updated 11/99 */
#ifndef sparc
  char *sprintf();
#endif
#endif

#ifdef sun
#ifndef sgi
#ifndef sparc  /* updated 11/99 */
 extern long random();
 extern srandom();
#endif
#endif
#endif

/*--------------------------------------------------------------------------*/

/* Boolean (int) constants. */

#define Basic_byte  unsigned char

#ifdef TRUE
# undef TRUE
#endif
#define TRUE   1

#ifdef FALSE
# undef FALSE
#endif
#define FALSE  0

#define YES    TRUE
#define NO     FALSE

#define ON     TRUE
#define OFF    FALSE

/*--------------------------------------------------------------------------*/

/* Human-readable logical operators and functional in-line if.*/

#define and    &&
#define or     ||
#define not    !
#define mod    %

#define If(COND,THEN,ELSE)  ((COND) ? (THEN) : (ELSE))

/*--------------------------------------------------------------------------*/

/* Nicer loop constructs. */

#define   upfor(I,S,E)  for (I = (S); I <= (E); I++)
#define downfor(I,S,E)  for (I = (S); I >= (E); I--)
#define until(Q)        while (not (Q));
#define loop            for (;;) 
#define once            while (0)
/*                      Use as in  "#define MACRO(...) do { ..... } once"
                        to enable semicolons after MACRO() envocations. */

/*--------------------------------------------------------------------------*/

/* Some useful macros. (NOTE: They might evaluate their arguments twice!) */

#define Odd(X)   ((X) % 2 == 1)
/* The proper def of "Even" would be "#define even(X)  (! Odd (X))";
   note the subtile difference to "((X) % 2 == 0)" wrt portability! */

#define Sign(X)    (((X) > 0) ? 1 : (((X) < 0) ? -1 : 0))
#define Abs(X)     (((X) >= 0) ? X : -(X))

#define Min(X,Y)   (((X) < (Y)) ? (X) : (Y))
#define Max(X,Y)   (((X) > (Y)) ? (X) : (Y))

#define Square(X)  ((X) * (X))

#ifndef __powerof2
#define __powerof2(EXP) (1 << (EXP))
#endif
#define bitsof(TYPE)  (BITS (TYPE))

/* ------------------------------------------------------------- */

/* Usually, the return values of ?printf() and ?scanf() are useless. */

#define  print  (void) printf
#define fprint  (void) fprintf
#define sprint  (void) sprintf
#define  scan   (void)  scanf
#define fscan   (void) fscanf
#define sscan   (void) sscanf

#define flush() fflush (stdout)

/*--------------------------------------------------------------------------*/

/* Sun's CAT operator.  Eg, CAT(CAT(pr,i),nt) ("Hello world."); */

#ifndef __GNUC__  /* Since this won't work in GNU C; use stringifiers! */
# ifndef CAT
#  define CAT_DEFINED_IN_BASIC
#  undef  CAT_IDENT
#  define CAT_IDENT(X) X
#  define CAT(A,B) CAT_IDENT(A)B
# else
# endif
#endif

/*--------------------------------------------------------------------------*/

/* Macros binfwrite() and binfread(), for binary in/output. */

/* NOTE: User of binfread() must provide enough space at *PTR.
   Cf, man fread; man fwrite. */

#define binfread(PTR,N,F) \
do { \
     int items = fread ((char *) (PTR), sizeof (*(PTR)), (N), (F)); \
     if (items != (N)) basic_error ("binfread: failed"); \
   } once

#define binfwrite(PTR,N,F) \
do { \
     int items = fwrite ((char *) (PTR), sizeof (*(PTR)), (N), (F)); \
     if (items != (N)) basic_error ("binfwrite: failed"); \
   } once

/*--------------------------------------------------------------------------*/

/* Assert(EX) & Assert_always(EX) --- Assertion macros. */

/* The Assert(EX) macro indicates that expression EX is expected to be TRUE
   (ie, nonzero) at this point of the program.  When EX is FALSE (ie, zero)
   the macro calls the basic_error() routine with a diagnostic comment.
   Compilation w/o -D__DEBUG__ ignores all Assert() and Assert_if() macros
   in your code, but not Assert_always(). (Based on /usr/include/Assert.h.) */
  
#define Assert_always(EX) \
do { \
     if (not (EX)) basic_error (basic__assert_format, __FILE__, __LINE__); \
   } once

#ifdef __DEBUG__
# define Assert(EX)          Assert_always (EX)
# define Assert_if(FLAG,EX)  do { if (FLAG) Assert_always (EX); } once
#else
# define Assert(EX)          /* relax */
# define Assert_if(FLAG,EX)  /* relax */
#endif

/*---------------------------------------------------------------------------*/

/* MODULE basic/basic.c --- some core routines */

extern char basic_charbit_mask__[];
extern char basic_charbit_mask_n[];

#define     charbit_on(I,CVAR)  ((basic_charbit_mask__[I] & CVAR) != 0)
#define     charbit_s1(I,CVAR)  ((char) (basic_charbit_mask__[I] | CVAR))
#define     charbit_s0(I,CVAR)  ((char) (basic_charbit_mask_n[I] & CVAR))

#define intbit_on(I,IVAR) ((__powerof2 (I) & IVAR) != 0)

#define NULL_HOOK  basic__null_hook

extern char basic__assert_format[];
extern void (* basic__null_hook) ();
     
/* basic.c */
void basic_relax(char *, ...);
int basic_relaxf(char *, ...);
void basic_error(char *, ...);
void basic_error_hook(void (*)(void));
int basic_kbytes(int);
double basic_mbytes(int);
char *basic_strip(char *);
void basic_system(char *, ...);

/*--------------------------------------------------------------------------*/

/* basic/files.c --- encapsulated file utilities */

FILE* basic_fopen();
void  basic_fclose();
/* files.c */
int basic_access(char []);

/*--------------------------------------------------------------------------*/

/* MODULE basic/malloc.c --- encapsulated malloc */

/* Macros for dynamic memory alloctaion:
   
   (T *)    MALLOC (T, N)      ... allocate N elements of type T
   void     REALLOC (P, T, N)  ... re-allocate memory object (char *) P
   (char *) STRDUP (S)         ... duplicate string (char *) S
   void     FREE (P)           ... deallocate memory object (char *) P
   void     BZERO (P, T, N)    ... clear bytes of memory object (char *) P

   The parameters T and N above always denote a type and an integer number.
   Momory objects (char *) P are assumed to be allocated with this module.
   Cf, malloc(), realloc(), strdup(), free(), bzero() of standard library.
   NOTE that REALLOC() is a void function... in contrast to realloc()!

   The macros below are ignored for basic_malloc_debug (0), which is the
   default.

   void     HIDE (P)           ... mark memory object P as "hidden"
   void     MARK (P, M)        ... mark memory object P with (int) M

   */

#define MALLOC(T,N) \
  (T*) basic_malloc ((int) sizeof(T) * (N), __FILE__, __LINE__)

#define REALLOC(P,T,N) \
  P = (T*) basic_realloc ((char*) P, (int) sizeof(T) * (N), __FILE__, __LINE__)

#define STRDUP(S) \
  basic_strdup (S, __FILE__, __LINE__)

#define FREE(P) \
  do { basic_free ((char*) P, __FILE__, __LINE__);  P = NULL; } once

#define BZERO(P,T,N)  bzero ((char*) P, (int) sizeof (T) * (N))

#define HIDE(P)    basic_malloc_mark ((char *) P, -1)
#define MARK(P,M)  basic_malloc_mark ((char *) P,  M)

typedef struct basic_malloc_info_struct
{
  int total;
  int in_use;
  int hidden;
  int arena;
  int used;
  int used_sml;
  int used_ord;
} Basic_malloc_info;

Basic_malloc_info* basic_malloc_info();
void basic_malloc_debug(int);
void basic_malloc_mark(char *, int);
int basic_malloc_marked_bytes(int, int);
void               basic_malloc_list_print();
void               basic_malloc_info_print();

/* NOTE: The routines below should be invoked only in connection */
/*       with the above mentioned allocation macros!             */
char *basic_malloc(int, char *, int);
char *basic_realloc(char *, int, char *, int);
void basic_free(char *, char *, int);
char *basic_strdup(char *, char *, int);

/*--------------------------------------------------------------------------*/

/* MODULE basic/getarg.c --- parse command-line arguments */

/* This was originally written by D'Arcy J M Cain. */

/* getarg.c */
int basic_getarg_init(int, char **);
int basic_getarg_inite(int, char **);
int basic_getarg(char *);
extern char* basic_getarg_optarg;
extern int   basic_getarg_optind;

/*--------------------------------------------------------------------------*/

/* MODULE basic/cb.c --- a (stack of) dynamically sized char buffer(s) */

/* cb.c */
void basic_cb_putc(int);
char *basic_cb_str(void);
void basic_cb_clear(void);
int basic_cb_len(void);
int basic_cb_size(void);

/* cb_doprnt.c */
int basic_cb_doprnt(register char *, va_list);
int basic_cb_do2prnt( int, register char *, va_list *);

int   basic_cb_addalinef();
char* basic_cb_getline();

int   basic_cb_printf(char *,...);
char* basic_cb_frmt(char *,...);

#define basic_cb_addaline  (void) basic_cb_addalinef
#define basic_cb_print     (void) basic_cb_printf

#define basic_cb_vprintf   basic_cb_doprnt

void basic_cb_push_buffer(void);
void basic_cb_pop_buffer(void);

/*--------------------------------------------------------------------------*/

/* MODULE basic/isort --- inplace (insertion) sorting of very small lists */

/* isort.c */
int basic_isort2(int *, int *);
int basic_isort3(int *, int *, int *);
int basic_isort4p(int *, int *, int *, int *);
int basic_isort4(int *, int *, int *, int *);
int basic_isort5p(int *, int *, int *, int *, int *);

/*--------------------------------------------------------------------------*/

/* MODULE basic/prime.c --- prime numbers */

int basic_prime_successor();
int basic_prime_test();

typedef struct basic_prime_info_struct
{
  int tests, mods;
} Basic_prime_info;

Basic_prime_info* basic_prime_info();

/*--------------------------------------------------------------------------*/

/* MODULE basic/uhash.c --- universal hashing */

void basic_uhash_new(int, int, int *, int []);
int  basic_uhash();

/*--------------------------------------------------------------------------*/

/* MODULE basic/istaque --- unbounded int stack/queue, dynamic re-allocation */

typedef char* Basic_istaque_adt;  /* Abstract data type! */

Basic_istaque_adt basic_istaque_new (int);
void basic_istaque_dispose (Basic_istaque_adt);

void basic_istaque_clear (Basic_istaque_adt);
int basic_istaque_empty (Basic_istaque_adt);

int basic_istaque_top (Basic_istaque_adt);
int basic_istaque_pop (Basic_istaque_adt);
void basic_istaque_push ( Basic_istaque_adt, int);
int basic_istaque_bot (Basic_istaque_adt);
int basic_istaque_get (Basic_istaque_adt);

int basic_istaque_length (Basic_istaque_adt);
void basic_istaque_print ( Basic_istaque_adt , FILE *);
double basic_istaque_resize ( Basic_istaque_adt , double );

/*--------------------------------------------------------------------------*/

/* MODULE basic/time.c --- system time etc */

double basic_utime(void);
char *basic_hostname(void);
char *basic_date(void);
void basic_daytime(int *, int *, int *, int *);
double basic_seconds(void);
int basic_seed(void);

/*--------------------------------------------------------------------------*/

/* MODULE basic/counter.c --- macros & routines for big counters */

typedef struct basic_counter_record
{
  int a, b;
} Basic_counter;

/* counter.c */
char *basic_counter (Basic_counter);
void basic_counter_reset (Basic_counter *);
void basic_counter_plus_ ( Basic_counter *, int);

#define basic_counter_plus(PTR2CNTR,INCR) basic_counter_plus_ (PTR2CNTR, INCR)

/* NOTE: basic_counter_plus() behaves like a function but is a macro, such
 *       that you can redefine it in case you don't bother counting at all.
 */

/*--------------------------------------------------------------------------*/

/* Miscellaneous routines. */

/* qsort.c */
void basic_qsort(int [], int, int, int (*)(void));
/* tokenize.c */
int basic_tokenize(char [], char *[], int);

/*--------------------------------------------------------------------------*/

/* MODULE math2.c --- math substitutes and some integer math */

#if defined (sgi) || defined (__convex__)
 double log2(double);
 double exp2(double);
 double exp10(double);
#endif

/* math2.c */
int basic_ipower(int, int);

/*--------------------------------------------------------------------------*/

/*                         .................................................
                           "First learn computer science and all the theory.
                           Next develop a programming style. Then forget all
                           that and just hack."    -- George Carrette [1990]
                           ................................................. */

#endif  /* #ifndef __BASIC_H__ */
