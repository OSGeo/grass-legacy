/* lia/det.c  ---  Lia determinants. */

/* All procedures lia_detI (for I=2,3,4) calculate small determinants of long
   integer arguments employing expansion by minors, esp, by the last column.
   (For lia_det4() the expansion is implemented in a bottom-up fashion which
    gives a speed up of 30% or so.)
   NOTE: In contrast to the basic routines of the Lia Library, lia_detI()'s
         output parameters are at the end of the parameter list! */

/*--------------------------------------------------------------------------*/

#include "base.h"

/*--------------------------------------------------------------------------*/

/* Workspace. */
#define AUX_SIZE  15
#define AUX(IND)  &(aux_buffer [li * IND])
static Lia *aux_buffer;
static int li = 0;

/*--------------------------------------------------------------------------*/

/* Some local macros. */

#define d2(X11,X12,X21,X22,D) \
do { \
     lia_mul (AUX (0), X11, X22); \
     lia_mul (AUX (1), X12, X21); \
     lia_sub (D, AUX (0), AUX (1)); \
   } once

#define e3(D123,X13,T23,X23,T13,X33,T12) \
do { \
     lia_mul (AUX (0), X33, T12); \
     lia_mul (AUX (1), X23, T13); \
     lia_sub (AUX (3), AUX (0), AUX (1)); \
     lia_mul (AUX (0), X13, T23); \
     lia_add (D123, AUX (3), AUX (0)); \
   } once

#define D12   AUX (5)
#define D13   AUX (6)
#define D14   AUX (7)
#define D23   AUX (8)
#define D24   AUX (9)
#define D34   AUX (10)
#define D123  AUX (11)
#define D124  AUX (12)
#define D134  AUX (13)
#define D234  AUX (14)

/*--------------------------------------------------------------------------*/

void lia_det ()
     /* Module initilization: allocate aux_buffer space. */
{
  li = lia_get_maximum ();
  aux_buffer = MALLOC (Lia, AUX_SIZE * li);
  BZERO (aux_buffer,   Lia, AUX_SIZE * li);
  MARK ( aux_buffer, -LIA_MAGIC);
}

/*--------------------------------------------------------------------------*/

void lia_det2 (a, b, 
               c, d,  result)
     Lia_obj a, b;
     Lia_obj c, d;
     Lia_obj result;  /* outout */
{
  lia_mul (AUX (0), a, d);
  lia_mul (AUX (1), b, c);
  lia_sub (result, AUX (0), AUX (1));
}

/*--------------------------------------------------------------------------*/

void lia_det3 (a, b, c,
               e, f, g,
               o, p, q,  result )
     Lia_obj a, b, c;
     Lia_obj e, f, g;
     Lia_obj o, p, q;
     Lia_obj result;  /* output */
{
  d2 (f, g,  p, q,  AUX (2));
  d2 (e, g,  o, q,  AUX (3));
  d2 (e, f,  o, p,  AUX (4));
  lia_mul (AUX (5), a, AUX (2));
  lia_mul (AUX (2), b, AUX (3));
  lia_mul (AUX (3), c, AUX (4));
  lia_sub (AUX (4), AUX (5), AUX (2));
  lia_add (result, AUX (4), AUX (3));
}

/*--------------------------------------------------------------------------*/

void lia_det4 (x11, x12, x13, x14,
               x21, x22, x23, x24,
               x31, x32, x33, x34,
               x41, x42, x43, x44,
               det)
     Lia_obj x11, x12, x13, x14;
     Lia_obj x21, x22, x23, x24;
     Lia_obj x31, x32, x33, x34;
     Lia_obj x41, x42, x43, x44;
     Lia_obj det;  /* output */
{
  d2 (x11, x12,  x21, x22,  D12);
  d2 (x11, x12,  x31, x32,  D13);
  d2 (x11, x12,  x41, x42,  D14);
  d2 (x21, x22,  x31, x32,  D23);
  d2 (x21, x22,  x41, x42,  D24);
  d2 (x31, x32,  x41, x42,  D34);
  ;
  e3 (D123, x13, D23, x23, D13, x33, D12);
  e3 (D124, x13, D24, x23, D14, x43, D12);
  e3 (D134, x13, D34, x33, D14, x43, D13);
  e3 (D234, x23, D34, x33, D24, x43, D23);
  ;
  lia_mul (AUX (0), x44, D123);
  lia_mul (AUX (1), x34, D124);
  lia_sub (AUX (3), AUX (0), AUX (1));
  lia_mul (AUX (0), x24, D134);
  lia_mul (AUX (1), x14, D234);
  lia_sub (AUX (4), AUX (0), AUX (1));
  lia_add (det, AUX (3), AUX (4));  
}
