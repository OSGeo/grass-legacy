/* lia/mint.c  ---  MINT version of lia_div().  Needs cc -lmp (see man mp). */

/*--------------------------------------------------------------------------*/

#if ! (defined (sun) || defined (sparc))

/* No!    MP(3X) (-lmp) package only runs on Suns! */

#else

/*--------------------------------------------------------------------------*/

#include "base.h"
#include "mint.h"

/* We need some external references from lia/chars.c. */

extern char lia_buffer[];
extern int  lia_buffer_pointer;

void lia_cput(), lia_dput();

/*--------------------------------------------------------------------------*/

MINT * lia_mint (longi)
     Lia_obj longi;
     /* Converts arbitrary Lia_obj to (MINT *) using lia_amint(). */
{
  MINT *positive, *negative;
  static MINT *minus_one = NULL;
  if (not minus_one)
    minus_one = itom (-1);
  positive = lia_amint (longi);
  if (lia_sign (longi) == -1)
    {
      negative = itom (0);
      mult (positive, minus_one, negative);
      return (negative);
    }
  return (positive);
}

/*--------------------------------------------------------------------------*/

MINT * lia_amint (longi)
     Lia_obj longi;
     /* Converts abs (Lia_obj) to (MINT *) using lia_buffer[].
        This works as a initializer for a MINT pointer.
        NOTE:  - xtom() digests positive hex numbers only;
               - it calls lia_clear(). */
{
  int p;
  static Lia *buffer = NULL;
  if (not buffer)
    {
      buffer = MALLOC (Lia, lia_common.max);
      MARK (buffer, -LIA_MAGIC);
    }
  lia_clear ();
  lia_assign (buffer, longi);
  if (Odd (buffer[0]))
    buffer[0] --;
  p = lia_buffer_pointer;
  lia_dput (buffer, FALSE);
  lia_cput ('\0');
  return (xtom (& (lia_buffer[p])));
}

/*--------------------------------------------------------------------------*/

void lia_div (result, remainder, long1, long2)
     Lia_obj result, remainder; /* output */
     Lia_obj long1, long2;
     /* Long-interger divison using MINT arithmetic:
        result    := long1 / long2;
        remainder := long1 mod long2.
        NOTE: does a lia_clear() since lia_amint() uses lia_buffer[]. */
{
  int s1 = lia_sign (long1), s2 = lia_sign (long2);
  MINT * a, * b, * q, * r;
  char * xres, * xrem;
  if ((long2[0] == 2) and (long2[1] == 0))
    basic_error ("lia_div: division by zero");
  lia_clear ();
  a = lia_amint (long1);
  b = lia_amint (long2);
  q = itom (0);
  r = itom (0);
  mdiv (a, b, q, r);
  xres = mtox (q);
  xrem = mtox (r);
  lia_strload (result, xres, "%x");
  lia_strload (remainder, xrem, "%x");
  if (s1 == -1)
    {
      lia_chs (remainder);
      if (s2 == 1)
        lia_chs (result);
    }
  else if (s2 == -1)
    lia_chs (result);
  /* free all heap storage stack-wise */
   free ((char *) xrem);   free ((char *) xres);  
  mfree ((char *) r);     mfree ((char *) q);
  mfree ((char *) b);     mfree ((char *) a);
}

/*--------------------------------------------------------------------------*/

#endif /* #ifdef sgi  #else */
