/* sos/rho1.c */

/*--------------------------------------------------------------------------*/

#include "geom/basic.h"
#include "geom/sos.h"
#include "internal.h"

/*--------------------------------------------------------------------------*/

void sos_rho1 (i, j, a, b)
     int i, j;
     Lia_obj a, b;  /* output */
     /* Calculates Rho1 == a/b (the radius of the circumsphere of) 
        3-dimensional edge given by its endpoints: indices i, j,
        all in proper range and pairwise different.
        NOTE that a/b can be equal to 0/0 (zero);
             but there is no 1/0 (infinity) case. */
{
  SoS_primitive_result *num;
#ifdef __DEBUG__
  if (sos_proto_flag)
    print ("sos_rho1 (%d,%d,%d)\n", i, j);
#endif
  (void) basic_isort2 (&i, &j);
  num = sos_rho1_num (i, j);
  lia_copy (num->lia_pointer, a);
  lia_copy (lia_const (4), b);  /* here, denumerator is constant! */
  if (num->depth == 0)
    { /* eps(num) == eps (den) == 1  ==>  normal ratio: a/b */
      /* do nothing */
    }
  else 
    { /* eps(num) <= eps(den) == 1  ==>  zero: 0/0 */
      lia_copy (lia_const (0), a);
      lia_copy (lia_const (0), b);
    }
#ifdef __DEBUG__
  if (sos_test_flag)
    if (not ((0 <= i) and (i < j) and (j <= sos_common.n)))
      basic_error ("sos_rho1: arguments were wrong");
  if (sos_proto_flag)
    {
      lia_clear ();
      print ("sos_rho1 (%d,%d) result: %s/%s (%d)\n",
             i, j, lia_deci (a), lia_deci (b), num->depth);
    }
#endif
}
