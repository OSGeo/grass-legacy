/* sos/rho2.c */

/*--------------------------------------------------------------------------*/

#include "geom/basic.h"
#include "geom/sos.h"
#include "internal.h"

/*--------------------------------------------------------------------------*/

void sos_rho2 (i, j, k, a, b)
     int i, j, k;
     Lia_obj a, b;  /* output */
     /* Calculates Rho2 == a/b (the radius of the circumsphere of) 
        the triangle in E^3 given by spanning vertices: indices i, j, k,
        all in proper range and pairwise different.
        NOTE that a/b can be equal to 1/0 (infinity) and 0/0 (zero). */
{
  SoS_primitive_result *num, *den;
#ifdef __DEBUG__
  if (sos_proto_flag)
    print ("sos_rho2 (%d,%d,%d,%d)\n", i, j, k);
#endif
  (void) basic_isort3 (&i, &j, &k);
  num = sos_rho2_num (i, j, k);
  lia_copy (num->lia_pointer, a);
  den = sos_rho2_den (i, j, k);
  lia_copy (den->lia_pointer, b);
  if ((num->depth == den->depth) and (num->depth == 0))
    { /* eps(num) == eps (den) == 1  ==>  normal ratio: a/b */
      /* do nothing */
    }
  else if (sos_epsilon_compare (num, den) < 1)
    { /* eps(num) <= eps(den)  ==>  zero: 0/0 */
      lia_copy (lia_const (0), a);
      lia_copy (lia_const (0), b);
    }
  else
    { /* eps(num) > eps(den)  ==>  infinity: 1/0 */
      lia_copy (lia_const (1), a);
      lia_copy (lia_const (0), b); 
    }
#ifdef __DEBUG__
  if (sos_test_flag)
    if (not ((0 <= i) and (i < j) and (j < k) and (k <= sos_common.n)))
      basic_error ("sos_rho2: arguments were wrong");
  if (sos_proto_flag)
    {
      lia_clear ();
      print ("sos_rho2 (%d,%d,%d) result: %s/%s (%d,%d;%d)\n",
             i, j, k, lia_deci (a), lia_deci (b),
             num->depth, den->depth, sos_epsilon_compare (num, den) );
    }
#endif
}
