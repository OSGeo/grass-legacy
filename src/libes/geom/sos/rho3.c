/* sos/rho3.c */

/*--------------------------------------------------------------------------*/

#include "geom/basic.h"
#include "geom/sos.h"
#include "internal.h"

/*--------------------------------------------------------------------------*/

void sos_rho3 (i, j, k, l, a, b)
     int i, j, k, l;
     Lia_obj a, b;  /* output */
     /* Calculates Rho3 == a/b (the radius of the circumsphere of) 
        the tetrahedron given by the spanning vertices: indices i, j, k, l,
        all in proper range and pairwise different.
        NOTE that a/b can be equal to 1/0 (infinity) and 0/0 (zero). */
{
  SoS_primitive_result *num, *den;
#ifdef __DEBUG__
  if (sos_proto_flag)
    print ("sos_rho3 (%d,%d,%d,%d)\n", i, j, k, l);
#endif
  (void) basic_isort4 (&i, &j, &k, &l);
  num = sos_rho3_num (i, j, k, l);
  lia_copy (num->lia_pointer, a);
  den = sos_rho3_den (i, j, k, l);
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
    if (not ((0 <= i) and (i < j) and (j < k) and (k < l)
             and (l <= sos_common.n)))
      basic_error ("sos_rho3: arguments were wrong");
  if (sos_proto_flag)
    {
      lia_clear ();
      print ("sos_rho3 (%d,%d,%d,%d) result: %s/%s (%d,%d;%d)\n",
             i, j, k, l, lia_deci (a), lia_deci (b),
             num->depth, den->depth, sos_epsilon_compare (num, den) );
    }
#endif
}
