/* sos/above3_star.c */

/*--------------------------------------------------------------------------*/

#include "geom/basic.h"
#include "geom/sos.h"
#include "internal.h"

/*--------------------------------------------------------------------------*/

int sos_above3_star (i, j, k, l)
     int i, j, k, l;
     /* SoS predicate above3_star.
        Assumes indices in proper range and pairwise different
        AND one of them to be 0, indicating the *-row,
        see sos_above3_star_set(). */
{
  int result, s, d1, d2;
#ifdef __DEBUG__
  if (sos_proto_flag)
    print ("sos_above3_star (%d,%d,%d,%d)\n", i, j, k, l); 
#endif
  (void) basic_isort3 (&i, &j, &k);
  if (i == 0)
    d1 = sos_lambda3_star (j, k) -> signum;
  else
    d1 = sos_lambda3 (i, j, k) -> signum;
  s = basic_isort4p (&i, &j, &k, &l);
  d2 = sos_lambda4_star (j, k, l) -> signum;
  result = If (Odd (s), (d1 == d2), (d1 != d2));
#ifdef __DEBUG__
  if (sos_test_flag)
    if (not ((0 == i) and (i < j) and (j < k) and (k < l)
             and (l <= sos_common.n)))
      basic_error ("sos_above3_star: arguments were wrong");
  if (sos_proto_flag)
    print ("sos_above3_star (%d,%d,%d,%d) result: %d (%d %d %d)\n",
           i, j, k, l, result, s, d1, d2);
#endif
  return (result);
}

/*--------------------------------------------------------------------------*/

void sos_above3_star_set (a, b, c, d)
     /*i*/ int a, b, c, d;
     /* Sets the *-row (index 0). */
{
  lia_load (sos_lia_0 (1), a);
  lia_load (sos_lia_0 (2), b);
  lia_load (sos_lia_0 (3), c);
  lia_load (sos_lia_0 (4), d);
}
