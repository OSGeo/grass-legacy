/* sos/above4.c */

/*--------------------------------------------------------------------------*/

#include "geom/basic.h"
#include "geom/sos.h"
#include "internal.h"

/*--------------------------------------------------------------------------*/

int sos_above4 (i, j, k, l, m)
     int i, j, k, l, m;
     /* SoS predicate above4.
        Assumes indices in proper range and pairwise different. */
{
  int result, s, d1, d2;
#ifdef __DEBUG__
  if (sos_proto_flag)
    print ("sos_above4 (%d,%d,%d,%d,%d)\n", i, j, k, l, m); 
#endif
  (void) basic_isort4 (&i, &j, &k, &l);
  d1 = sos_lambda4 (i, j, k, l) -> signum;
  s = basic_isort5p (&i, &j, &k, &l, &m);
  d2 = sos_lambda5 (i, j, k, l, m) -> signum;
  result = If (Odd (s), (d1 == d2), (d1 != d2));
#ifdef __DEBUG__
  if (sos_test_flag)
    if (not ((0 < i) and (i < j) and (j < k) and (k < l) and (l < m)
             and (m <= sos_common.n)))
    basic_error ("sos_above4: arguments were wrong");
  if (sos_proto_flag)
    print ("sos_above4 (%d,%d,%d,%d,%d) result: %d (%d %d %d)\n",
           i, j, k, l, m, result, s, d1, d2);
#endif
  return (result);
}
