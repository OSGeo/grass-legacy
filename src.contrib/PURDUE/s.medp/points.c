/*- void write_points (Map,n, thresh)   // no return value
 * struct Map_info *Map;                // map struct, including file pointer
 * int n;                               // number of sites
 * int thresh;                          // threshold
 *
 * writes sites as points in the vector file
 */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "polish.h"
#include "gis.h"
#include "Vect.h"

void 
write_points (struct Map_info *Map, int n, int verbose)
{
  static struct line_pnts *Points;
  double x[2], y[2];
  int i;
  extern float *datax, *datay;

  Points = Vect_new_line_struct ();	/* init line_pnts struct */

  if (verbose)
    fprintf (stderr, "Writing points ...                  ");
  for (i = 0; i < n; ++i)
  {
    x[0] = x[1] = datax[i];
    y[0] = y[1] = datay[i];

    if (Vect_copy_xy_to_pnts (Points, x, y, 2) > 0)
      G_fatal_error ("Out of memory");
    Vect_write_line (Map, DOT, Points);
    if (verbose)
      G_percent (i, n, 1);
  }
  if (verbose)
    G_percent (1, 1, 1);
}
