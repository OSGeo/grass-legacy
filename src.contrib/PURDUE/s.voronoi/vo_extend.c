#include <stdio.h>
#include <math.h>
#include "vo_defs.h"

/*-extend_line()  finds coordinates along the boundary of a window
 *                  that also lie on a specified line (ax+by=c). The
 *                  line can cross at least two boundaries---the line
 *                  that intersects the midpoint of s1 and s2 determines 
 *                  which coordinates are placed in (c_x, c_y).
 *
 * The limits of the window are described by:
 *    e:  east
 *    w:  west
 *    s:  south
 *    n:  north
 * Note that the following constraints must be true:
 *    ( w < e )     ( s < n )
 *
 *    x and y are points on the line ax + by = c that are assumed
 *    to lie within the window.
 *
 *    the c_x and c_y values are changed.
 *
 * returns: 0 on error, 1 otherwise
 */

int extend_line (double s, double n, double w, double e,
  double a, double b, double c, double x, double y,
  double *c_x, double *c_y,
  double s1_x, double s1_y, double s2_x, double s2_y)
{
  int i;
  double nx[4], ny[4], mx,my; 

  if (x > w && x < e && y > s && y < n)
  {
    nx[0] = e;
    ny[0] = (c - a * nx[0]) / b;

    nx[1] = w;
    ny[1] = (c - a * nx[1]) / b;

    ny[2] = s;
    nx[2] = (c - b * ny[2]) / a;

    ny[3] = n;
    nx[3] = (c - b * ny[3]) / a;

    mx = s1_x + (s2_x - s1_x)/2.0;
    my = s1_y + (s2_y - s1_y)/2.0;
    for(i=0; i<4; ++i)
    {
/*         ( (nx[i] <= x && mx <= x) || (nx[i] >= x && mx >= x) )  &&
          ( (ny[i] <= y && my <= y) || (ny[i] >= y && my >= y) ) )  */
      if (in_region(nx[i],ny[i])  &&
            ( (nx[i] <= mx && mx <= x) || (nx[i] >= mx && mx >= x) )  &&
          ( (ny[i] <= my && my <= y) || (ny[i] >= my && my >= y) ) ) 
            /* ( (nx[i] <= x && x <= mx) || (nx[i] >= x && x >= mx) )  &&
          ( (ny[i] <= y && y <= my) || (ny[i] >= y && y >= my) ) )  */
      { 
fprintf(stderr,"\nDIAG: x=%g y=%g mx=%g my=%g nx=%g ny=%g\n",x,y,mx,my,nx[i],ny[i]);
        *c_x = nx[i];
        *c_y = ny[i];
        return 1;
      } 
    }
  }
fprintf(stderr,"\nDIAG: extend line failed\n");
  return 0;
}
