#include <math.h>
#include    "Vect.h"

#define	SCALE    16000.0
/*      I arbitrarily increased this to help find straight lines ...
   #define      BOX_MIN    23.0
 */
#define	BOX_MIN    46.0


/* dig_bound_box ()   has been removed and replaced w/ this:   4.0 */

int 
dig_bound_box2 (
		 struct line_pnts *points,
		 double *N, double *S, double *E, double *W,
		 long orig_scale)
{
  static int box_min = 0;
  static long scale;

  double *xptr, *yptr;
  int n_coors;
  double dist;

  /*
     *  on straight lines the bounding box was so small it made it hard to find
     *  the line with a mouse.   first time this is called set the bounding box
     *  minimum limit.  the minimum was calculated at a scale of 16000;
     *  takes any scale into account.
     *    
     *  find current distance between (N,S), (E,W).  if its less then box_min
     *  enlarge the box to the minimum;  it also keeps the line in the middle
     *  of the box.
   */

  xptr = points->x;
  yptr = points->y;
  n_coors = points->n_points;

  /*  first time this was called  */
  if (box_min <= 0 || orig_scale != scale)
    {
      scale = (double) orig_scale;
      if (scale <= 0)
	box_min = BOX_MIN;
      else
	box_min = ((double) scale) / SCALE * BOX_MIN;
    }

  if (n_coors == 0)
    return (1);

  *E = *xptr;
  *W = *xptr;
  *N = *yptr;
  *S = *yptr;

  while (--n_coors)
    {
      xptr++;
      yptr++;
      if (*xptr < *W)
	*W = *xptr;
      if (*xptr > *E)
	*E = *xptr;
      if (*yptr < *S)
	*S = *yptr;
      if (*yptr > *N)
	*N = *yptr;
    }

  /*  have got the bounding box,  now make sure its large enough  */

/*DEBUG */
#ifdef FOO
  dist = (*N - *S) - box_min;
  if (dist < 0)
    {
      half_dist = fabs (dist) / 2;
      *N += half_dist;
      *S -= half_dist;
    }

  dist = (*E - *W) - box_min;
  if (dist < 0)
    {
      half_dist = fabs (dist) / 2;
      *E += half_dist;
      *W -= half_dist;
    }
#endif

  /*  new code added for scale problems */

  dist = fabs (*E * 0.0000001);
  *E += dist;
  *W -= dist;
  dist = fabs (*N * 0.0000001);
  *N += dist;
  *S -= dist;

  return (0);
}
