/*
 * Spherical distance routine:  This is a temporary fix to allow the
 * calculation of distance on a spherical surface...until I can get
 * G_distance working, but it probably won't make too much difference in
 * the end.
 */

/*- Parameters passed:
 *  long1,lat1,long2,lat2 the coordinates of any two points (doubles */

/*
 * What this does: This is a "rough and ready" function that has been
 * tested.  It calculates the distance between any two points on the
 * surface of a sphere, which for most applications (i.e. tremendous
 * accuracy at very long distances) is all that is needed.
 */
#include <math.h>

double 
sphere_dist (double long1, double lat1, double long2, double lat2)
{
  double rad_conv, radius, lag;
  double a1, a2, d1, d2, cx;

  rad_conv = 4.0 * atan (1.0) / 180;
  radius = 6378.1370;

  /*
   * The following bit uses some spherical geometry to calculate the arc
   * length, see D.H. Maling.1973. COORDINATE SYSTEMS AND MAP PROJECTIONS,
   * p41 (call no. GA 110.M32).  Pi/180 is the degree to radian
   * conversion.
   */

  a1 = long1 * rad_conv;
  d1 = lat1 * rad_conv;
  a2 = long2 * rad_conv;
  d2 = lat2 * rad_conv;
  cx = sin (d1) * sin (d2) + cos (d1) * cos (d2) * cos (a2 - a1);

  /*
   * The following tests for and catches out-of-bounds errors for the
   * acos(x) function.  It occurs because of precision rounding.
   */
  if (cx > 1.0 || cx < -1.0)
    lag = 0.0;
  else
    lag = radius * acos (cx);

  /* fprintf(stderr,"\nlag=%lf",lag); */

  return (lag);
}
