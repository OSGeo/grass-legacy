#include <stdlib.h>
#include "krig.h"

/*
 * 1st Kriging subroutine - find the distances to the closest n sample pts
 * 
 * Parameters passed: new_north, new_east Raster image easting and
 * northing(doubles)
 * 
 * 
 * What this does: This function calculates the distance between some point
 * new_east, new_north and each entry in the list of original data points
 * to be used in the interpolation procedure. These distances are stored
 * along with the original  data lat, long, z value in a double called
 * dist. The array of structures is then sorted on the basis of this
 * distance. Thus the original data order IS NOT retained when using this
 * function.
 */
void 
dist_to_n_smpls (double new_east, double new_north)
{
  int indx, i, cnt;		/* Index into original array */
  extern int npoints, nsearch;
  extern double *smpl_dist, *smpl_z, *smpl_north, *smpl_east;
  extern Point *points;

  /* Check each lat, long value and create an array of distances */

  for (indx = 0; indx < npoints; indx++)
  {
    points[indx].dist = sphere_dist (points[indx].east,
				points[indx].north, new_east, new_north);
  }
/* Now sort the distances */
  qsort (points, npoints, sizeof (Point), cmp);	

  /* Begin searching the sorted list, quadrant by quadrant. */
  for (indx = 0, cnt = 0; indx < npoints; indx++)
  {
    if (cnt < nsearch)
    {
      /* Search the 1st quadrant      */
      if (points[indx].east > new_east && points[indx].north >= new_north
	  && points[indx].dist > 0)
      {
	smpl_dist[cnt] = points[indx].dist;
	smpl_north[cnt] = points[indx].north;
	smpl_east[cnt] = points[indx].east;
	smpl_z[cnt] = points[indx].z;
	cnt++;
      }
      /* Search the 2nd quadrant      */
      else if (points[indx].east >= new_east && points[indx].north < new_north
	       && points[indx].dist > 0)
      {
	smpl_dist[cnt] = points[indx].dist;
	smpl_north[cnt] = points[indx].north;
	smpl_east[cnt] = points[indx].east;
	smpl_z[cnt] = points[indx].z;
	cnt++;
      }
      /* Search the 3rd quadrant      */
      else if (points[indx].east < new_east && points[indx].north <= new_north
	       && points[indx].dist > 0)
      {
	smpl_dist[cnt] = points[indx].dist;
	smpl_north[cnt] = points[indx].north;
	smpl_east[cnt] = points[indx].east;
	smpl_z[cnt] = points[indx].z;
	cnt++;
      }
      /* Search the 4th quadrant      */
      else if (points[indx].east <= new_east && points[indx].north > new_north
	       && points[indx].dist > 0)
      {
	smpl_dist[cnt] = points[indx].dist;
	smpl_north[cnt] = points[indx].north;
	smpl_east[cnt] = points[indx].east;
	smpl_z[cnt] = points[indx].z;
	cnt++;
      }
    }
  }
}

/*
 * Function min, simply returns a -1 if its 1st arg is less   than the
 * second, 0 if they are the same and +1 number if the first is larger
 * than the second
 */

int 
cmp (void *arg_1, void *arg_2)
{
  double diff;			/* difference between arg_1 and arg_2 */
  int return_code = 0;		/* integer to be returned */

  diff = ((Point *) arg_1)->dist - ((Point *) arg_2)->dist;
  if (diff < 0.0)
  {
    return_code = -1;
  }
  else if (diff > 0.0)
  {
    return_code = 1;
  }
  return return_code;
}

