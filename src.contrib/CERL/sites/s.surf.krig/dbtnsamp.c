#include "gis.h"
#include "krig.h"

/*
 * 2nd Kriging subroutine - find the distances between the cloesest n
 * sample pts Parameters passed:	none What this does: This function
 * calculates the distances between the closest nsearch sample points as
 * chosen in the 1st Kriging subroutine. No sorting is necessary as
 * the points are taken directly the already sorted list.
 */

void 
dist_btw_n_smpls (double range)
{
  int i, j;
  extern int nsearch;
  extern double *smpl_east, *smpl_north, **dist_btw_smpl;

  for (i = 0; i < nsearch; i++)
  {
    for (j = i + 1; j < nsearch; j++)
    {
      dist_btw_smpl[i][j] = sphere_dist (smpl_east[i], smpl_north[i],
					 smpl_east[j], smpl_north[j]);
      /*
       * The statement that followsis a very annoying!  It is necessary at
       * the moment as there are the occational 0 distances, which
       * completely stuffs the matrix routines.  The reason for the 0
       * distances is that GRASS apparently rounds the lat-long
       * coordinates off to 1 decimal point, so we end up with identical
       * sets of coordinates, thus 143.432, -12.234 and 143.490, -12.255
       * are indistinguishable.
       */
      if (dist_btw_smpl[i][j] == 0)
	dist_btw_smpl[i][j] = range / 100;
    }
  }
}
