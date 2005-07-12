/*      Name:   findzc
        Bill Hoff       5/5/86

        Modified by David Satnik, CWU GIS Lab 9/25/90

Purpose:        Find the locations and orientations of zero crossings.

Details:        This routine finds the locations and orientations of zero
                crossings in the input array "conv", which is the result
                of the convolution of the Marr-Hildreth operator with the
                image.  The output of this routine is an array "zc" which
                is non-zero only at zero crossing pixels.  At those pixels,
                the value is 1 + (orientation), where orientation is a
                value from 0 to NumOrients (a global variable).
*/
#include <stdio.h>
#include <math.h>
#include "gis.h"
#include "globals.h"

#define PI      M_PI
#define TINY    1.0e-3

int 
findzc (double conv[], int size, double zc[], double thresh)
{
  int i, j, p, ni, dir, nbr[4];
  double  ang;


  /* go through entire conv image - but skip border rows and cols */
  for (i=1; i < size-1; i++)

    for (p=i*size+1, j=1; j < size-1; j++, p++) {

      /* examine the 4-neighbors of position p */
      nbr[0] = p - 1;           /* left */
      nbr[1] = p + 1;           /* right */
      nbr[2] = p - size;        /* up */
      nbr[3] = p + size;        /* down */

      zc[p] = 0;
      for (ni=0; ni < 4; ni++)  {
        /* condition for a zc: sign is different than a neighbor
           and the absolute value is less than that neighbor.
           Also, threshold magnitudes to eliminate noise */
        if (((conv[p] > 0 && conv[nbr[ni]] < 0) ||
             (conv[p] < 0 && conv[nbr[ni]] > 0)) &&
            (fabs (conv[p]) < fabs (conv[nbr[ni]])) &&
            (fabs (conv[p] - conv[nbr[ni]]) > thresh))  {

          /* found a zc here, get angle of gradient */
          if (fabs (conv[nbr[1]] - conv[nbr[0]]) < TINY)        {
            ang = PI/2.0;
            if  (conv[nbr[2]] - conv[nbr[3]] < 0)
              ang = -ang;
          }
          else
            ang = atan2(conv[nbr[2]] - conv[nbr[3]],
                        conv[nbr[1]] - conv[nbr[0]]);
          /* scale -PI..PI to 0..NumOrients-1 */
          dir = NumOrients * ((ang + PI) / (2 * PI)) + 0.4999;

          /* shift scale so that 0 (not 8) is straight down */
          dir = (3*NumOrients/4 + dir) % NumOrients;
          /* add to differentiate between no zc and an orientation */
          zc[p] = 1 + dir;
          break;                /* quit looking at neighbors */
        }
      }                         /* for ni */
    }                           /* for p */


    return 0;
}
