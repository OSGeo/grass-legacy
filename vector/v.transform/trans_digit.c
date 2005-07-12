/*
****************************************************************************
*
* MODULE:       v.transform
* AUTHOR(S):    See other files as well...
*               Eric G. Miller <egm2@jps.net>
* PURPOSE:      To transform a vector layer's coordinates via a set of tie
*               points.
* COPYRIGHT:    (C) 2002 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
/*
*  Written by the Radim Blazek
*/

#include <math.h>
#include "libtrans.h"
#include "Vect.h"
#include "gis.h"
#include "trans.h"

#define PI M_PI

int 
transform_digit_file (struct Map_info *Old, struct Map_info *New,
	              int    shift, 
	              double xshift, double yshift, double zshift, double ztozero,
		      double zrot, double xscale, double yscale, double zscale)
{
    int    i, type;
    double ang, x, y; 
    static struct line_pnts *Points;
    static struct line_cats *Cats;

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    

    ang = PI * zrot / 180;
    
    while (1) {
	type = Vect_read_next_line (Old, Points, Cats);
        
	if ( type == -1 ) /* error */
	    return 0;
	
	if ( type == -2 ) /* EOF */
	    return 1;
	
	for ( i = 0; i < Points->n_points; i++ ) {
	    if ( !shift ) {
                transform_a_into_b( Points->x[i], Points->y[i], &(Points->x[i]), &(Points->y[i]) );
            } else {
		x = xshift + xscale * Points->x[i] * cos(ang) - yscale * Points->y[i] * sin(ang); 
		y = yshift + xscale * Points->x[i] * sin(ang) + yscale * Points->y[i] * cos(ang); 
		Points->x[i] = x;
		Points->y[i] = y;
	    }
            /* ztozero shifts oldmap z to zero, zshift shifts rescaled object
             * to target elevation: */
            Points->z[i] = ((Points->z[i] + ztozero) * zscale) + zshift;
	}
	
	Vect_write_line (New,  type, Points, Cats);
    }
}
