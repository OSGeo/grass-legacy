/*****************************************************************************
 *
 * MODULE:       v.out.dgn 
 * AUTHOR(S):    Radim Blazek
 * PURPOSE:      Export GRASS vector file to Microstation DGN file
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#include "math.h"
#include "gis.h"
#include "Vect.h"
#include "dgn.h"
#include "proto.h"

/* calculate center of line */

int
line_cent( struct line_pnts *points, double *x, double *y)
{
    int    i, np;
    double dx, dy, len, l, dl, p;

    np = points->n_points;

    /* length */
    len = 0;
    for (i = 0; i < np - 1; i++)
      {
        dx = points->x[i+1] - points->x[i];  
        dy = points->y[i+1] - points->y[i];  
        len += sqrt (dx*dx + dy*dy);
      }
   
    /* find center */
    l = 0;
    for (i = 0; i < np - 1; i++)
      {
        dx = points->x[i+1] - points->x[i];  
        dy = points->y[i+1] - points->y[i];  
	dl = sqrt ( dx*dx + dy*dy);
        l += dl;
        if ( l >= len / 2 )
	  {
	      p = (len / 2 - ( l - dl )) / dl;
	      dx = p * dx;
	      dy = p * dy;
	      
	      *x = points->x[i] + dx;  
	      *y = points->y[i] + dy;  	  
	      return (0);
	  } 
      }    
    return (0);
}

