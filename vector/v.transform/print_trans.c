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


#include <stdio.h>
#include "gis.h"
#include "trans.h"
#include "glocale.h"

int 
print_transform_resids (int n_points)
{
	int i ;

	G_message ( "\n");
	G_message ( _("                          CHECK MAP RESIDUALS\n\n"));
	G_message ( _("                Current Map                  New Map\n"));
	G_message ( _(" POINT      X coord    Y coord  |        X coord   Y coord    |      residuals\n"));
	G_message ( "\n");

	for (  i = 0 ;  i < n_points;  i++ )
 	{

		if ( use[i])
			G_message ( " %2d.  %12.2f %12.2f | %12.2f   %12.2f | %12.2f\n",
			 i+1, ax[i], ay[i], bx[i], by[i], residuals[i]) ;
	
 	}
		
	G_message ( _("\n\n  Number of points: %d\n"), n_points) ;
	G_message ( _("  Residual mean average   : %f\n"), rms) ;

	return (0) ;

}		/*  print_transform_resid()  */


