
#include	<stdio.h>
#include	"map.h"
#include    "libtrans.h"
#include    "georef.h"

int 
geo_reset_transform (void)
{

	/*  compute_transformation_coef() returns:
	*   -2,  Not enough points
	*   1,   everything is okay
	*   -1,  points weren't spread out enough
	*/

	if ( compute_transformation_coef (ax, ay, bx, by, use, MAX_COOR) != 1)
		return(-1) ;

	 return(0) ;

}
