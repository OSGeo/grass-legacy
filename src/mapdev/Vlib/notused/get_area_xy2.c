/*
**  Written by:  Mike Higgins 5 1988
** 		 Dave Gerdes
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"

/* NOT DONE */


/* returns 0  or -1 on error */
V2_get_area_xy (Map, area, Points)
	struct Map_info *map;
	int area;
	struct line_pnts *Points;
{
    Points = dig__P_get_area_xy (&MAP2(fd), area) ;

    if (Points == NULL)
	return (-1) ;

    *x = Points->x ;
    *y = Points->y ;
    *n_points = Points->n_points ;

    return (0) ;
}
