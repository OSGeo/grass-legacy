/****************************************************************************
 *
 * MODULE:       r.distance
 *
 * AUTHOR(S):    Michael Shapiro - CERL
 *
 * PURPOSE:      Locates the closest points between objects in two
 *               raster maps.
 *
 * COPYRIGHT:    (C) 2003 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 ***************************************************************************/

#include "defs.h"

/* given two CatEdgeLists, find the cells which are closest
 * and return east,north for the tow cells and the distance between them
 */

/* this code assumes that list1 and list2 have at least one cell in each */

void 
find_minimum_distance (struct CatEdgeList *list1, struct CatEdgeList *list2, double *east1, double *north1, double *east2, double *north2, double *distance, struct Cell_head *region)
{
    int i1,i2;
    double dist;
    double e1,n1,e2,n2;
    extern double G_distance();
    extern double G_row_to_northing();
    extern double G_col_to_easting();


    for (i1 = 0 ; i1 < list1->ncells; i1++)
    {
	e1 = G_col_to_easting  (list1->col[i1]+0.5, region);
	n1 = G_row_to_northing (list1->row[i1]+0.5, region);

	for (i2 = 0 ; i2 < list2->ncells; i2++)
	{
	    e2 = G_col_to_easting  (list2->col[i2]+0.5, region);
	    n2 = G_row_to_northing (list2->row[i2]+0.5, region);
	    dist = G_distance (e1, n1, e2, n2);

	    if ((i1 == 0 && i2 == 0) || (dist < *distance) )
	    {
		*distance = dist;
		*east1    = e1;
		*north1   = n1;
		*east2    = e2;
		*north2   = n2;
	    }
	}
    }
}
