#include <stdio.h>
#include <stdlib.h>
#include "Vect.h"
#include "global.h"

/* Read areas of areas */
int 
read_areas(struct Map_info *Map)
{
	int idx, cat_no;
	int area_num, nareas, centr;
	double area;
	struct line_cats *Cats;

	Cats = Vect_new_cats_struct ();

	nareas = Vect_get_num_areas( Map );
	/* Cycle through all areas */
	for (area_num = 1 ; area_num <= nareas; area_num++)
	{
	    area = Vect_get_area_area ( Map, area_num );

	    /* get the category number for area_num */
	    centr = Vect_get_area_centroid ( Map, area_num );
	    if ( centr > 0 ) {
		Vect_read_line ( Map, NULL, Cats, centr);
		Vect_cat_get ( Cats, options.field, &cat_no );
	    } else { 
		cat_no = 0;
	    }

	    idx = find_cat( cat_no);
	    
	    Values[idx].d1 += area;
	}

	return 0;
}

