#include <stdio.h>
#include <stdlib.h>
#include "Vect.h"
#include "global.h"

/* Read areas of areas */
int 
read_areas(struct Map_info *Map)
{
	int i, idx, found;
	int area_num, nareas, centr;
	double area;
	struct line_cats *Cats;

	Cats = Vect_new_cats_struct ();

	nareas = Vect_get_num_areas( Map );
	/* Cycle through all areas */
	for (area_num = 1 ; area_num <= nareas; area_num++)
	{
	    area = Vect_get_area_area ( Map, area_num );

	    found = 0;
	    centr = Vect_get_area_centroid ( Map, area_num );
	    if ( centr > 0 ) {
		Vect_read_line ( Map, NULL, Cats, centr);

		for ( i = 0; i < Cats->n_cats; i++ ) {
		    if ( Cats->field[i] == options.field ) {
			idx = find_cat(Cats->cat[i]);
			Values[idx].d1 += area;
			found = 1;
		    }
		}
	    }
	    
	    if ( !found ) {  /* no category found */
		idx = find_cat(0);
		Values[idx].d1 += area;
	    }
	}

	return 0;
}

