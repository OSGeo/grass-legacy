#include <stdio.h>
#include <stdlib.h>
#include "Vect.h"
#include "global.h"

/* 
 * Read: - points/centroids : cat,count,coor
 *       - lines/boundaries : cat,count,length
 */

int 
read_lines(struct Map_info *Map )
{
    int    i, idx, nlines, type, found;
    register int line_num;
    struct line_pnts *Points;
    struct line_cats *Cats;
    double len;

    /* Initialize the Point struct */
    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct ();

    /* Cycle through all lines */
    nlines = Vect_get_num_lines ( Map );
    for (line_num = 1 ; line_num <= nlines; line_num++)
    {
	type = Vect_read_line ( Map, Points, Cats, line_num);
	if ( !(type & options.type ) ) continue;
		
	found = 0;

	for ( i = 0; i < Cats->n_cats; i++ ) {
	    if ( Cats->field[i] == options.field ) {
		idx = find_cat(Cats->cat[i]);
		if (  options.option == O_COUNT ) {
		    Values[idx].i1++;
		} else if ( options.option == O_LENGTH && (type & GV_LINES) ) {
		    /* Calculate line length */
		    len = length (Points->n_points, Points->x, Points->y);
		    Values[idx].d1 += len;
		} else if ( options.option == O_COOR && (type & GV_POINTS) ) {
		    /* overwrite by last one, count is used in update */ 
		    Values[idx].d1 = Points->x[0];
		    Values[idx].d2 = Points->y[0];
		    Values[idx].i1++;
		}
		found = 1;
	    }
	}

	if ( !found ) {  /* Values for cat 0 are reported at the end */
	    idx = find_cat(0);
	    if (  options.option == O_COUNT ) {
		Values[idx].i1++;
	    } else if ( options.option == O_LENGTH && (type & GV_LINES) ) {
		len = length (Points->n_points, Points->x, Points->y);
		Values[idx].d1 += len;
	    } else if ( options.option == O_COOR && (type & GV_POINTS) ) {
		Values[idx].d1 = Points->x[0];
		Values[idx].d2 = Points->y[0];
		Values[idx].i1++;
	    }
	}
    }

    return 0;
}

