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
    int idx, cat_no, nlines, type;
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
		
	Vect_cat_get ( Cats, options.field, &cat_no );
	/* Go on even if cat is 0, values for cat 0 are reported at the end */

	idx = find_cat( cat_no);
	
	if (  options.option == O_CAT ) {
	    /* Do nothing because find_cat() already inserted new cat */
	} else if (  options.option == O_COUNT ) {
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
    }

    return 0;
}

