#include <stdio.h>
#include <stdlib.h>
#include "Vect.h"
#include "global.h"

/* Read area cats for one side 
 *  val - pointer where value is stored
 *  count - pointer to count of cats read 
 *  ACats - area categories
 */
void read_side_cats ( struct line_cats *ACats, int *val, int *count )
{
    int i, found;

    G_debug ( 4, "read_side_cats() n_cats = %d, val = %d, count = %d", ACats->n_cats, *val, *count );
    
    if ( *count > 1 ) return; /* it doesn't make sense to read/check more cats */

    found = 0;

    for ( i = 0; i < ACats->n_cats; i++ ) {
	if ( ACats->field[i] == options.qfield ) {
	    found = 1;
	    if ( *count == 0 ) { /* first */
		*val = ACats->cat[i]; /* set value to first found cat */
		(*count)++;
	    } else { /* *count == 1 */
		/* Check if it is the same category */
		if ( *val != ACats->cat[i] ) {
		    *count = 2;
		    break; /* no need to read more */
		}
	    }
	} 
    }
    if ( !found ) { /* i.e. area cat is NULL (-1) */
	if ( *count == 0 ) { /* first */
	    *val = -1;
	    (*count)++;
	} else { /* *count == 1 */
	    /* Check if it is the same category */
	    if ( *val != -1 ) {
		*count = 2;
	    }
	}
    }
}

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
    struct line_cats *Cats, *LCats, *RCats;
    double len;

    /* Initialize the Point struct */
    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct ();
    LCats = Vect_new_cats_struct ();
    RCats = Vect_new_cats_struct ();

    /* Cycle through all lines */
    nlines = Vect_get_num_lines ( Map );
    for (line_num = 1 ; line_num <= nlines; line_num++)
    {
	type = Vect_read_line ( Map, Points, Cats, line_num);
	if ( !(type & options.type ) ) continue;

	found = 0;

	/* Find left/right area cats if necessary */
	if ( options.option == O_SIDES && type == GV_BOUNDARY ) {
	    int area_left, area_right, centroid;

	    Vect_get_line_areas ( Map, line_num, &area_left, &area_right );

	    /* left */
	    Vect_reset_cats ( LCats );
	    if ( area_left < 0 ) 
		area_left = Vect_get_isle_area ( Map, abs(area_left) ); 

	    if ( area_left > 0 ) { 
		centroid = Vect_get_area_centroid ( Map, area_left );
		if ( centroid > 0 ) {
		    Vect_read_line ( Map, NULL, LCats, centroid);
		}
	    } 
	     
	    /* right */
	    Vect_reset_cats ( RCats );
	    if ( area_right < 0 ) 
		area_right = Vect_get_isle_area ( Map, abs(area_right) ); 

	    if ( area_right > 0 ) { 
		centroid = Vect_get_area_centroid ( Map, area_right );
		if ( centroid > 0 ) {
		    Vect_read_line ( Map, NULL, RCats, centroid);
		}
	    } 
	}

	for ( i = 0; i < Cats->n_cats; i++ ) {
	    if ( Cats->field[i] == options.field ) {
		idx = find_cat(Cats->cat[i]);
		if (  options.option == O_COUNT ) {
		    Values[idx].count1++;
		} else if ( options.option == O_LENGTH && (type & GV_LINES) ) {
		    /* Calculate line length */
		    len = length (Points->n_points, Points->x, Points->y);
		    Values[idx].d1 += len;
		} else if ( options.option == O_COOR && (type & GV_POINTS) ) {
		    /* overwrite by last one, count is used in update */ 
		    Values[idx].d1 = Points->x[0];
		    Values[idx].d2 = Points->y[0];
		    Values[idx].d3 = Points->z[0];
		    Values[idx].count1++;
		} else if ( options.option == O_SIDES && type == GV_BOUNDARY ) {
		    
                    read_side_cats ( LCats, &(Values[idx].i1), &(Values[idx].count1) );
                    read_side_cats ( RCats, &(Values[idx].i2), &(Values[idx].count2) );
		}
		found = 1;
	    }
	}

	if ( !found ) {  /* Values for no category (cat = -1) are reported at the end */
	    idx = find_cat(-1);
	    if (  options.option == O_COUNT ) {
		Values[idx].count1++;
	    } else if ( options.option == O_LENGTH && (type & GV_LINES) ) {
		len = length (Points->n_points, Points->x, Points->y);
		Values[idx].d1 += len;
	    } else if ( options.option == O_COOR && (type & GV_POINTS) ) {
		Values[idx].d1 = Points->x[0];
		Values[idx].d2 = Points->y[0];
		Values[idx].d3 = Points->z[0];
		Values[idx].count1++;
	    } else if ( options.option == O_SIDES && type == GV_BOUNDARY ) {
		read_side_cats ( LCats, &(Values[idx].i1), &(Values[idx].count1) );
		read_side_cats ( RCats, &(Values[idx].i2), &(Values[idx].count2) );
	    }
	}
    }

    return 0;
}

