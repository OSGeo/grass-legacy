/*
*  Written by the Radim Blazek
*/

#include "Vect.h"
#include "gis.h"
#include "libtrans.h"

int 
transform_digit_file (struct Map_info *Old, struct Map_info *New)
{
    int    i, type;
    static struct line_pnts *Points;

    Points = Vect_new_line_struct ();
    
    while (1) {
	type = Vect_read_next_line (Old, Points);
        
	if ( type == -1 ) /* error */
	    return 0;
	
	if ( type == -2 ) /* EOF */
	    return 1;
	
	for ( i = 0; i < Points->n_points; i++ ) {
            transform_a_into_b( Points->x[i], Points->y[i], &(Points->x[i]), &(Points->y[i]) );
	}
	
	Vect_write_line (New,  type, Points);
    }
}
