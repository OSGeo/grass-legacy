/* ***************************************************************
 * *
 * * MODULE:       v.clean
 * * 
 * * AUTHOR(S):    Radim Blazek
 * *               
 * * PURPOSE:      Clean lines
 * *               
 * * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 * *
 * *               This program is free software under the 
 * *               GNU General Public License (>=v2). 
 * *               Read the file COPYING that comes with GRASS
 * *               for details.
 * *
 * **************************************************************/
#include <stdlib.h> 
#include "gis.h"
#include "Vect.h"

int 
rmdac ( struct Map_info *Out )
{
        int    i, type, area, ndupl, nlines;

	nlines = Vect_get_num_lines (Out);

        G_debug (1, "nlines =  %d", nlines );

	ndupl = 0;
	fprintf (stderr, "Duplicate area centroids: %5d", ndupl ); 
	for ( i = 1; i <= nlines; i++ ){ 
	    if ( !Vect_line_alive ( Out, i ) ) continue;

	    type = Vect_read_line (Out, NULL, NULL, i);
	    if ( !(type & GV_CENTROID) ) continue;

	    area = Vect_get_centroid_area ( Out, i );
            G_debug (3, "  area = %d", area);
	    
	    if ( area < 0 ) {
		Vect_delete_line (Out, i); 
		ndupl++;
		
	        fprintf (stderr, "\rDuplicate area centroids: %5d", ndupl ); 
		fflush ( stderr );
	    }
	}
	fprintf (stderr, "\n" ); 

	return 1;
}


