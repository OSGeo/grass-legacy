#include <stdio.h>
#include <stdlib.h>
#include "Vect.h"
#include "global.h"

int read_lines(struct Map_info *Map, struct Categories *Labels)

{
	int idx, cat_no, n_points;
	register int line_num;
	P_LINE *Lines;
	double len,*X, *Y;
	struct line_pnts *Points;
	char *lab;

	/* Initialize the Point struct */
        Points = Vect_new_line_struct();

	/* Cycle through all lines */
	for (line_num = 1 ; line_num <= Map->n_lines ; line_num++)
	{
	    Lines = &(Map->Line[line_num]);
	    if (Lines->type != LINE) continue;
		    
	    /* get the category number for line_num */
	    cat_no = Map->Att[Lines->att].cat;

	    idx = find_cat( cat_no);
	    
	    if ( options.option == O_LENGTH ) {
		if (0 > V1_read_line (Map, Points, Lines->offset)) {
		    fprintf (stderr, "Out of Memory\n");
		    return ERROR;
		}
	    }

	    if ( options.option == O_LENGTH ) {
		/* Calculate line length */
        	n_points = Points->n_points;
		X = Points->x;
		Y = Points->y;
		len = length(n_points,X,Y);
	    }	
	    
	    if ( options.option == O_LABEL ) {
		
	    }

	    if ( idx < 0) {
		if ( vstat.rcat < vstat.alloc )
		    switch (options.option) {
			case O_CAT: 
			case O_COUNT:
			    list_ci[vstat.rcat].cat = cat_no;
			    list_ci[vstat.rcat].i1 = 1;
			    vstat.rcat++;
			    break;
			case O_LENGTH:			    
			    list_cd[vstat.rcat].cat = cat_no;
			    list_cd[vstat.rcat].d1 = len;
			    vstat.rcat++;
			    break;
			case O_LABEL:
			    list_cc[vstat.rcat].cat = cat_no;
			    lab = G_get_cat(cat_no, Labels);
			    list_cc[vstat.rcat].c1 = (char *) G_malloc ( strlen(lab)+1 );
			    strcpy ( list_cc[vstat.rcat].c1, G_get_cat(cat_no, Labels) );
			    vstat.rcat++;
			    break;			
		    }	    
	    } else {
	        switch (options.option) {
		    case O_CAT: 
		    case O_COUNT:
	    		list_ci[idx].i1++;
			break;
		    case O_LENGTH:
	    		list_cd[idx].d1 += len;
			break;
		}		
	    }
	}

	return OK;
}

