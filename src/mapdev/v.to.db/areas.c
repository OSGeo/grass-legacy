#include <stdio.h>
#include <stdlib.h>
#include "Vect.h"
#include "global.h"

int read_areas(struct Map_info *Map, struct Categories *Labels)

{
	int idx, cat_no, n_points, i, island;
	register int area_num;
	P_AREA *Areas;
	double area,*X, *Y;
	struct line_pnts *Points;
	char *lab;

	/* Initialize the Point struct */
        Points = Vect_new_line_struct();

	/* Cycle through all areas */
	for (area_num = 1 ; area_num <= Map->n_areas ; area_num++)
	{
	    /* get the category number for area_num */
	    cat_no = Map->Att[Map->Area[area_num].att].cat;

	    idx = find_cat( cat_no);
	    
	    if ( options.option == O_AREA ) {
		Vect_get_area_points( Map, area_num, Points);
	    }

	    if ( options.option == O_AREA ) {
		/* Calculate area */
		area = G_area_of_polygon(Points->x, Points->y, Points->n_points);
		if (Map->Area[area_num].n_isles) {
            	    for (i=0; i < Map->Area[area_num].n_isles; i++) {
                	island = Map->Area[area_num].isles[i];
			Vect_get_isle_points(Map, Map->Area[area_num].isles[i], Points);
			area -= G_area_of_polygon(Points->x, Points->y, Points->n_points);
                     }
		}
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
			case O_AREA:			    
			    list_cd[vstat.rcat].cat = cat_no;
			    list_cd[vstat.rcat].d1 = area;
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
		    case O_AREA:
	    		list_cd[idx].d1 += area;
			break;
		}		
	    }
	}

	return OK;
}

