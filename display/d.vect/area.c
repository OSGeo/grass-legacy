#include "gis.h"
#include "Vect.h"
#include "display.h"
#include "raster.h"
#include "plot.h"


int darea ( struct Map_info *Map, struct cat_list *Clist, int bcolor, int fcolor ) {
    int i, ltype, num, area, isle, n_isles, n_points;
    double xl, yl;
    struct line_pnts *Points, *IPoints;
    struct line_cats *Cats;
    int cat, centroid;
    //struct Plus_head *Plus;
    //P_AREA_2D *AREA;
    
    G_debug (1, "display areas:");
    //Plus = &(Map->plus);
    Points = Vect_new_line_struct ();
    IPoints = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();

	
    num = V2_num_areas(Map);
    G_debug (1, "n_areas = %d", num);
    
    for ( area = 1; area <= num; area++ ) {
        G_debug (3, "area = %d", area);

        if ( Clist->n_ranges > 0)
          { 
             centroid = Vect_get_area_centroid ( Map, area ); 
             G_debug (3, "centroid = %d", centroid);
	     if ( centroid < 1 ) continue;
	     V2_read_line (Map, Points, Cats, centroid );
             if ( Vect_cat_get(Cats, Clist->field, &cat) ) { 
	         if ( !(Vect_cat_in_cat_list (cat, Clist)) )
                     continue;
	     } else {
		 continue;
	     } 
          }

        /* fill */	
	Vect_get_area_points ( Map, area, Points );   
        G_debug (3, "n_points = %d", Points->n_points);
  
	n_points = Points->n_points;
	xl = Points->x[n_points-1];
	yl = Points->y[n_points-1];
	n_isles = Vect_get_area_num_isles ( Map, area );   
        for ( i = 0; i < n_isles; i++) {
	    isle = Vect_get_area_isle ( Map, area, i );   
	    Vect_get_isle_points ( Map, isle, IPoints );
	    Vect_append_points ( Points, IPoints, GV_FORWARD);
	    Vect_append_point ( Points, xl, yl ); /* ??? */
	}
	
        R_standard_color(fcolor) ;
	G_plot_polygon ( Points->x, Points->y, Points->n_points);
	
	/* boundary */
	Vect_get_area_points ( Map, area, Points );   
        R_standard_color(bcolor) ;
	for ( i = 0; i < Points->n_points - 1; i++) { 
            G_plot_line (Points->x[i], Points->y[i], Points->x[i+1], Points->y[i+1]);
	}
        for ( i = 0; i < n_isles; i++) {
	    isle = Vect_get_area_isle ( Map, area, i );   
	    Vect_get_isle_points ( Map, isle, Points );
	    for ( i = 0; i < Points->n_points - 1; i++) { 
		G_plot_line (Points->x[i], Points->y[i], Points->x[i+1], Points->y[i+1]);
	    }
	}
    }

    Vect_destroy_line_struct (Points);
    Vect_destroy_cats_struct (Cats);
    
    return 0;
}


 
