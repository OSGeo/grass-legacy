#include "gis.h"
#include "Vect.h"
#include "display.h"
#include "raster.h"
#include "plot.h"


int darea ( struct Map_info *Map, struct cat_list *Clist, int bcolor, int fcolor, 
	     int chcat, int id_flag ) {
    int    i, j, num, area, isle, n_isles, n_points;
    double xl, yl;
    struct line_pnts *Points, *IPoints;
    struct line_cats *Cats;
    int cat, centroid;
    
    G_debug (1, "display areas:");
    Points = Vect_new_line_struct ();
    IPoints = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();

	
    num = Vect_get_num_areas(Map);
    G_debug (2, "n_areas = %d", num);
    
    for ( area = 1; area <= num; area++ ) {
        G_debug (3, "area = %d", area);

	if ( !Vect_area_alive (Map, area) ) continue;
        if ( chcat )
          { 
	     if ( id_flag ) {
		 if ( !(Vect_cat_in_cat_list (area, Clist)) )
		     continue;
	     } else {
		 centroid = Vect_get_area_centroid ( Map, area ); 
		 G_debug (3, "centroid = %d", centroid);
		 if ( centroid < 1 ) continue;
		 Vect_read_line (Map, Points, Cats, centroid );
		 if ( Vect_cat_get(Cats, Clist->field, &cat) ) { 
		     if ( !(Vect_cat_in_cat_list (cat, Clist)) )
			 continue;
		 } else {
		     continue;
		 } 
	     }
          }
        G_debug (3, "display area %d", area);

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
	    Vect_append_point ( Points, xl, yl, 0 ); /* ??? */
	}
	
	if ( fcolor > -1 ) {
            R_color(fcolor) ;
	    G_plot_polygon ( Points->x, Points->y, Points->n_points);
	}
	
	/* boundary */
	if ( bcolor > -1 ) {
	    Vect_get_area_points ( Map, area, Points );   
	    R_color(bcolor) ;
	    for ( i = 0; i < Points->n_points - 1; i++) { 
		G_plot_line (Points->x[i], Points->y[i], Points->x[i+1], Points->y[i+1]);
	    }
	    for ( i = 0; i < n_isles; i++) {
		isle = Vect_get_area_isle ( Map, area, i );   
		Vect_get_isle_points ( Map, isle, Points );
		for ( j = 0; j < Points->n_points - 1; j++) { 
		    G_plot_line (Points->x[j], Points->y[j], Points->x[j+1], Points->y[j+1]);
		}
	    }
	}
    }

    Vect_destroy_line_struct (Points);
    Vect_destroy_cats_struct (Cats);
    
    return 0;
}


 
