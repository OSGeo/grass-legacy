/* plot1() - Level One vector reading */

#include "gis.h"
#include "Vect.h"
#include "display.h"
#include "raster.h"
#include "plot.h"


int dir ( struct Map_info *Map, int type, struct cat_list * Clist, int chcat )
{
    int    i, ltype, dsize;
    double len, x, y, angle, msize;
    struct line_pnts *Points;
    struct line_cats *Cats;
    int cat;
    
    G_debug (1, "display direction:");
    dsize = 5;
    msize = dsize * ( D_d_to_u_col(2) - D_d_to_u_col(1) ); /* do it better */ 
    
    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    Vect_rewind ( Map );
    
    while (1)
     {
	ltype =  Vect_read_next_line (Map, Points, Cats);   
        switch ( ltype )
	{
	case -1:
	    fprintf (stderr, "\nERROR: vector file - can't read\n" );
	    return -1;
	case -2: /* EOF */
	    return  0;
	}

	if ( !(type & ltype & GV_LINES )  ) continue;

        if ( chcat )
          { 
             if ( Vect_cat_get(Cats, Clist->field, &cat) ) { 
	         if ( !(Vect_cat_in_cat_list (cat, Clist)) )
                     continue;
	     } else {
		 continue;
	     } 
          }
	
	len = Vect_line_length ( Points );

	Vect_point_on_line ( Points, len/2, &x, &y, NULL, &angle, NULL );

	G_debug (3, "plot direction: %f, %f", x, y);
        G_plot_icon(x, y, G_ICON_ARROW, angle, msize);
      }
	

    Vect_destroy_line_struct (Points);
    Vect_destroy_cats_struct (Cats);
    
    return 0; /* not reached */
}


