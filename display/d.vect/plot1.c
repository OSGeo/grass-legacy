/* plot1() - Level One vector reading */

#include "gis.h"
#include "Vect.h"
#include "display.h"
#include "raster.h"
#include "plot.h"


int plot1 (
    struct Map_info *Map, int type, int area, 
    struct cat_list *Clist, int color, int fcolor, int chcat, int icon, int size, int id_flag)
{
    int i, ltype, nlines, line;
    double *x, *y;
    struct line_pnts *Points;
    struct line_cats *Cats;
    int cat;
    double msize;

    msize = size * ( D_d_to_u_col(2) - D_d_to_u_col(1) ); /* do it better */
    
    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    Vect_rewind ( Map );
    
    /* Is it necessary to reset line/label color in each loop ? */

    R_color(color) ;

    if ( Vect_level ( Map ) >= 2 )
	nlines = Vect_get_num_lines ( Map );

    line = 0;
    while (1)
     {
	if ( Vect_level ( Map ) >= 2 ) { 
	    line++;
	    if ( line > nlines ) return 0;
	    if ( !Vect_line_alive (Map, line) ) continue;
	    ltype =  Vect_read_line (Map, Points, Cats, line);   
	} else {
	    ltype =  Vect_read_next_line (Map, Points, Cats);   
	    switch ( ltype )
	    {
	    case -1:
		fprintf (stderr, "\nERROR: vector file - can't read\n" );
		return -1;
	    case -2: /* EOF */
		return  0;
	    }
	}

	if ( !(type & ltype) ) continue;

	if ( chcat ) {
	     if ( id_flag ) { /* use line id */
		 if ( !(Vect_cat_in_cat_list ( line, Clist)) )
		     continue;
	     } else {
		 if ( Vect_cat_get(Cats, Clist->field, &cat) ) { 
		     if ( !(Vect_cat_in_cat_list (cat, Clist)) )
			 continue;
		 } else {
		     continue;
		 } 
	     }
	}
	
	x = Points->x;
	y = Points->y;

        if ( ltype & GV_POINTS )
	  {
	    G_plot_icon(x[0], y[0], icon, 0, msize);  
	  }
	else if ( Points->n_points == 1 ) /* line with one coor */
	  {
	    G_plot_line(x[0], y[0], x[0], y[0]);
	  }
	else
	  {
	    for(i=1; i < Points->n_points; i++)
	      {
	        G_plot_line(x[0], y[0], x[1], y[1]);
	        x++;
	        y++;
	      }
	  }
      }
	

    Vect_destroy_line_struct (Points);
    Vect_destroy_cats_struct (Cats);
    
    return 0; /* not reached */
}


