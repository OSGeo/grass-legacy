/* plot1() - Level One vector reading */

#include "gis.h"
#include "Vect.h"
#include "display.h"
#include "raster.h"
#include "plot.h"


int plot1 (
    struct Map_info *Map, int type, int area, 
    struct cat_list *Clist, int color, int fcolor)
{
    int i, ltype;
    double *x, *y;
    struct line_pnts *Points;
    struct line_cats *Cats;
    int cat;
    
    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    Vect_rewind ( Map );
    
    /* Is it necessary to reset line/label color in each loop ? */

    R_standard_color(color) ;

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

	if ( !(type & ltype) ) continue;
	
        if ( Clist->n_ranges > 0)
          {
             Vect_cat_get(Cats, Clist->field, &cat);
	     if ( !(Vect_cat_in_cat_list (cat, Clist)) )
                 continue;
          }
	
	x = Points->x;
	y = Points->y;

	
        if ( ltype & ELEMENT_TYPE_DOT )
	  {
	    G_plot_line(x[0], y[0], x[0], y[0]);
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


