/* plot1() - Level One vector reading */

#include "gis.h"
#include "Vect.h"
#include "display.h"
#include "raster.h"
#include "plot.h"
#include "symbol.h"
#include "glocale.h"

int plot1 (
    struct Map_info *Map, int type, int area, 
    struct cat_list *Clist, int color, int fcolor, int chcat, SYMBOL *Symb, int size, int id_flag)
{
    int i, j, k, ltype, nlines, line;
    double *x, *y, xd, yd, xd0, yd0;
    struct line_pnts *Points, *PPoints;
    struct line_cats *Cats;
    double msize;
    SYMBPART *part;
    SYMBCHAIN *chain;
    int x0, y0, xp, yp;

    msize = size * ( D_d_to_u_col(2) - D_d_to_u_col(1) ); /* do it better */
    
    Points = Vect_new_line_struct ();
    PPoints = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    Vect_rewind ( Map );
    
    /* Is it necessary to reset line/label color in each loop ? */

    if ( color > -1 ) R_color(color) ;

    if ( Vect_level ( Map ) >= 2 )
	nlines = Vect_get_num_lines ( Map );

    line = 0;
    while (1) {
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
		fprintf (stderr, _("\nERROR: vector file - can't read\n" ));
		return -1;
	    case -2: /* EOF */
		return  0;
	    }
	}

	if ( !(type & ltype) ) continue;

	if ( chcat ) {
	     int found = 0;

	     if ( id_flag ) { /* use line id */
		 if ( !(Vect_cat_in_cat_list ( line, Clist)) )
		     continue;
	     } else {
		 for ( i = 0; i < Cats->n_cats; i++ ) {
		     if ( Cats->field[i] == Clist->field && Vect_cat_in_cat_list ( Cats->cat[i], Clist) ) {
			 found = 1;
                         break;
                     }
                 }
                 if (!found) continue;
	     }
	}
	
	x = Points->x;
	y = Points->y;

        if ( (ltype & GV_POINTS) && Symb != NULL ) {
	    /* Note: this should go to some library function */
	    G_plot_where_xy(x[0], y[0], &x0, &y0);  
 
            for ( i = 0; i < Symb->count; i++ ) {
                part = Symb->part[i];

	        switch ( part->type ) {
		    case S_POLYGON:
			/* Note: it may seem to be strange to calculate coor in pixels, then convert
			 *       to E-N and plot. I hope that we get some D_polygon later. */
			if ( (part->fcolor.color == S_COL_DEFAULT && fcolor > -1) ||
			      part->fcolor.color == S_COL_DEFINED ) 
			{
			    if ( part->fcolor.color == S_COL_DEFAULT )
				R_color(fcolor);
			    else
				R_RGB_color ( part->fcolor.r, part->fcolor.g, part->fcolor.b );

			    Vect_reset_line ( PPoints );

			    for ( j = 0; j < part->count; j++ ) { /* Construct polygon */
				chain = part->chain[j];
				for ( k = 0; k < chain->scount; k++ ) { 
				    xp  = x0 + chain->sx[k];
				    yp  = y0 - chain->sy[k];
				    G_plot_where_en ( xp, yp, &xd, &yd );
				    Vect_append_point ( PPoints, xd, yd, 0);
				}
				if ( j == 0 ) {
				    xd0 = PPoints->x[0];
				    yd0 = PPoints->y[0];
				} else {
				    Vect_append_point ( PPoints, xd0, yd0, 0);
				}
			    }
			    
			    G_plot_polygon ( PPoints->x, PPoints->y, PPoints->n_points);
			}
			if ( (part->color.color == S_COL_DEFAULT && color > -1 ) ||
			      part->color.color == S_COL_DEFINED  ) 
			{
			    if ( part->color.color == S_COL_DEFAULT ) {
				R_color(color);
			    } else {
			        R_RGB_color ( part->color.r, part->color.g, part->color.b );
			    }

			    for ( j = 0; j < part->count; j++ ) { 
				chain = part->chain[j];
				for ( k = 0; k < chain->scount; k++ ) { 
				    xp  = x0 + chain->sx[k];
				    yp  = y0 - chain->sy[k];
				    if ( k == 0 ) D_move_abs ( xp, yp );
				    else D_cont_abs ( xp, yp );

				}
			    }
			    
			}
			
                        break;
                    case S_STRING: 
			if ( part->color.color == S_COL_NONE ) break;
			else if ( part->color.color == S_COL_DEFAULT ) R_color(color) ;
			else R_RGB_color ( part->color.r, part->color.g, part->color.b );
			    
			chain = part->chain[0];

                        for ( j = 0; j < chain->scount; j++ ) { 
			    xp  = x0 + chain->sx[j];
			    yp  = y0 - chain->sy[j];
			    if ( j == 0 ) D_move_abs ( xp, yp );
			    else D_cont_abs ( xp, yp );

                        }
                        break;
                }
            }
            if (color > -1) R_color(color) ; /* Reset color */
        } else if (color > -1 ) {
	    if ( Points->n_points == 1 ) { /* line with one coor */
	        G_plot_line(x[0], y[0], x[0], y[0]);
	    } else {
		for(i=1; i < Points->n_points; i++) {
		    G_plot_line(x[0], y[0], x[1], y[1]);
		    x++;
		    y++;
		  }
	    }
	}
    }

    Vect_destroy_line_struct (Points);
    Vect_destroy_cats_struct (Cats);
    
    return 0; /* not reached */
}


