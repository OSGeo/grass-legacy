#include <stdio.h>
#include <unistd.h>
#include "gis.h"
#include "Vect.h"
#include "raster.h"
#include "display.h"
#include "colors.h"
#include "global.h"
#include "proto.h"


/* Display points */
int display_points ( struct line_pnts *Points, int color )
{
    int i;
    
    G_debug (2, "display_points()");

    R_color( color ) ;

    for(i=1; i < Points->n_points; i++) {
        G_plot_line ( Points->x[i-1], Points->y[i-1], Points->x[i], Points->y[i]);
    }
    R_flush();

    return 1;
}

/* Display vector line */
int display_line ( int line )
{
    int type;
    static struct line_pnts *Points;
    static struct line_cats *Cats;
    static int first = 1;
    
    G_debug (2, "display_line()");

    if ( first ) {
        Points = Vect_new_line_struct ();
        Cats = Vect_new_cats_struct ();
	first = 0;
    }

    if ( !Vect_line_alive ( &Map, line ) ) return 0;

    type =  Vect_read_line ( &Map, Points, Cats, line); 

    display_points ( Points, WHITE );

    return 1;
}

/* Display vector map */
int display_map ( void )
{
    int i, n;
    
    G_debug (2, "display_map()");

    n = Vect_get_num_lines ( &Map );
    for(i=1; i <= n; i++) {
	display_line ( i );
    }

    return 1;
}
