#include <unistd.h>
#include "gis.h"
#include "Vect.h"
#include "display.h"
#include "vectpoints.h"
#include "globals.h"

int plot (char *name, char *mapset, struct line_pnts *Points)
{
    double *x, *y;
    int i, np;
    int line, nlines;
    struct Cell_head window;
    struct Map_info P_map;
    char msg[100];

    Vect_set_open_level (2);
    Vect_set_fatal_error ( GV_FATAL_RETURN );
    
    if ( 2 > Vect_open_old (&P_map, name, mapset))
    {
	return -1;
    }

    G_get_set_window (&window);

    G_setup_plot ( D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	           D_move_abs, D_cont_abs);

    nlines = Vect_get_num_lines (&P_map);


    for ( line = 1; line <= nlines; line++ ) { 
        Vect_read_line(&P_map, Points, NULL, line );

	np = Points->n_points;
	x = Points->x;
	y = Points->y;
	for(i=1; i < np; i++)
	{
	    G_plot_line(x[0], y[0], x[1], y[1]);
	    x++;
	    y++;
	}
    }

    Vect_close ( &P_map );
    return 0;
}
