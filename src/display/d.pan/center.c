#include <string.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "local_proto.h"

static int max (int a, int b) {return a>b?a:b;}

int make_window_center (struct Cell_head *window, double mag)
{
    char buffer[64] ;
    int screen_x, screen_y ;
    double north,east;
    double east_west, north_south;
    int len_n, len_e;
    int t;
    int button;

    screen_y = (get_map_top() + get_map_bot()) / 2;
    screen_x = (get_map_left() + get_map_rite()) / 2;

    fprintf(stderr, "\n\n");
    fprintf(stderr, "Buttons:\n") ;
    fprintf(stderr, "Left:   Where am I?\n") ;
#ifdef ANOTHER_BUTTON
    fprintf(stderr, "Middle: Quit\n") ;
    fprintf(stderr, "Right:  Mark point to be at the center of the new region\n\n") ;
#else
    fprintf(stderr, "Middle: Mark point to be at the center of the new region\n") ;
    fprintf(stderr, "Right:  Quit\n\n") ;
#endif

    len_n = len_e = 0;
    do
    {
	R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
	button &= 0xf;

	east  = D_d_to_u_col((double)screen_x)  ;
	north = D_d_to_u_row((double)screen_y)  ;

	G_limit_south (&north, window->proj);
	G_limit_north (&north, window->proj);
	G_limit_east  (&east, window->proj);
	G_limit_west  (&east, window->proj);

	screen_y = (int) D_u_to_d_row (north);
	screen_x = (int) D_u_to_d_col (east);

	t = (window->north - north) / window->ns_res;
	north = window->north - (t+.5) * window->ns_res;

	t = (window->east - east) / window->ew_res;
	east = window->east - (t+.5) * window->ew_res;

	strcpy (buffer, "?");
	G_format_northing(north, buffer, window->proj)  ;
	len_n = max (len_n, strlen(buffer));
	fprintf(stderr,"north: %-*s  ", len_n, buffer);

	strcpy (buffer, "?");
	G_format_easting(east, buffer, window->proj)  ;
	len_e = max (len_e, strlen(buffer));
	fprintf(stderr,"east: %-*s  ", len_e, buffer);

	fprintf (stderr,"\r");
	fflush (stderr);

    } while (button == LEFTB) ;

    fprintf (stderr, "\n\n");

    if (button == RIGHTB)
	return 1;

/* added for panning */
    east_west = (window->east - window->west)/mag;
    window->east =  east + east_west/2;
    window->west =  east - east_west/2;
    if (window->proj == PROJECTION_LL){
	if (east_west>360){
	    window->east =  east + 180;
	    window->west =  east - 180;
	}
	window->east = G_adjust_easting (window->east, window);
    }

    north_south = (window->north - window->south)/mag;
    window->north =  north + north_south/2;
    window->south =  north - north_south/2;
    G_limit_south (&window->south, window->proj);
    G_limit_north (&window->north, window->proj);
/* added for panning */

    strcpy (buffer, "?");
    G_format_northing(window->north, buffer, window->proj)  ;
    fprintf(stderr,"north: %s  ", buffer);

    strcpy (buffer, "?");
    G_format_northing(window->south, buffer, window->proj)  ;
    fprintf(stderr,"south: %s  ", buffer);

    strcpy (buffer, "?");
    G_format_easting(window->east, buffer, window->proj)  ;
    fprintf(stderr,"east: %s  ", buffer);

    strcpy (buffer, "?");
    G_format_easting(window->west, buffer, window->proj)  ;
    fprintf(stderr,"west: %s  ", buffer);

    fprintf(stderr, "\n\n");

    return 0;
}

