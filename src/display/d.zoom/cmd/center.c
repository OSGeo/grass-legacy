#include "gis.h"

make_window_center (window)
    struct Cell_head *window ;
{
    char buffer[64] ;
    int screen_x, screen_y ;
    double north,east;
    double east_west;
    int len_n, len_e;
    int t;
    int button ;
    double D_d_to_u_col()  ;
    double D_d_to_u_row()  ;
    double D_u_to_d_col()  ;
    double D_u_to_d_row()  ;

    screen_y = (get_map_top() + get_map_bot()) / 2;
    screen_x = (get_map_left() + get_map_rite()) / 2;

    fprintf(stderr, "\n\n");
    fprintf(stderr, "Buttons:\n") ;
    fprintf(stderr, "Left:   Where am I?\n") ;
    fprintf(stderr, "Middle: Where am I?\n") ;
    fprintf(stderr, "Right:  Mark longitude to be at the center of the region\n\n") ;

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

    } while (button != 3) ;

    fprintf (stderr, "\n\n");

    east_west = window->east - window->west;
    window->east =  east + east_west/2;
    window->west =  east - east_west/2;

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

    fprintf (stderr, "\n\n");
}

static
max(a,b)
{
    return a>b?a:b;
}
