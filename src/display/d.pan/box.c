#include <string.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "local_proto.h"

static int max(int a,int b) {return a>b?a:b;}

int make_window_box (struct Cell_head *window)
{
    char buffer[64] ;
    int screen_x, screen_y ;
    double ux1, uy1 ;
    double ux2, uy2 ;
    double north,south,east,west;
    int len_n, len_s, len_e, len_w;
    int t;
    int button ;

    screen_y = get_map_top() ;
    screen_x = get_map_left() ;

    fprintf(stderr, "\n\n");
    fprintf(stderr, "Buttons:\n") ;
    fprintf(stderr, "Left:   Establish a corner\n") ;
    fprintf(stderr, "Middle: Check coordinates\n") ;
    fprintf(stderr, "Right:  Accept region\n\n") ;

    ux1 = D_get_u_west() ;
    uy1 = D_get_u_south() ;

    G_limit_south (&uy1, window->proj);
    G_limit_north (&uy1, window->proj);
    G_limit_east  (&ux1, window->proj);
    G_limit_west  (&ux1, window->proj);

    cur_screen_y = (int) D_u_to_d_row (uy1);
    cur_screen_x = (int) D_u_to_d_col (ux1);

    screen_x = cur_screen_x + 10 ;
    screen_y = cur_screen_y + 10 ;


    len_n = len_s = len_e = len_w = 0;
    do
    {
	R_get_location_with_box(cur_screen_x, cur_screen_y, &screen_x, &screen_y, &button) ;
	button &= 0xf;

	ux2 = D_d_to_u_col((double)screen_x)  ;
	uy2 = D_d_to_u_row((double)screen_y)  ;

	G_limit_south (&uy2, window->proj);
	G_limit_north (&uy2, window->proj);
	G_limit_east  (&ux2, window->proj);
	G_limit_west  (&ux2, window->proj);

	screen_y = (int) D_u_to_d_row (uy2);
	screen_x = (int) D_u_to_d_col (ux2);

	switch(button)
	{
	case 1:
		cur_screen_x = screen_x ;
		cur_screen_y = screen_y ;
		ux1 = ux2 ;
		uy1 = uy2 ;
		break ;
	case 2:
	case 3: break;
	}


	north = uy1>uy2?uy1:uy2 ;
	south = uy1<uy2?uy1:uy2 ;
	west  = ux1<ux2?ux1:ux2 ;
	east  = ux1>ux2?ux1:ux2 ;

	G_limit_east (&east, window->proj);
	G_limit_west (&west, window->proj);
	G_limit_north (&north, window->proj);
	G_limit_south (&south, window->proj);

	t = (window->north - north) / window->ns_res;
	north = window->north - (t) * window->ns_res;

	t = (south - window->south) / window->ns_res;
	south = window->south + (t) * window->ns_res;

	t = (window->east - east) / window->ew_res;
	east = window->east - (t) * window->ew_res;

	t = (west - window->west) / window->ew_res;
	west = window->west + (t) * window->ew_res;

	strcpy (buffer, "?");
	G_format_northing(north, buffer, window->proj)  ;
	len_n = max (len_n, strlen(buffer));
	fprintf(stderr,"north: %-*s  ", len_n, buffer);

	strcpy (buffer, "?");
	G_format_northing(south, buffer, window->proj)  ;
	len_s = max (len_s, strlen(buffer));
	fprintf(stderr,"south: %-*s  ", len_s, buffer);

	strcpy (buffer, "?");
	G_format_easting(east, buffer, window->proj)  ;
	len_e = max (len_e, strlen(buffer));
	fprintf(stderr,"east: %-*s  ", len_e, buffer);

	strcpy (buffer, "?");
	G_format_easting(west, buffer, window->proj)  ;
	len_w = max (len_w, strlen(buffer));
	fprintf(stderr,"west: %-*s  ", len_w, buffer);

	fprintf (stderr,"\r");
	fflush (stderr);

    } while (button != 3) ;

    fprintf (stderr, "\n\n");

    window->north = north;
    window->south = south;
    window->east  = east ;
    window->west  = west ;
}

