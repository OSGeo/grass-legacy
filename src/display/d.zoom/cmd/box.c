#include <string.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"

static int max(int,int);

int make_window_box ( struct Cell_head *window, double magnify, char pan)
{
    char buffer[64] ;
    int screen_x, screen_y ;
    double ux1, uy1 ;
    double ux2, uy2 ;
    double north,south,east,west;
    int len_n, len_s, len_e, len_w;
    int t;
    int button ;
    int cur_screen_x, cur_screen_y ;
    int quitonly;  /* required if user just wants to quit d.zoom */
    int prebutton; /* which previous button was pressed? */
    int reached;
    
    screen_y = get_map_top() ;
    screen_x = get_map_left() ;
    quitonly=0;
    
    fprintf(stderr, "\n\n");
    fprintf(stderr, "Buttons:\n") ;
    fprintf(stderr, "Left:   Establish a corner to zoom in\n") ;
    fprintf(stderr, "Middle: Unzoom stepwise\n") ;
    fprintf(stderr, "Right:  Accept region/Quit\n\n") ;

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
	reached = 0;
	R_get_location_with_box(cur_screen_x, cur_screen_y, &screen_x, &screen_y, &button) ;
	button &= 0xf;
/*	fprintf (stdout,"\nscreen_x: %d screen_y: %d\n",screen_x,screen_y);*/

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
		quitonly=0;
		break ;
	case 2:
		if(U_east > window->east ||
		   U_west < window->west ||
		   U_south < window->south ||
		   U_north > window->north)
		{
			if(pan)
			    make_window_center(window, magnify, ux2, uy2);
			else
			    make_window_center(window, magnify, -1.0, -1.0);
		}
		else
		{
			reached = 1;
			fprintf(stderr, "** Reached at region boundary **\n");
		}
	        button=3;
	        quitonly=2;   /* leave after unzoom */
	        break;
	case 3:
		if (prebutton == 1)
		  quitonly=0; /* box opening */
		else
		  quitonly=1; /* quit only, no action*/
		break;
	}

	if(quitonly==2)
	   prebutton = 2;
	else
	   prebutton = button;

	if(prebutton==1)
	{
	   north = uy1;
	   east = ux1;

	   G_limit_north (&north, window->proj);
	   G_limit_east (&east, window->proj);

	   t = (window->north - north) / window->ns_res;
	   north = window->north - (t) * window->ns_res;
	
	   t = (window->east - east) / window->ew_res;
	   east = window->east - (t) * window->ew_res;

           strcpy (buffer, "?");
           G_format_northing(north, buffer, window->proj)  ;
           len_n = max (len_n, strlen(buffer));
           fprintf(stderr,"%-*s(N)  ", len_n, buffer);

           strcpy (buffer, "?");
           G_format_easting(east, buffer, window->proj)  ;
           len_e = max (len_e, strlen(buffer));
           fprintf(stderr,"%-*s(E)  ", len_e, buffer);

           fprintf (stderr,"\r");
           fflush (stderr);
	}
    } while (button != 3) ;

    if(quitonly != 1)
    {
	if(prebutton == 2)
	{
	   east = window->east;
	   west = window->west;
	   south = window->south;
	   north = window->north;
	}else{
	   north = uy1>uy2?uy1:uy2 ;
	   south = uy1<uy2?uy1:uy2 ;
	   west  = ux1<ux2?ux1:ux2 ;
	   east  = ux1>ux2?ux1:ux2 ;
	
	   G_limit_north (&north, window->proj);
	   G_limit_south (&south, window->proj);
	   G_limit_east (&east, window->proj);
	   G_limit_west (&west, window->proj);
	
	   t = (window->north - north) / window->ns_res;
	   north = window->north - (t) * window->ns_res;
	
	   t = (south - window->south) / window->ns_res;
	   south = window->south + (t) * window->ns_res;
	
	   t = (window->east - east) / window->ew_res;
	   east = window->east - (t) * window->ew_res;
	
	   t = (west - window->west) / window->ew_res;
	   west = window->west + (t) * window->ew_res;
	}

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

    	window->north = north;
    	window->south = south;
    	window->east  = east ;
    	window->west  = west ;
    }

    fprintf (stderr, "\n\n");

    return quitonly;
}

static int max(int a,int b)
{
    return a>b?a:b;
}
