#include "gis.h"

make_window_box (window)
    struct Cell_head *window ;
{
    char buffer[64] ;
    int screen_x, screen_y ;
    double ux, uy ;
    double north,south,east,west;
    double mx, my, dx, dy;
    int len_n, len_s, len_e, len_w;
    int t;
    int button ;
    double D_d_to_u_col()  ;
    double D_d_to_u_row()  ;
    double D_u_to_d_col()  ;
    double D_u_to_d_row()  ;
    double D_get_u_west() ;
    double D_get_u_south() ;
    double D_get_d_west() ;
    double D_get_d_south() ;

	/* kang - added */
	char **pads;
	int npads;
	int gotone;

    screen_y = get_map_top() ;
    screen_x = get_map_left() ;

	/* kang - get list of pads (frames) */
	R_pad_list (&pads, &npads);

/*
    fprintf(stderr, "\n\n");
    fprintf(stderr, "Buttons:\n") ;
    fprintf(stderr, "Left:   Establish new center\n") ;
    fprintf(stderr, "Middle: Check coordinates\n") ;
    fprintf(stderr, "Right:  Establish new center\n\n") ;
*/

    my = (window->north + window->south) / 2;
    mx = (window->east + window->west) / 2; 

    len_n = len_s = len_e = len_w = 0;
/*
    do
    {
*/
	R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
	button &= 0xf;

	/* kang - call the subroutine to get the frame and re-initialize it */
	gotone = identb1_win(screen_x, screen_y, npads, pads);
	D_setup(0);

	ux = D_d_to_u_col((double)screen_x)  ;
	uy = D_d_to_u_row((double)screen_y)  ;

	screen_y = (int) D_u_to_d_row (uy);
	screen_x = (int) D_u_to_d_col (ux);

/*
	switch(button)
	{
	case 1:
		break ;
	case 2:
		return(0);
		break;
	case 3: break;
	}
*/

	dx = ux - mx;
	dy = uy - my;
	north = window->north + dy;
	south = window->south + dy;
	west  = window->west + dx;
	east  = window->east + dx;

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

/* kang - comment out for no printing */
/*
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
*/

/*
    } while (button == 2) ;
*/

/*
    fprintf (stderr, "\n\n");
*/

    window->north = north;
    window->south = south;
    window->east  = east ;
    window->west  = west ;
    return(1);
}

static
max(a,b)
{
    return a>b?a:b;
}

identb1_win(x, y, npads, pads)
    int x, y;
   int npads;
   char **pads;
{
   char **list;
   int count;
   int closest ;
   int p;
   int stat ;
   int t, b, l, r ;
   int button ;
   int gotone ;
   char cur_pad[256] ;

      closest = 9999999 ;
      gotone = 0 ;

      for (p = 0; p < npads; p++)
      {
         if (! strlen(pads[p]))
            continue ;

         stat = R_pad_select(pads[p]) ;
         if (stat)
         {
            R_pad_perror ("ERROR", stat);
            continue;
         }

   /* Check each window's "d_win" */
         stat = R_pad_get_item ("d_win", &list, &count);
         if (stat)
         {
            R_pad_perror ("ERROR", stat);
            continue;
         }
         sscanf(list[0],"%d %d %d %d", &t, &b, &l, &r) ;
         R_pad_freelist(list, count) ;

   /* If chosen point is outside pad window, continue */
         if ( x < l || x > r
           || y < t || y > b)
            continue ;

   /* If right edge closer than closest, the save pad name */
         if ( (r - x) >= 0 && (r - x) < closest)
         {
            closest = r - x ;
            gotone = 1 ;
            strcpy(cur_pad, pads[p]) ;

            /* kang - make sure that when multiple frames exist, clicking
                    the full_screen won't activate */
            if (npads > 1 && strcmp(cur_pad, "full_screen") == 0) {
                gotone = 0 ;
            }
         }
      }

      if (gotone) {
         D_set_cur_wind(cur_pad) ;
        D_timestamp() ;
        }

   return(gotone) ;
}

