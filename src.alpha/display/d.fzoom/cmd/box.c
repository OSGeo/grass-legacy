#include "gis.h"

make_window_box (window)
    struct Cell_head *window ;
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
    double D_d_to_u_col()  ;
    double D_d_to_u_row()  ;
    double D_u_to_d_col()  ;
    double D_u_to_d_row()  ;
    double D_get_u_west() ;
    double D_get_u_south() ;
    double D_get_d_west() ;
    double D_get_d_south() ;

	/* kang - added */
    int count;
    char **pads;
    int npads;
	int gotone;

    screen_y = get_map_top() ;
    screen_x = get_map_left() ;


/*
    fprintf(stderr, "\n\n");
    fprintf(stderr, "Muis;:\n") ;
    fprintf(stderr, "Left:  Establish corner\n") ;
    fprintf(stderr, "Center: Reject region\n") ;
    fprintf(stderr, "Right: Accept region\n\n") ;
*/

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

    count = 1;

	/* kang - Get list of pads (windows) */
	R_pad_list (&pads, &npads);

/*
   screen_x = (R_screen_rite() + R_screen_left()) / 2 ;
   screen_y = (R_screen_top() + R_screen_bot()) / 2 ;
*/

    len_n = len_s = len_e = len_w = 0;
    do
    {
		R_get_location_with_box(cur_screen_x, cur_screen_y, &screen_x, &screen_y, &button) ;
		button &= 0xf;

		if (count == 1) {
			/* kang - call the subroutine to get the frame name */
			gotone = identb1_win(screen_x, screen_y, npads, pads);

			if (gotone == 0)
				continue;

			/* kang - re-initialize the selected frame info */
			D_setup(0);

			ux1 = D_d_to_u_col((double)screen_x)  ;
			uy1 = D_d_to_u_row((double)screen_y)  ;

    		cur_screen_y = (int) D_u_to_d_row (uy1);
    		cur_screen_x = (int) D_u_to_d_col (ux1);
		}

	ux2 = D_d_to_u_col((double)screen_x)  ;
	uy2 = D_d_to_u_row((double)screen_y)  ;

	G_limit_south (&uy2, window->proj);
	G_limit_north (&uy2, window->proj);
	G_limit_east  (&ux2, window->proj);
	G_limit_west  (&ux2, window->proj);

	screen_y = (int) D_u_to_d_row (uy2);
	screen_x = (int) D_u_to_d_col (ux2);

	/* kang */
	if (count == 1) {
		cur_screen_x = screen_x ;
		cur_screen_y = screen_y ;
		ux1 = ux2 ;
		uy1 = uy2 ;
	} else {
	}
	count = count + 1;

/* kang - comment out - change to the above code */
/*
	switch(button)
	{
	case 1:
		cur_screen_x = screen_x ;
		cur_screen_y = screen_y ;
		ux1 = ux2 ;
		uy1 = uy2 ;
		break ;
	case 2:
		return 0;
		break;
	case 3: break;
	}
*/


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

/* kang - comment out for not printing any info */
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

    } while (count <= 2) ;
/* kang - always use button 1 */
/*
    } while (button != 3) ;
*/

/*
    fprintf (stderr, "\n\n");
*/

    window->north = north;
    window->south = south;
    window->east  = east ;
    window->west  = west ;
    return 1;
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

