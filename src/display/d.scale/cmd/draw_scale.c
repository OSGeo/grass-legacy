
/* draw_scale places a scale in the upper left hand corner of a map image */
#include <stdio.h>
#include <math.h>
#include "options.h"
#include "gis.h"
#define NUMSCALES	12


/* declare variables */
static struct 
	{
	char *name ;
	double size ;
	double limit ;
	} scales[] =
		{ ""          ,      0. ,     20.,
		  "10 meters" ,     10. ,     70.,
		  "50 meters" ,     50. ,    200.,
		  "100 meters",    100. ,    700.,
		  "500 meters",    500. ,   2000.,
		  "1 km"      ,   1000. ,   7000.,
		  "5 km"      ,   5000. ,  20000.,
		  "10 km"     ,  10000. ,  70000.,
		  "50 km"     ,  50000. , 200000.,
		  "100 km"    , 100000. , 700000.,
		  "1000 km"   ,1000000. ,7000000.,
		  "10000 km"  ,10000000.,70000000.
		} ;

draw_scale ( use_mouse)
{
	double meters;
	int line_len ;
	int incr ;
	double D_get_a_to_d_xconv() ;
	double D_get_u_east(), D_get_u_west() ;
	double D_get_u_north(), D_get_u_south() ;
	double D_get_d_west(), D_get_d_north() ;
	double D_get_d_east(), D_get_d_south() ;
	double D_u_to_d_row(), D_u_to_d_col();
	int x_pos, y_pos, button;
	double D_get_ew_resolution() ;
	int t, b, l, r ;
	int i ;
	int size ;
	char *tmp_fname ;
	char yn[128] ;

    tmp_fname = G_tempfile();

/* Establish text size */
	D_get_screen_window(&t, &b, &l, &r) ;
	R_set_window(t, b, l, r) ;
	size = (int)(.025 * (float)(b - t)) ;
	R_text_size(size, size) ;
        G_begin_distance_calculations();

        /* compute the distance from east to west in the middle of the window */
	meters = G_distance(D_get_u_east(), (D_get_u_north() + D_get_u_south())/2., D_get_u_west(), (D_get_u_north() + D_get_u_south())/2.);

/* get the dot coordinates for the position */
	if (coord_inp==0) {
	  y_pos  = (int)D_get_d_north() ;
	  x_pos = (int)D_get_d_west() ;
	}
	else {
	  y_pos = (int)(east*(r - l)/100.) ;
	  x_pos = (int)(north*(b-t)/100.) ;;
	}

	do{
	/* if the user wants to use the mouse let him/her */
		if (use_mouse) {
		  printf("\nUse mouse to select the scale location:");
		  printf("\nAny Button: select point\n");
		  R_get_location_with_pointer(&x_pos, &y_pos, &button);
		}

	/* find the right scale */
		for (incr=0; incr<NUMSCALES; incr++)
		{
			if (meters <= scales[incr].limit)
				break ;
		}

		if (! incr)
			return(-1) ;

		line_len = (int)(D_get_a_to_d_xconv() * (scales[incr].size / 
			      G_distance(D_get_u_west(), (D_get_u_north() + D_get_u_south())/2., D_get_u_west() + D_get_ew_resolution(), (D_get_u_north() + D_get_u_south())/2.))) ;

	/* Blank out area with background color */
		R_standard_color(color1) ;
		r = x_pos + 40 + line_len + size * strlen(scales[incr].name) ;
		if(use_mouse)
			R_panel_save(tmp_fname,y_pos+5,y_pos+35,x_pos+5,r);
		for(i=y_pos + 5; i < y_pos + 35; i++)
			R_move_abs(x_pos+5, i), R_cont_abs(r, i) ;
			
	/* Draw legend */
		R_standard_color(color2) ;
		R_move_abs (x_pos + 10, y_pos + 25) ;
		R_cont_rel ( 0,-10) ;
		R_cont_rel (10, 10) ;
		R_cont_rel ( 0,-10) ;
		R_move_rel (-5, 14) ;
		R_cont_rel ( 0,-17) ;
		R_cont_rel (-2, -0) ;
		R_cont_rel ( 2, -2) ;
		R_cont_rel ( 2,  2) ;
		R_cont_rel (-2, -0) ;
		R_move_abs (x_pos + 30, y_pos + 10) ;
		R_cont_abs (x_pos + 30, y_pos + 30) ;
		R_move_abs (x_pos + 30, y_pos + 20) ;
		R_cont_abs (x_pos + 30 + line_len, y_pos + 20) ;
		R_move_abs (x_pos + 30 + line_len, y_pos + 10) ;
		R_cont_abs (x_pos + 30 + line_len, y_pos + 30) ;

		R_move_abs (x_pos + 40 + line_len, y_pos + 25) ;
		R_text(scales[incr].name) ;
		R_flush();

		if(use_mouse)
		{
			printf("Look OK [y] ? ");
			gets(yn);
			if (!strcmp("n",yn) || !strcmp("N",yn) ||
				!strcmp("no",yn) || !strcmp("NO",yn))
			{
				R_panel_restore(tmp_fname);
				printf("Try again? [y] ? ");
				gets(yn);
				if (!strcmp("n",yn) || !strcmp("N",yn) ||
					!strcmp("no",yn) || !strcmp("NO",yn))
				{
					return(0) ;
				}
			}
			else
			{
				R_panel_delete(tmp_fname);
				use_mouse = 0;
			}
		}
	} while (use_mouse) ;

	return(0) ;
}
