/* draw_scale places a scale in the upper left hand corner of a map image */
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "display.h"
#include "D.h"
#include "raster.h"
#include "options.h"
#define NUMSCALES	12

/* declare variables */
static struct 
	{
	char *name ;
	double size ;
	double limit ;
	int seg ;
	} scales[] =
		{ {""          ,      0. ,     20., 10},
		  {"10 meters" ,     10. ,     70., 10},
		  {"50 meters" ,     50. ,    200., 5},
		  {"100 meters",    100. ,    700., 10},
		  {"500 meters",    500. ,   2000., 5},
		  {"1 km"      ,   1000. ,   7000., 10},
		  {"5 km"      ,   5000. ,  20000., 5},
		  {"10 km"     ,  10000. ,  70000., 10},
		  {"50 km"     ,  50000. , 200000., 5},
		  {"100 km"    , 100000. , 700000., 10},
		  {"1000 km"   ,1000000. ,7000000., 10},
		  {"10000 km"  ,10000000.,70000000., 10}
		} ;

int draw_scale (int use_mouse)
{
	double meters ;
	double line_len ;
	int incr ;
	int x_pos, y_pos, button;
	int t, b, l, r ;
	int pt, pb, pl, pr;
	int i;
	int size ;
	char *tmp_fname ;
	double seg_len;
	int xarr[5], yarr[5];

    tmp_fname = G_tempfile();

/* Establish text size */
	D_get_screen_window(&t, &b, &l, &r) ;
	R_set_window(t, b, l, r) ;
	size = (int)(.025 * (float)(b - t)) ;
	R_text_size(size, size) ;

	meters  = D_get_u_east() - D_get_u_west() ;
	meters *= G_database_units_to_meters_factor() ;

/* get the dot coordinates for the position */
	if (coord_inp==0) {
	  y_pos  = (int)D_get_d_north() ;
	  x_pos = (int)D_get_d_west() ;
	}
	else {
	  y_pos = (int)(east*(r - l)/100.) ;
	  x_pos = (int)(north*(b-t)/100.) ;;
	}
	/* if the user wants to use the mouse let him/her */
	if (use_mouse) {
	  fprintf (stdout,"Left:  choose location\n");
	  fprintf (stdout,"Middle: cancel\n");
	  fflush(stdout);
	  R_get_location_with_pointer(&x_pos, &y_pos, &button);
	  if (button == 2) return 0;
	}

	do{

	/* find the right scale */
		for (incr=0; incr<NUMSCALES; incr++)
		{
			if (meters <= scales[incr].limit)
				break ;
		}

		if (! incr)
			return(-1) ;

		line_len = D_get_a_to_d_xconv() * (scales[incr].size / D_get_ew_resolution())
			/ G_database_units_to_meters_factor();
		seg_len = line_len / scales[incr].seg;

	/* Blank out area with background color */
		R_standard_color(color1) ;
		r = x_pos + 40 + (int) line_len + size * strlen(scales[incr].name) ;
		pt = y_pos + 5; if (pt < t) t = t;
		pb = y_pos + 35;if (pb > b) pb = b;
		pl = x_pos + 5; if (pl < l) pl = l;
		pr = r;         if (pr > r) pr = r;
		
		if(use_mouse)
		/* bugfix 1/2002 MN: pr+1, otherwise saved area too small */
			R_panel_save(tmp_fname,pt,pb,pl,pr+1);
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
		R_move_abs (x_pos + 30, y_pos + 22) ;
		R_cont_rel ((int) line_len, 0) ;
		R_cont_rel (0, -4) ;
		R_cont_rel ((int) line_len * -1, 0) ;
		R_cont_rel (0, 4) ;
		xarr[0] = 0; yarr[0] = 0;
		xarr[1] = (int) seg_len; yarr[1] = 0;
		xarr[2] = 0; yarr[2] = -4;
		xarr[3] = (int) (seg_len * -1); yarr[3] = 0;
		xarr[4] = 0; yarr[4] = 4;
		for (i = 1; i <= scales[incr].seg; i+=2)
		{
			R_polygon_rel(xarr, yarr ,4);
			/* bugfix 1/2002 MN: added +1 to have bar completely filled */
			R_move_rel((int) (seg_len * 2 + 1), 0);
		}
		R_move_abs (x_pos + 40 + (int) line_len, y_pos + 25) ;
		R_text(scales[incr].name) ;
		R_stabilize();

		if(use_mouse)
			{
			  fprintf (stdout,"\nLeft: choose location\n");
			  fprintf (stdout,"Middle: cancel\n");
			  fprintf (stdout,"Right: confirm location\n");
			fflush(stdout);                        
	  		R_get_location_with_pointer(&x_pos, &y_pos, &button);
			if (button == 1 || button == 2)
			{
				R_panel_restore(tmp_fname);
				if (button == 2)
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
