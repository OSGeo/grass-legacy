/*  %W%  %G%  */

/* draw_scale places a scale in the upper left hand corner of a map image */
#include <stdio.h>
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

draw_scale (color1, color2)
{
	double meters ;
	int line_len ;
	int incr ;
	double D_get_a_to_d_xconv() ;
	double D_get_u_east(), D_get_u_west() ;
	double D_get_d_west(), D_get_d_north() ;
	int D_west, D_north ;
	double D_get_ew_resolution() ;
	int t, b, l, r ;
	int i ;
	int size ;

/* Establish text size */
	D_get_screen_window(&t, &b, &l, &r) ;
	R_set_window(t, b, l, r) ;
	size = (int)(.025 * (float)(b - t)) ;
	R_text_size(size, size) ;

	meters  = D_get_u_east() - D_get_u_west() ;
	D_north  = (int)D_get_d_north() ;
	D_west = (int)D_get_d_west() ;

/* find the right scale */
	for (incr=0; incr<NUMSCALES; incr++)
	{
		if (meters <= scales[incr].limit)
			break ;
	}

	if (! incr)
		return(-1) ;

	line_len = (int)(D_get_a_to_d_xconv() * (scales[incr].size / D_get_ew_resolution()) ) ;

/* Blank out area with background color */
	R_standard_color(color1) ;
	r = D_west + 40 + line_len + size * strlen(scales[incr].name) ;
	for(i=D_north + 5; i < D_north + 35; i++)
		R_move_abs(D_west+5, i), R_cont_abs(r, i) ;
		
/* Draw legend */
	R_standard_color(color2) ;
	R_move_abs (D_west + 10, D_north + 25) ;
	R_cont_rel ( 0,-10) ;
	R_cont_rel (10, 10) ;
	R_cont_rel ( 0,-10) ;
	R_move_rel (-5, 14) ;
	R_cont_rel ( 0,-17) ;
	R_cont_rel (-2, -0) ;
	R_cont_rel ( 2, -2) ;
	R_cont_rel ( 2,  2) ;
	R_cont_rel (-2, -0) ;
	R_move_abs (D_west + 30, D_north + 10) ;
	R_cont_abs (D_west + 30, D_north + 30) ;
	R_move_abs (D_west + 30, D_north + 20) ;
	R_cont_abs (D_west + 30 + line_len, D_north + 20) ;
	R_move_abs (D_west + 30 + line_len, D_north + 10) ;
	R_cont_abs (D_west + 30 + line_len, D_north + 30) ;

	R_move_abs (D_west + 40 + line_len, D_north + 25) ;
	R_text(scales[incr].name) ;

	return(0) ;
}
