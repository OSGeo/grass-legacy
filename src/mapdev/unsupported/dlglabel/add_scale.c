/*  @(#)add_scale.c	2.1  6/26/87  */
/* add_scale places a scale in the upper left hand corner of a map image */

#include "convert.h"

#define NUMSCALES	10


/* declare variables */
static struct 
{
	char *name ;
	float size ;
	float G_limit ;
} scales[] =
		{ ""          ,      0.,     20.,
		  "10 meters" ,     10.,     70.,
		  "50 meters" ,     50.,    200.,
		  "100 meters",    100.,    700.,
		  "500 meters",    500.,   2000.,
		  "1 km"      ,   1000.,   7000.,
		  "5 km"      ,   5000.,  20000.,
		  "10 km"     ,  10000.,  70000.,
		  "50 km"     ,  50000., 200000.,
		  "100 km"    , 100000., 700000.
		} ;

add_scale ()
{
	double meters ;
	int line_len ;
	int incr ;
	int WINDO_TOP, WINDO_LEFT ;

	meters  = U_east - U_west ;
	WINDO_TOP  = get_window_top() ;
	WINDO_LEFT = get_window_left() ;

/* find the right scale */
	for (incr=0; incr<NUMSCALES; incr++)
	{
		if (meters <= scales[incr].G_limit)
			break ;
	}

	if (incr)
	{
		line_len = scales[incr].size * U_to_D_xconv ;

		R_standard_color(D_translate_color("white") ) ;
		R_move_abs (WINDO_LEFT + 10, WINDO_TOP + 25) ;
		R_cont_rel ( 0, -10) ;
		R_cont_rel (10, 10) ;
		R_cont_rel ( 0, -10) ;
		R_move_rel (-5, 14) ;
		R_cont_rel ( 0, -17) ;
		R_cont_rel (-2,  0) ;
		R_cont_rel ( 2, -2) ;
		R_cont_rel ( 2, 2) ;
		R_cont_rel (-2,  0) ;
		R_move_abs (WINDO_LEFT + 30, WINDO_TOP + 10) ;
		R_cont_abs (WINDO_LEFT + 30, WINDO_TOP + 30) ;
		R_move_abs (WINDO_LEFT + 30, WINDO_TOP + 20) ;
		R_cont_abs (WINDO_LEFT + 30 + line_len, WINDO_TOP + 20) ;
		R_move_abs (WINDO_LEFT + 30 + line_len, WINDO_TOP + 10) ;
		R_cont_abs (WINDO_LEFT + 30 + line_len, WINDO_TOP + 30) ;

		R_move_abs (WINDO_LEFT + 40 + line_len, WINDO_TOP + 20) ;
		R_text(scales[incr].name) ;

	}
	return(0) ;
}
