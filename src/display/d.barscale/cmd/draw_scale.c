/* draw_scale places a scale in the upper left hand corner of a map image */
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "options.h"

#define NUMSCALES	14

/* declare variables */
static const struct scale {
	char *name ;
	double size ;
	double limit ;
	int seg ;
} all_scales[2][NUMSCALES] = {
	{
		/* meters */
		{""          ,       0.,      20., 10},
		{"10 meters" ,      10.,      70., 10},
		{"50 meters" ,      50.,     200.,  5},
		{"100 meters",     100.,     700., 10},
		{"500 meters",     500.,    2000.,  5},
		{"1 km"      ,    1000.,    7000., 10},
		{"5 km"      ,    5000.,   20000.,  5},
		{"10 km"     ,   10000.,   70000., 10},
		{"50 km"     ,   50000.,  200000.,  5},
		{"100 km"    ,  100000.,  700000., 10},
		{"500 km"    ,  500000., 2000000.,  5},
		{"1000 km"   , 1000000., 7000000., 10},
		{"5000 km"   , 5000000.,20000000.,  5},
		{"10000 km"  ,10000000.,70000000., 10}
	},
	{
		/* feet/miles */
		{""          ,      0.000,      10., 10},
		{"10 feet"   ,      3.048,      20., 10},
		{"50 feet"   ,     15.240,     100.,  5},
		{"100 feet"  ,     30.480,     200., 10},
		{"500 feet"  ,    152.400,    1000.,  5},
		{"1000 feet" ,    304.800,    2000., 10},
		{"1 mile"    ,   1609.344,   10000.,  5},
		{"5 miles"   ,   8046.720,   20000.,  5},
		{"10 miles"  ,  16093.440,  100000., 10},
		{"50 miles"  ,  80467.200,  200000.,  5},
		{"100 miles" , 160934.400, 1000000., 10},
		{"500 miles" , 804672.000, 2000000.,  5},
		{"1000 miles",1609344.000,10000000., 10},
		{"5000 miles",8046720.000,20000000.,  5},
	}
};

int draw_scale(char *save, int toptext)
{
	double meters ;
	double line_len ;
	int incr ;
	int x_pos, y_pos;
	int t, b, l, r ;
	int pt, pb, pl, pr;
	int i;
	int size ;
	double seg_len;
	int xarr[5], yarr[5];
	const struct scale *scales = all_scales[use_feet];

	/* Establish text size */
	D_get_screen_window(&t, &b, &l, &r) ;
	R_set_window(t, b, l, r) ;
	size = 14 ;
	R_text_size(size, size) ;

	meters  = D_get_u_east() - D_get_u_west() ;
	meters *= G_database_units_to_meters_factor() ;

	x_pos = (int)(east *(r-l)/100.);
	y_pos = (int)(north*(b-t)/100.);

	/* find the right scale */
	for (incr=0; incr<NUMSCALES; incr++)
	{
		if (meters <= scales[incr].limit)
			break ;
	}

	if (!incr)
		return(-1) ;

	line_len = D_get_u_to_d_xconv() * scales[incr].size
		/ G_database_units_to_meters_factor();
	seg_len = line_len / scales[incr].seg;

	/* Blank out area with background color */
	if(toptext)
	{
		pr = x_pos + 35 + (int) line_len ;
		pt = y_pos - 15; if (pt < t) pt = t;
	}
	else
	{
		pr = x_pos + 35 + (int) line_len + size * strlen(scales[incr].name) ;
		pt = y_pos + 0; if (pt < t) pt = t;
	}
	pb = y_pos + 30;if (pb > b) pb = b;
	pl = x_pos + 0; if (pl < l) pl = l;
	pr = pr;        if (pr > r) pr = r;

	if(save)
		R_panel_save(save,pt,pb,pl,pr);

	if(do_background) {
		R_standard_color(color1) ;
		R_box_abs(pl, pt, pr, pb);
	}
	
	/* Draw legend */
	R_standard_color(color2) ;
	R_move_abs (x_pos + 5, y_pos + 20) ;
	R_cont_rel ( 0,-10) ;
	R_cont_rel (10, 10) ;
	R_cont_rel ( 0,-10) ;
	R_move_rel (-5, 14) ;
	R_cont_rel ( 0,-17) ;
	R_cont_rel (-2, -0) ;
	R_cont_rel ( 2, -2) ;
	R_cont_rel ( 2,  2) ;
	R_cont_rel (-2, -0) ;

	if(do_bar) {
		R_move_abs (x_pos + 25, y_pos + 17) ;
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
			R_move_rel((int) (seg_len * 2 + 1), 0);
		}
	}
	else {  /* draw simple line scale */
		R_move_abs (x_pos + 25, y_pos + 5) ;
		R_cont_abs (x_pos + 25, y_pos + 25) ;
		R_move_abs (x_pos + 25, y_pos + 15) ;
		R_cont_abs (x_pos + 25 + line_len, y_pos + 15) ;
		R_move_abs (x_pos + 25 + line_len, y_pos + 5) ;
		R_cont_abs (x_pos + 25 + line_len, y_pos + 25) ;
	}

	if(toptext)
	{
		R_move_abs(x_pos+25 + (int)(line_len/2. - strlen(scales[incr].name)*size*0.81/2), y_pos);
		R_text(scales[incr].name) ;
	}
	else
	{
		R_move_abs (x_pos + 35 + (int) line_len, y_pos + 20) ;
		R_text(scales[incr].name) ;
	}	
	
	R_stabilize();

	return(0) ;
}
