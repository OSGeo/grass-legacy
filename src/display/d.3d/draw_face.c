#include <gis.h>
#include "display.h"
#include "D.h"
#include "options.h"
#include "l_proto.h"

int draw_north_face (DCELL min, DCELL max)
{
	int scr_x, scr_y ;
	DCELL d1 = min, d2 = max;
	if(G_is_d_null_value(&d1)) min = 0.;
	if(G_is_d_null_value(&d2)) max = 0.;
	point_calc(window.west+window.ew_res, window.north-window.ns_res,
		min * exag, &scr_x, &scr_y) ;
	D_move_abs(scr_x, scr_y) ;
	point_calc(window.east-window.ew_res, window.north-window.ns_res,
		min * exag, &scr_x, &scr_y) ;
	D_cont_abs(scr_x, scr_y) ;
	point_calc(window.east-window.ew_res, window.north-window.ns_res,
		max * exag, &scr_x, &scr_y) ;
	D_cont_abs(scr_x, scr_y) ;
	point_calc(window.west+window.ew_res, window.north-window.ns_res,
		max * exag, &scr_x, &scr_y) ;
	D_cont_abs(scr_x, scr_y) ;
	point_calc(window.west+window.ew_res, window.north-window.ns_res,
		min * exag, &scr_x, &scr_y) ;
	D_cont_abs(scr_x, scr_y) ;
	if (min < 0. && max > 0.)
	{
		point_calc(window.west+window.ew_res, window.north-window.ns_res,
			0., &scr_x, &scr_y) ;
		D_move_abs(scr_x, scr_y) ;
		point_calc(window.east-window.ew_res, window.north-window.ns_res,
			0., &scr_x, &scr_y) ;
		D_cont_abs(scr_x, scr_y) ;
	}

	return 0;
}

int 
draw_south_face (DCELL min, DCELL max)
{
	int scr_x, scr_y ;
	DCELL d1 = min, d2 = max;
	if(G_is_d_null_value(&d1)) min = 0.;
	if(G_is_d_null_value(&d2)) max = 0.;
	point_calc(window.west+window.ew_res, window.south+window.ns_res,
		min * exag, &scr_x, &scr_y) ;
	D_move_abs(scr_x, scr_y) ;
	point_calc(window.east-window.ew_res, window.south+window.ns_res,
		min * exag, &scr_x, &scr_y) ;
	D_cont_abs(scr_x, scr_y) ;
	point_calc(window.east-window.ew_res, window.south+window.ns_res,
		max * exag, &scr_x, &scr_y) ;
	D_cont_abs(scr_x, scr_y) ;
	point_calc(window.west+window.ew_res, window.south+window.ns_res,
		max * exag, &scr_x, &scr_y) ;
	D_cont_abs(scr_x, scr_y) ;
	point_calc(window.west+window.ew_res, window.south+window.ns_res,
		min * exag, &scr_x, &scr_y) ;
	D_cont_abs(scr_x, scr_y) ;
	if (min < 0. && max > 0.)
	{
		point_calc(window.west+window.ew_res, window.south-window.ns_res,
			0., &scr_x, &scr_y) ;
		D_move_abs(scr_x, scr_y) ;
		point_calc(window.east-window.ew_res, window.south-window.ns_res,
			0., &scr_x, &scr_y) ;
		D_cont_abs(scr_x, scr_y) ;
	}

	return 0;
}

int 
draw_east_face (DCELL min, DCELL max)
{
	int scr_x, scr_y ;
	DCELL d1 = min, d2 = max;
	if(G_is_d_null_value(&d1)) min = 0.;
	if(G_is_d_null_value(&d2)) max = 0.;
	point_calc(window.east-window.ew_res, window.south+window.ns_res,
		min * exag, &scr_x, &scr_y) ;
	D_move_abs(scr_x, scr_y) ;
	point_calc(window.east-window.ew_res, window.north-window.ns_res,
		min * exag, &scr_x, &scr_y) ;
	D_cont_abs(scr_x, scr_y) ;
	point_calc(window.east-window.ew_res, window.north-window.ns_res,
		max * exag, &scr_x, &scr_y) ;
	D_cont_abs(scr_x, scr_y) ;
	point_calc(window.east-window.ew_res, window.south+window.ns_res,
		max * exag, &scr_x, &scr_y) ;
	D_cont_abs(scr_x, scr_y) ;
	point_calc(window.east-window.ew_res, window.south+window.ns_res,
		min * exag, &scr_x, &scr_y) ;
	D_cont_abs(scr_x, scr_y) ;
	if (min < 0. && max > 0.)
	{
		point_calc(window.east-window.ew_res, window.south+window.ns_res,
			0., &scr_x, &scr_y) ;
		D_move_abs(scr_x, scr_y) ;
		point_calc(window.east-window.ew_res, window.north-window.ns_res,
			0., &scr_x, &scr_y) ;
		D_cont_abs(scr_x, scr_y) ;
	}

	return 0;
}

int 
draw_west_face (DCELL min, DCELL max)
{
	int scr_x, scr_y ;
	DCELL d1 = min, d2 = max;
	if(G_is_d_null_value(&d1)) min = 0.;
	if(G_is_d_null_value(&d2)) max = 0.;
	point_calc(window.west+window.ew_res, window.south+window.ns_res,
		min * exag, &scr_x, &scr_y) ;
	D_move_abs(scr_x, scr_y) ;
	point_calc(window.west+window.ew_res, window.north-window.ns_res,
		min * exag, &scr_x, &scr_y) ;
	D_cont_abs(scr_x, scr_y) ;
	point_calc(window.west+window.ew_res, window.north-window.ns_res,
		max * exag, &scr_x, &scr_y) ;
	D_cont_abs(scr_x, scr_y) ;
	point_calc(window.west+window.ew_res, window.south+window.ns_res,
		max * exag, &scr_x, &scr_y) ;
	D_cont_abs(scr_x, scr_y) ;
	point_calc(window.west+window.ew_res, window.south+window.ns_res,
		min * exag, &scr_x, &scr_y) ;
	D_cont_abs(scr_x, scr_y) ;
	if (min < 0. && max > 0.)
	{
		point_calc(window.west-window.ew_res, window.south+window.ns_res,
			0., &scr_x, &scr_y) ;
		D_move_abs(scr_x, scr_y) ;
		point_calc(window.west-window.ew_res, window.north-window.ns_res,
			0., &scr_x, &scr_y) ;
		D_cont_abs(scr_x, scr_y) ;
	}

	return 0;
}
