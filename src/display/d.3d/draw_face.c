#include <gis.h>
#include "options.h"

draw_north_face(min, max)
	CELL min, max ;
{
	int scr_x, scr_y ;
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
	if (min < 0 && max > 0)
	{
		point_calc(window.west+window.ew_res, window.north-window.ns_res,
			0., &scr_x, &scr_y) ;
		D_move_abs(scr_x, scr_y) ;
		point_calc(window.east-window.ew_res, window.north-window.ns_res,
			0., &scr_x, &scr_y) ;
		D_cont_abs(scr_x, scr_y) ;
	}
}

draw_south_face(min, max)
	CELL min, max ;
{
	int scr_x, scr_y ;
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
	if (min < 0 && max > 0)
	{
		point_calc(window.west+window.ew_res, window.south-window.ns_res,
			0., &scr_x, &scr_y) ;
		D_move_abs(scr_x, scr_y) ;
		point_calc(window.east-window.ew_res, window.south-window.ns_res,
			0., &scr_x, &scr_y) ;
		D_cont_abs(scr_x, scr_y) ;
	}
}

draw_east_face(min, max)
	CELL min, max ;
{
	int scr_x, scr_y ;
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
	if (min < 0 && max > 0)
	{
		point_calc(window.east-window.ew_res, window.south+window.ns_res,
			0., &scr_x, &scr_y) ;
		D_move_abs(scr_x, scr_y) ;
		point_calc(window.east-window.ew_res, window.north-window.ns_res,
			0., &scr_x, &scr_y) ;
		D_cont_abs(scr_x, scr_y) ;
	}
}

draw_west_face(min, max)
	CELL min, max ;
{
	int scr_x, scr_y ;
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
	if (min < 0 && max > 0)
	{
		point_calc(window.west-window.ew_res, window.south+window.ns_res,
			0., &scr_x, &scr_y) ;
		D_move_abs(scr_x, scr_y) ;
		point_calc(window.west-window.ew_res, window.north-window.ns_res,
			0., &scr_x, &scr_y) ;
		D_cont_abs(scr_x, scr_y) ;
	}
}
