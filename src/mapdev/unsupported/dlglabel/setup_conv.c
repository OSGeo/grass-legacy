/*  @(#)setup_conv.c	2.1  6/26/87  */
#include <stdio.h>
#include "dlghead.h"
#include "gis.h"

static	int		D_x_beg,	D_y_beg,	D_x_end,	D_y_end ;
struct	Cell_head	window ;

setup_conversions()
{

	double	U_west, U_east, U_north, U_south ;
	double	D_get_d_north(), D_get_d_south(), D_get_d_east(), D_get_d_west(), ;

/* Key all coordinate G_limits off dlg header coordinates */
	U_west  = dlg_coors.utm_e[0] ;
	U_east  = dlg_coors.utm_e[0] ;
	U_south = dlg_coors.utm_n[0] ;
	U_north = dlg_coors.utm_n[0] ;
	for(i=1; i<4; i++)
	{
		if (U_west  > dlg_coors.utm_e[i]) U_west  = dlg_coors.utm_e[i] ;
		if (U_east  < dlg_coors.utm_e[i]) U_east  = dlg_coors.utm_e[i] ;
		if (U_south > dlg_coors.utm_n[i]) U_south = dlg_coors.utm_n[i] ;
		if (U_north < dlg_coors.utm_n[i]) U_north = dlg_coors.utm_n[i] ;
	}


	window.format = '1' ;
	window.rows = 0 ;	 window.col = 0 ;
	window.proj = dlg_head.plani_code ;			/*  UTM's  */
	window.zone = dlg_head.plani_zone ;

	/*  don't know what to put for this  */
	window.ew_res = 50.0 ;
	window.ns_res = 50.0 ;

	window.north	= U_north ;
	window.south	= U_south ;
	window.east	= U_east ;
	window.west	= U_west ;

/*  now setup the conversion coefficients for graphics  */
	D_D_do_conversions(&window) ;

/*  setup screen for drawing map outline  */
	D_x_beg = (int)D_get_d_west ;
	D_x_end = (int)D_get_d_east ;
	D_y_beg = (int)D_get_d_north ;
	D_y_end = (int)D_get_d_south ;

}
	/*  setup_conversions()  */


outline_window()
{
	R_set_window ( D_get_d_north, D_get_d_south, D_get_d_east, D_get_d_west) ;
}

