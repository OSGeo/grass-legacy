/*  @(#)wind_conv.c	2.1  6/26/87  */
#include "graphics.h"
#include "wind.h"
#include "gis.h"
#include "digit.h"

window_conversions(N, S, E, W)
	double N, S, E, W ;
{
	double D_vert, D_hori , Dadj;
	struct Cell_head window;

	U_west  = W ;
	U_east  = E ;
	U_north = N ;
	U_south = S ;

	reset_draw_win();

/* Calculate Dot limits from Array limits */
	D_vert = (double)screen_bot - (double)screen_top ;
	D_hori = (double)screen_right - (double)screen_left ;

	D_north = (double)screen_top ;
	D_west  = (double)screen_left ;

	U_to_D_xconv = D_hori / (U_east - U_west) ;
	U_to_D_yconv = D_vert / (U_north - U_south) ;
	if (U_to_D_xconv > U_to_D_yconv)
		U_to_D_xconv = U_to_D_yconv ;
	else
		U_to_D_yconv = U_to_D_xconv ;
	D_hori = U_to_D_xconv * (U_east  - U_west ) ;
	D_vert = U_to_D_yconv * (U_north - U_south) ;

	D_south = D_north + D_vert ;
	D_east  = D_west  + D_hori ;


	U_to_D_xconv = (D_east  - D_west ) / (U_east  - U_west ) ;
	U_to_D_yconv = (D_north - D_south) / (U_north - U_south) ;

    {
	double fabs ();
	double ux1, ux2, uy1, uy2;

	screen_to_utm(0, 0, &ux1, &uy1);
	screen_to_utm(1, 0, &ux2, &uy2);
	Pix_size = fabs (ux2 - ux1);
    }

}

utm_to_screen(Ux, Uy, Sx, Sy)
	double Ux, Uy ;
	int *Sx, *Sy ;
{
	*Sx = (int)(((Ux - U_west) * U_to_D_xconv) + D_west) ;
	*Sy = (int)(((Uy - U_south) * U_to_D_yconv) + D_south) ;
}

screen_to_utm(Sx, Sy, Ux, Uy)
	int Sx, Sy ;
	double *Ux, *Uy ;
{
	*Ux = ((double)Sx - D_west) / U_to_D_xconv + U_west ;
	*Uy = ((double)Sy - D_south)/ U_to_D_yconv + U_south ;
}

outline_window()
{
	standard_color( translate_color("white")) ;
	move_abs((int)D_west, (int)D_north) ;
	cont_abs((int)D_east, (int)D_north) ;
	cont_abs((int)D_east, (int)D_south) ;
	cont_abs((int)D_west, (int)D_south) ;
	cont_abs((int)D_west, (int)D_north) ;
}

get_D_west()
{
	return((int)D_west) ;
}

get_D_east()
{
	return((int)D_east) ;
}

get_D_north()
{
	return((int)D_north) ;
}

get_D_south()
{
	return((int)D_south) ;
}
