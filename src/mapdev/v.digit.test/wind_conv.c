/*  @(#)wind_conv.c	2.1  6/26/87  */
#include "graphics.h"
#include "display.h"
#include "raster.h"
#include "wind.h"
#include "gis.h"
#include "local_proto.h"

int 
window_conversions (double N, double S, double E, double W)
{
	double D_vert, D_hori , Dadj;
	struct Cell_head window;

	U_west  = W ;
	U_east  = E ;
	U_north = N ;
	U_south = S ;

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

	

/* code copied from   Displaylib */
/* Pull all edges in so picture stays centered */
#ifdef FOO
        Dadj = ((screen_bot - screen_top ) - D_vert) / 2 ;
        if (Dadj > 0.0)
        {
            D_north = screen_top + Dadj ;
            D_south = D_north + D_vert ;
        }
        else
        {
                D_south = screen_bot ;
        }
        Dadj = ((screen_right - screen_left ) - D_hori) / 2 ;
        if (Dadj > 0.0)
        {
            D_west = screen_left + Dadj ;
            D_east = D_west + D_hori ;
        }
        else
        {
                D_east = screen_right ;
        }
#endif
/* --- */


	U_to_D_xconv = (D_east  - D_west ) / (U_east  - U_west ) ;
	U_to_D_yconv = (D_north - D_south) / (U_north - U_south) ;


    return 0;
}

int 
utm_to_screen (double Ux, double Uy, int *Sx, int *Sy)
{
	*Sx = (int)(((Ux - U_west) * U_to_D_xconv) + D_west) ;
	*Sy = (int)(((Uy - U_south) * U_to_D_yconv) + D_south) ;
    return 0;
}

int 
screen_to_utm (int Sx, int Sy, double *Ux, double *Uy)
{
	*Ux = ((double)Sx - D_west) / U_to_D_xconv + U_west ;
	*Uy = ((double)Sy - D_south)/ U_to_D_yconv + U_south ;
    return 0;
}

int 
outline_window (void)
{
	R_standard_color( D_translate_color("white")) ;
	R_move_abs((int)D_west, (int)D_north) ;
	R_cont_abs((int)D_east, (int)D_north) ;
	R_cont_abs((int)D_east, (int)D_south) ;
	R_cont_abs((int)D_west, (int)D_south) ;
	R_cont_abs((int)D_west, (int)D_north) ;
	V_flush() ;
    return 0;
}

int 
get_D_west (void)
{
	return((int)D_west) ;
}

int 
get_D_east (void)
{
	return((int)D_east) ;
}

int 
get_D_north (void)
{
	return((int)D_north) ;
}

int 
get_D_south (void)
{
	return((int)D_south) ;
}
