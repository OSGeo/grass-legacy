/*  @(#)conversions.c	1.1  5/4/87  */
/*  file contains - init_conversions(), do_conversions(),
*	get_window_top(),	get_window_bot(),	get_window_left(),
*	get_window_rite(),	current_thresh(), 	screen_to_utm()  ;
*/

#include <stdio.h>
#include "convert.h"

#define	TEXT_WIDTH	6
#define	TEXT_HEIGHT 7
	
static	double		WINDO_BOT,	WINDO_TOP,	WINDO_LEFT,	WINDO_RITE ;

init_conversions()
{
	/*  only called once  */

	int		top,	bot,	left,	rite ;
	D_get_screen_window ( &top, &bot, &left, &rite) ;

	WINDO_TOP  = (double)top ;
	WINDO_BOT  = (double)bot ;
	WINDO_LEFT = (double)left ;
	WINDO_RITE = (double)rite ;

	/*  set once for any text calls here after  */
	R_text_size( TEXT_WIDTH, TEXT_HEIGHT) ;

}

do_conversions()
{
	double D_vert, D_hori ;
	int i ;

/* Calculate Dot G_limits from Array G_limits */
	D_vert = WINDO_BOT - WINDO_TOP ;
	D_hori = WINDO_RITE - WINDO_LEFT ;

	D_north = WINDO_TOP ;
	D_west  = WINDO_LEFT ;

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

#ifdef DEBUG
	fprintf(stderr,
		" D_w %10.1f  D_e %10.1f  D_s %10.1f  D_n %10.1f\n",
		D_west, D_east, D_south, D_north) ;
	fprintf(stderr,
		" U_w %10.1f  U_e %10.1f  U_s %10.1f  U_n %10.1f\n",
		U_west, U_east, U_south, U_north) ;
	fprintf(stderr,
		" BOT %10.1f  TOP %10.1f  LFT %10.1f  RHT %10.1f\n",
		WINDO_BOT, WINDO_TOP, WINDO_LEFT, WINDO_RITE) ;
	getchar() ;
#endif DEBUG
}

get_window_top()
{
	return( (int)WINDO_TOP) ;
}


get_window_bot()
{
	return( (int)WINDO_BOT) ;
}


get_window_left()
{
	return( (int)WINDO_LEFT) ;
}


get_window_rite()
{
	return( (int)WINDO_RITE) ;
}



double
current_thresh()
{
	double	thresh ;
	thresh =  (double)5.0 / U_to_D_xconv  ;
	return (thresh) ;
}


screen_to_utm (screen_x, screen_y, ux, uy)
	int 	screen_x, screen_y ;
	double	*ux,	*uy ;
{
		int	sx,	sy ;

		sx =  screen_x ;
		sy =  screen_y ;

		*ux = ((double)sx - D_west) / U_to_D_xconv + U_west ;
		*uy = ((double)sy - D_south)/ U_to_D_yconv + U_south ;
}

