#include "graphics.h"
#include <stdio.h>
#define GETEM	if (notgotem) getem()
static int notgotem = 1 ;
static int wind_top ;
static int wind_bot ;
static int wind_rite ;
static int wind_left ;

get_wind_bot() 
{
	GETEM ;
	return(wind_bot) ;
}

get_wind_top() 
{
	GETEM ;
	return(wind_top) ;
}

get_wind_rite() 
{
	GETEM ;
	return(wind_rite) ;
}

get_wind_left() 
{
	GETEM ;
	return(wind_left) ;
}

get_map_bot() 
{
	GETEM ;
	return(wind_bot) ;
}

get_map_top() 
{
	register float tmp1, tmp2, tmp3 ;
	GETEM ;
	if (notgotem) { getem() ; notgotem = 0; }
	tmp1 = (float)wind_bot ;
	tmp2 = (float)wind_top ;
	tmp3 = tmp1 + WINDOW_PROP_SCREEN_Y * (tmp2 - tmp1) ;
	return ((int)tmp3) ;
}

get_map_left()
{
	register float tmp1, tmp2, tmp3 ;
	GETEM ;
	tmp1 = (float)wind_left ;
	tmp2 = (float)wind_rite ;
	tmp3 = tmp2 - WINDOW_PROP_SCREEN_X * (tmp2 - tmp1) ;
	return ((int)tmp3) ;
}

get_map_rite() 
{
	GETEM ;
	return(wind_rite) ;
}

get_wind_y_pos(position)
	float position ;
{
	register float tmp1, tmp2, tmp3 ;
	GETEM ;
	tmp1 = (float)wind_top ;
	tmp2 = (float)wind_bot ;
	tmp3 = tmp1 + position * (tmp2 - tmp1) ;
	return ((int)tmp3) ;
}

get_wind_x_pos(position)
	float position ;
{
	register float tmp1, tmp2, tmp3 ;
	GETEM ;
	tmp1 = (float)wind_left ;
	tmp2 = (float)wind_rite ;
	tmp3 = tmp1 + position * (tmp2 - tmp1) ;
	return ((int)tmp3) ;
}

extern
getem()
{
	D_get_screen_window( &wind_top, &wind_bot,
		&wind_left, &wind_rite) ;
	notgotem = 0 ;
}
