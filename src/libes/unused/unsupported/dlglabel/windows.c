/*  @(#)windows.c	2.1  6/26/87  */
#include "convert.h"
#include "dlg.h"

push_window()
{
	Clear_menu() ;
	if (c_wind >= MAX_WINDOWS-1)
	{
		Write_message(1, "No more windows") ;
		sleep(2) ;
		Clear_message() ;
		return(-1) ;
	}
	if ( make_window() )
		return(-1) ;

	c_wind++ ;
	_west[c_wind] = U_west ;
	_east[c_wind] = U_east ;
	_south[c_wind] = U_south ;
	_north[c_wind] = U_north ;
	do_conversions() ;
	outline_map() ;
	return(0) ;
}

pop_window()
{
	if (c_wind)
	{
		c_wind-- ;
		U_west = _west[c_wind] ;
		U_east = _east[c_wind] ;
		U_south = _south[c_wind] ;
		U_north = _north[c_wind] ;
		do_conversions() ;
		outline_map() ;
	}
}

auto_area_zoom(choice)
	int choice ;
{
	double NORTH, SOUTH, EAST, WEST ;
	double i ;

	NORTH = area[choice].N ;
	SOUTH = area[choice].S ;
	EAST  = area[choice].E ;
	WEST  = area[choice].W ;

	i = NORTH - SOUTH ;
	NORTH += i * .1 ;
	SOUTH -= i * .1 ;
	i = EAST - WEST ;
	EAST += i * .1 ;
	WEST -= i * .1 ;

	c_wind++ ;
	_west[c_wind]  = U_west  = WEST  ;
	_east[c_wind]  = U_east  = EAST  ;
	_south[c_wind] = U_south = SOUTH ;
	_north[c_wind] = U_north = NORTH ;
	do_conversions() ;
	outline_map() ;
	return(0) ;
}
