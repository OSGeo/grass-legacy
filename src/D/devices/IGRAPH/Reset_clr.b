/*
 * The systems color represented by "number" is set using the color component
 * intensities found in the "red", "grn", and "blu" variables.  A value of
 * 0 represents 0 intensity; a value of 255 represents 100% intensity.
 *
 *  One other function in this file load_color_table().
 *
 *  Written by the GRASS Team in the Winter of 88.
 *
 */


#include	<tools.h>
#include  "igraphics.h"

#define LOW_COLOR 0
#define HIGH_COLOR 255

extern  int  VSNO ;
extern  int  NCOLORS ;

static  struct  vlt_slot  vlt[MAX_IGRAPH_COLORS] ;

reset_color(number, red, grn, blu)
	int number ;
	int red, grn, blu ;
{
	unsigned int ui ;
	unsigned short us ;

	/*  check range  */
	if ( number < LOW_COLOR || number > HIGH_COLOR)
		return(-1) ;

	vlt[number].v_slot = number ;
	ui = (unsigned int)red ;
	us = (unsigned short)( ui  << 8) ;
	vlt[number].v_red = us ;

	ui = (unsigned int)grn ;
	us = (unsigned short)( ui  << 8) ;
	vlt[number].v_green =  us ;

	ui = (unsigned int)blu ;
	us = (unsigned short)( ui  << 8) ;
	vlt[number].v_blue = us ;

	return(0) ;

}

load_color_table() 
{
	Loadvlt ( VSNO, vlt, get_num_colors() ) ;
}
