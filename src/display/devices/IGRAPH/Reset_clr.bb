/*
 * The systems color represented by "number" is set using the color component
 * intensities found in the "red", "grn", and "blu" variables.  A value of
 * 0 represents 0 intensity; a value of 255 represents 100% intensity.
 *
 *  Functions in file:
 *    reset_color(number, red, grn, blu)
 *    Init_color_lookup() - creates a table of color intensities.
 *    load_color_table()
 *
 *  Written by the GRASS Team in the Winter of 88.
 *
 */


#include	<tools.h>
#include  "igraphics.h"

extern  int  VSNO ;
extern  int  NCOLORS ;

static  struct  vlt_slot  vlt[MAX_IGRAPH_COLORS+1] ;

static  unsigned short  color_lookup_table[MAX_COLOR_INTENSITY+1] ;

/*
*  The array color_lookup_table[] must be the same type as the
*  v_red, v_green, and v_blue in struct vlt_slot ;
*
*/

reset_color(number, red, grn, blu)
	int number ;
	int red, grn, blu ;
{
	unsigned int ui ;
	unsigned short us ;

	/*  check range (0-255) */
	if ( number < MIN_COLOR_INTENSITY || number > MAX_COLOR_INTENSITY)
		return(-1) ;

	vlt[number].v_slot = number ;

	vlt[number].v_red = color_lookup_table[red] ;
	vlt[number].v_green = color_lookup_table[grn] ;
	vlt[number].v_blue = color_lookup_table[blu] ;

	return(0) ;

}

Init_color_lookup() 
{

	int  i ;
	unsigned int ui ;
	unsigned short us ;

	for ( i = MIN_COLOR_INTENSITY;  i <= MAX_COLOR_INTENSITY ; i++)
	{
	ui = (unsigned int)i ;
	us = (unsigned short)( ui  << 8) ;
	color_lookup_table[i] = us ;
	}
}

load_color_table() 
{
	Loadvlt ( VSNO, vlt, get_num_colors() ) ;
}
