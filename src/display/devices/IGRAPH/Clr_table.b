/*******************
*
*This came from ../lib .  Had to be altered to add load_color_table()  ;
*(Loadvlt() )
*
*  Written by the GRASS Team in the Winter of 88.
*
*******************/


#include "colors.h"
static int n_colors = 0 ;
static int n_levels = 0 ;
static int table_type ;
static float span ;
static int Red[256], Grn[256], Blu[256] ;

Color_table_float()
{
	int i ;

	if(! can_do_float())
		return(-1) ;

	if (n_colors == 0)
	{
		Get_num_colors(&n_colors) ;
	}

/* Clear the entire color table */
	reset_color(0, 0, 0, 0) ;
	for(i=1; i<n_colors; i++)
		reset_color(i, 255, 255, 255) ;

	table_type = FLOAT ;
	Color_offset(0) ;

/* Generate standard color table */
	reset_color(RED,     255,   0,   0) ;
	reset_color(ORANGE,  255, 127,   0) ;
	reset_color(YELLOW,  255, 255,   0) ;
	reset_color(GREEN,     0, 255,   0) ;
	reset_color(BLUE,      0,   0, 255) ;
	reset_color(INDIGO,    0, 127, 255) ;
	reset_color(VIOLET,  255,   0, 255) ;
	reset_color(WHITE,   255, 255, 255) ;
	reset_color(BLACK,     0,   0,   0) ;
	reset_color(GRAY,    127, 127, 127) ;
	reset_color(BROWN,   180,  75,  25) ;
	reset_color(MAGENTA, 255,   0, 127) ;
	reset_color(AQUA,    100, 127, 255) ;

	load_color_table() ;

	return(0) ;
}

Color_table_fixed()
{
	int r, g, b ;
	unsigned char R, G, B ;
	int i ;
	int n_levels_sq = 0 ;

	table_type = FIXED ;

/* figure out how many equal levels of r, g, and b are possible with the
 * available colors */
	if (n_levels == 0)
	{
		Get_num_colors(&n_colors) ;
/*
		for(n_levels=0; n_levels*n_levels*n_levels < n_colors; n_levels++)
*/
		for(n_levels=0; n_levels*n_levels*n_levels <= n_colors; n_levels++)
			;
		n_levels-- ;
		span = 255.0 / (float)(n_levels - 1) ;
	
	/* Create easy lookup for _get_look_for_color() */
		for(i=0; i<256; i++)
		{
			Red[i] = (int)((i / 256.0) * n_levels) * n_levels * n_levels ;
			Grn[i] = (int)((i / 256.0) * n_levels) * n_levels ;
			Blu[i] = (int)((i / 256.0) * n_levels) ;
		}
	}

/* Generate "fixed" color table */
	i = 0 ;

	for(r=0; r<n_levels; r++)
	{
		R = (int)(r * span) ;
		for(g=0; g<n_levels; g++)
		{
			G = (int)(g * span) ;
			for(b=0; b<n_levels; b++)
			{
				B = (int)(b * span) ;
				reset_color(i++, R, G, B) ;
			}
		}
	}

	load_color_table() ;

/* Generate lookup for "standard" colors */
	assign_standard_color(RED,     _get_lookup_for_color(255,   0,   0)) ;
	assign_standard_color(ORANGE,  _get_lookup_for_color(255, 128,   0)) ;
	assign_standard_color(YELLOW,  _get_lookup_for_color(255, 255,   0)) ;
	assign_standard_color(GREEN,   _get_lookup_for_color(  0, 255,   0)) ;
	assign_standard_color(BLUE,    _get_lookup_for_color(  0,   0, 255)) ;
	assign_standard_color(INDIGO,  _get_lookup_for_color(  0, 128, 255)) ;
	assign_standard_color(VIOLET,  _get_lookup_for_color(255,   0, 255)) ;
	assign_standard_color(BLACK,   _get_lookup_for_color(  0,   0,   0)) ;
	assign_standard_color(WHITE,   _get_lookup_for_color(255, 255, 255)) ;
	assign_standard_color(GRAY,    _get_lookup_for_color(175, 175, 175)) ;
	assign_standard_color(BROWN,   _get_lookup_for_color(180,  77,  25)) ;
	assign_standard_color(MAGENTA, _get_lookup_for_color(255,   0, 128)) ;
	assign_standard_color(AQUA,    _get_lookup_for_color(100, 128, 255)) ;

	return(0) ;
}

_get_lookup_for_color(red, grn, blu)
	int red, grn, blu ;
{
/*
	return( (int)(((float)red / 256. ) * n_levels) * n_levels * n_levels
		  + (int)(((float)grn / 256.)  * n_levels) * n_levels
		  + (int)(((float)blu / 256.)  * n_levels) ) ;
*/
	return( Red[red] + Grn[grn] + Blu[blu] ) ;
}

get_table_type()
{
	return table_type ;
}
