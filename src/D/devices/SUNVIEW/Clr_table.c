#include "../lib/colors.h"
#include "graphics.h"
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
	span = 255.0 / (float)(n_levels-1) ;

	for(r=0; r<n_levels; r++)
	{
		R = (int)(r * span ) ;
		for(g=0; g<n_levels; g++)
		{
			G = (int)(g * span ) ;
			for(b=0; b<n_levels; b++)
			{
				B = (int)(b * span ) ;
				reset_color(i++, R, G, B) ;
			}
		}
	}

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
	return( Red[red] + Grn[grn] + Blu[blu] ) ;
}

_get_lookup_for_color_with_leftover (red, grn, blu, rd, gd, bd)
	int red, grn, blu ;
	unsigned char *rd, *gd, *bd ;
{
	static unsigned char *r, *g, *b ;
	int extra_red, extra_grn, extra_blu ;
	int ri, gi, bi ;
	int ret_val ;
	static unsigned char first = 1 ;
	char *malloc() ;

	if(table_type != FIXED)
		Color_table_fixed() ;

	if(first)
	{
		int i ;
		r = (unsigned char *)malloc(256) ;
		g = (unsigned char *)malloc(256) ;
		b = (unsigned char *)malloc(256) ;
        pw_setcmsname(pixwin, "grass");
		/*
		pw_getcolormap(pixwin, 0, 256, r, g, b);
		*/
		first = 0 ;
		for(i=0; i<256; i++)
		{
			pw_getcolormap(pixwin, i, 1, &r[i], &g[i], &b[i]);
		}
	}

	extra_red = extra_grn = extra_blu = 0 ;

/* Add in previous left-over */
	ri = red + *rd ;
	gi = grn + *gd ;
	bi = blu + *bd ;

/* Check if intensity over 100% */
	if (ri > 255) { extra_red = ri - 255 ; ri = 255 ; }
	if (gi > 255) { extra_grn = gi - 255 ; gi = 255 ; }
	if (bi > 255) { extra_blu = bi - 255 ; bi = 255 ; }

	ret_val =  Red[ri] + Grn[gi] + Blu[bi] ;

	extra_red += ri - r[ret_val] ;
	extra_grn += gi - g[ret_val] ;
	extra_blu += bi - b[ret_val] ;

	*rd = extra_red / 2 ;
	*gd = extra_grn / 2 ;
	*bd = extra_blu / 2 ;

	return(ret_val) ;
}

get_table_type()
{
	return table_type ;
}
