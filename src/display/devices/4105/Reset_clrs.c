#include <stdio.h>
#include "graphics.h"

Reset_colors(red, grn, blu, tot_colr)
	float *red, *grn, *blu ;
	int tot_colr ;
{
	int n ;
	for (n=0; n<tot_colr; n++)
		Reset_color(red[n], grn[n], blu[n], n) ;
}

Reset_color(red, grn, blu, number)
	float red, grn, blu ;
	int number ;
{
	extern int color_lookup[] ;
	int index ;
	if (red >= 1.0) red = .99 ;
	if (grn >= 1.0) grn = .99 ;
	if (blu >= 1.0) blu = .99 ;
	if (red < 0.0) red = 0. ;
	if (grn < 0.0) grn = 0. ;
	if (blu < 0.0) blu = 0. ;
	index = 50 + (int)( blu / .20)
	           + 5 * (int)( grn / .20)
	           + 25 * (int)( red / .20) ;
	color_lookup[number] = index ;
}
