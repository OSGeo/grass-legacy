#include <stdio.h>
#include "colors.h"

Reset_colors(min, max, red, grn, blu)
    int min, max ;
    unsigned char *red, *grn, *blu ;
{
    if (get_table_type() == FLOAT)
	_float_Reset_colors(min, max, red, grn, blu) ;
    else
	_fixed_Reset_colors(min, max, red, grn, blu) ;
}

Reset_color(red, grn, blu, num)
    unsigned char red, grn, blu ;
    int num ;
{
    unsigned char r, g, b ;
    r = red; g = grn; b = blu;
    Reset_colors (num, num, &r, &g, &b);
}

static
_fixed_Reset_colors(min, max, red, grn, blu)
    int min, max ;
    unsigned char *red, *grn, *blu ;
{
    int incr ;
    int n ;

    assign_fixed_color(min,
	_get_lookup_for_color((int)red[0], (int)grn[0], (int)blu[0]));
/* assign min, then max, then those in between. This
 * allows memory allocation to happen only once or twice
 */
    if (min < max)
    {
	n = max-min;
	assign_fixed_color(max,
	    _get_lookup_for_color((int)red[n], (int)grn[n], (int)blu[n]));
	for (incr=min+1,n=1; incr<max; incr++,n++)
	    assign_fixed_color(incr,
		_get_lookup_for_color((int)red[n], (int)grn[n], (int)blu[n]));
    }
}

static
_float_Reset_colors(min, max, red, grn, blu)
    int min, max ;
    unsigned char *red, *grn, *blu ;
{
    int incr ;
    int n ;

    _float_Reset_color(red[0], grn[0], blu[0], min) ;
    if (min < max)
    {
	n = max-min;
	_float_Reset_color(red[n], grn[n], blu[n], max) ;
	for (incr=min+1,n=1; incr<max; incr++,n++)
	    _float_Reset_color(red[n], grn[n], blu[n], incr) ;
    }
}

static
_float_Reset_color(red, grn, blu, incr)
    unsigned char red, grn, blu ;
    int incr ;
{
    reset_color(incr + get_color_offset() + get_max_std_colors(), red, grn, blu ) ;
}
