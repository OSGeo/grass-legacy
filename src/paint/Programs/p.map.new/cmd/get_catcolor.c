#include "gis.h"
#include "misc.h"
#include "colormode.h"
#include "colorsdiff.h"
#include "parms.h"
#include "local_proto.h"


int get_catcolr(DCELL val, int rr,int cc)
{
    unsigned char white;
    unsigned char black;
	/* color carryovers */
    short red_carry_right;
    short grn_carry_right;
    short blu_carry_right;
	int c,red,grn,blu;
	DCELL dtmp=val;


    white = WHITE ;
    black = BLACK ;


    red_carry_right = red_carry_below[cc];
    grn_carry_right = grn_carry_below[cc];
    blu_carry_right = blu_carry_below[cc];


    G_get_d_raster_color (&dtmp, &red, &grn, &blu, &(parms.pcolr));
    if (colormode == COLORMODE_DIFFUSION)
    {
        red += red_carry_right + red_carry_below[cc];
        grn += grn_carry_right + grn_carry_below[cc];
        blu += blu_carry_right + blu_carry_below[cc];
    }
    else if (colormode == COLORMODE_DITHER)
    {
        unsigned char x;
        x = red; red_dither (&x, rr, cc, 1); red = x;
        x = grn; grn_dither (&x, rr, cc, 1); grn = x;
        x = blu; blu_dither (&x, rr, cc, 1); blu = x;
    }

    if(parms.map_type==CELL_TYPE)
        c = lookup_from_pattern((CELL)val,rr,cc);
    else 
	c = -1;
    if (c < 0)
	c = printer_color_number (red,grn,blu);

    return c;
}
