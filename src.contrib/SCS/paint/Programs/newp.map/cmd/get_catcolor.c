#include "gis.h"
#include "misc.h"
#include "colormode.h"
#include "colorsdiff.h"
#include "parms.h"


int get_catcolr(catnum,rr, cc )
	int catnum;
	int rr, cc;
{
    unsigned char white;
    unsigned char black;
	int	count;
	int prows, pcols;
	/* color carryovers */
    short red_carry_right;
    short grn_carry_right;
    short blu_carry_right;
	int i,c,red,grn,blu;


    white = WHITE ;
    black = BLACK ;


		red_carry_right = red_carry_below[cc];
		grn_carry_right = grn_carry_below[cc];
		blu_carry_right = blu_carry_below[cc];


	G_get_color ((CELL)catnum, &red, &grn, &blu, parms.pcolr);
		    if (colormode == COLORMODE_DIFFUSION)
		    {
			red += red_carry_right + red_carry_below[cc-1];
			grn += grn_carry_right + grn_carry_below[cc-1];
			blu += blu_carry_right + blu_carry_below[cc-1];
		    }
		    else if (colormode == COLORMODE_DITHER)

		    {
			unsigned char x;
			x = red; red_dither (&x, rr, cc, 1); red = x;
			x = grn; grn_dither (&x, rr, cc, 1); grn = x;
			x = blu; blu_dither (&x, rr, cc, 1); blu = x;
		    }

		    c = lookup_from_pattern(catnum,rr,cc);
		    if (c < 0)
			c = printer_color_number (red,grn,blu);

			return c;
}

