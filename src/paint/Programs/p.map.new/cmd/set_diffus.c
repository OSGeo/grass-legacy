#include "gis.h"
#include "colorsdiff.h"
#include "graphics.h"
#include "local_proto.h"

int set_diffusion_color (int w)
{
    int prows, pcols;
    int i;
    int red = 0, grn = 0, blu = 0;
    short red_carry_right;
    short grn_carry_right;
    short blu_carry_right;


    prows 	=  graphics.window.right-graphics.window.left;
    pcols 	=  graphics.window.bottom-graphics.window.top;

    red_carry_below	= (short *)G_malloc (pcols * sizeof (short));
    grn_carry_below	= (short *)G_malloc (pcols * sizeof (short));
    blu_carry_below	= (short *)G_malloc (pcols * sizeof (short));



    G_zero (red_carry_below, pcols * sizeof (short));
    G_zero (grn_carry_below, pcols * sizeof (short));
    G_zero (blu_carry_below, pcols * sizeof (short));

    red_carry_right = 0;
    blu_carry_right = 0;
    grn_carry_right = 0;

    for (i=0; i<w; i++) { 
        red += red_carry_right + red_carry_below[i];
        grn += grn_carry_right + grn_carry_below[i];
        blu += blu_carry_right + blu_carry_below[i];
        red_carry_below[i] =  red_carry_right = red_carryover(red)/2;
        grn_carry_below[i] =  grn_carry_right = grn_carryover(grn)/2;
        blu_carry_below[i] =  blu_carry_right = blu_carryover(blu)/2;
    }

    return 0;
}
