#define GLOBAL
#include "P.h"
Pinit()
/*  Created from hpd550c driver by Garth Tier, 
    CSIRO Division of Wildlife and Ecology, 
    Alice Springs NT 0870, Australia
    Email: tie013@its.csiro.au*/
{
    NPIXELS = 2400; /* 300dpi x 8" page*/

    ras_row = 0;
    ras_nrows = 4;
    esc("E");	/* reset.  was ... */
                /* cancel any characters in printer buffer */
/*    Poutc ((char)24); */
/*    esc("4"); */	/* clear-parameters */
/*    esc("C10");*/	/* foreground black, no background */
    esc("&k2G");  /* LF to CR+LF  */
    esc("&l26A");	/* A4 paper */
    esc("&l0E");         /* No top Margin  */
    esc("*r-3U");        /*  CYM Palette */
    esc("*o2Q");         /* Raster Graphics Shingling  */
    esc("*o1D");         /* Raster Graphics depletion  */
    esc("*b10T");        /* I have no idea what this is */
    esc("*t300R");       /* 300 dpi graphics  */
    esc("*rA");          /* Start graphics at left */
    esc("*b0M");         /* No compression */
}
