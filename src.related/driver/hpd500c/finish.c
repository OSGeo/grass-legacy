#include "P.h"

/*  Created from hpd550c driver by Garth Tier, 
    CSIRO Division of Wildlife and Ecology, 
    Alice Springs NT 0870, Australia
    Email: tie013@its.csiro.au*/

    /* Escape sequences for HP Deskjet 500c added 
       These are the same as for the 550c */

Pfinish()
{
    esc("*rbC");  /* end raster graphics  */
    Palpha() ;	/* flush any remaining graphics */
    formfeed();
    esc("E");	/* reset for fun  */
    Pflush();
    sleep(3);  /* to allow timing problem with parallel ports to resolve */
}
