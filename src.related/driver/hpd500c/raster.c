#include "P.h"

/*  Created from hpd550c driver by Garth Tier, 
    CSIRO Division of Wildlife and Ecology, 
    Alice Springs NT 0870, Australia
    Email: tie013@its.csiro.au*/

    /* Flush raster code removed as Deskjet moves down anyway */

Praster ()
{
    ras_row = 0;
    flush_raster();
}

end_raster ()
{
    flush_raster();
}

flush_raster()
{
    ras_row = 0;
}
