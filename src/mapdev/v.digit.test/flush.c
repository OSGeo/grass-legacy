#include "raster.h"

int V_flush (void)
{
    R_flush ();
    R_stabilize ();	/* needed for X windows */
    return 0;
}
