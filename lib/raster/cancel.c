#include "gis.h"
#include "raster.h"
#include "graph.h"

static int cancel = 0;

void R_set_cancel ( int v )
{
    cancel = v;
}

int R_get_cancel ( void )
{
    return cancel;
}
