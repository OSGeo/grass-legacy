#include "gis.h"
#include "raster.h"
#include "graph.h"

static int (*update_function)( int, int ) = NULL;

void R_set_update_function ( int (*fnc)( int, int ) )
{
    update_function = fnc;
}

int R_call_update_function ( int wx, int wy )
{
    if ( update_function != NULL )
        update_function ( wx, wy );
    return 1;
}
