#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "global.h"
#include "proto.h"

/* Utilities */

/* For given type returns pointer to name */
char *
get_line_type_name ( int type)
{
    char *name;

    switch ( type ) {
        case GV_POINT:
	    name = G_store ( "point" );
	    break;
        case GV_LINE:
	    name = G_store ( "line" );
	    break;
        case GV_BOUNDARY:
	    name = G_store ( "boundary" );
	    break;
        case GV_CENTROID:
	    name = G_store ( "centroid" );
	    break;
        default:
	    name = G_store ( "unknown type" );
    }
    
    return name;
}

