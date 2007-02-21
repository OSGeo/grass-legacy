#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/raster.h>
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

static int sxo, syo, mode;

void set_location(int x, int y)
{
    sxo = x;
    syo = y;
}

void set_mode(int m)
{
    mode = m;
}

void get_location(int *sxn, int *syn, int *button)
{
    R_set_update_function (update);

    switch (mode)
    {
    case MOUSE_POINT:
	R_get_location_with_pointer (sxn, syn, button);
	break;
    case MOUSE_LINE:
	R_get_location_with_line (sxo, syo, sxn, syn, button); 
	break;
    case MOUSE_BOX:
	R_get_location_with_box (sxo, syo, sxn, syn, button); 
	break;
    }
}

int do_tool(tool_func_begin *begin, tool_func_update *update, tool_func_end *end, void *closure)
{
    int sxn = COOR_NULL;
    int syn = COOR_NULL;
    int button;
    int ret;

    driver_open();
    ret = (*begin)(closure);

    if (ret)
    {
	driver_close();
	return ret;
    }

    while (1)
    {
	/* Get next coordinate */
        get_location(&sxn, &syn, &button); 

	if (button == 0)
	    break;

	if ((*update)(closure, sxn, syn, button))
	    break;
    }

    ret = (*end)(closure);
    driver_close();

    return ret;
}

