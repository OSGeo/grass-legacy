#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <tcl.h>
#include <tk.h>
#include "gis.h"
#include "raster.h"
#include "global.h"
#include "proto.h"

/* Interface functions between GUI and C:
*  c_*() functions in C, called from GUI, in this case from Tk
*/

/* Request to cancel running tool */
int
c_cancel ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    G_debug (3, "c_cancel()");
    R_set_cancel ( 1 );
    Tool_next = TOOL_NOTHING;
    return TCL_OK;
}

/* set the next tool to start */
int
c_next_tool ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    char *tl;

    G_debug (3, "c_next_tool()");
    if ( argc < 2 ) {
	G_warning ( "c_next_tool(): inicorrect number of parameters" );
	return TCL_ERROR;
    }
    tl = argv[1];

    if ( strcmp ( tl, "new_point" ) == 0 )
	Tool_next = TOOL_NEW_POINT;
    else if ( strcmp ( tl, "new_line" ) == 0 )
	Tool_next = TOOL_NEW_LINE;
    else if ( strcmp ( tl, "new_boundary" ) == 0 )
	Tool_next = TOOL_NEW_BOUNDARY;
    else if ( strcmp ( tl, "new_centroid" ) == 0 )
	Tool_next = TOOL_NEW_CENTROID;
    else if ( strcmp ( tl, "move_vertex" ) == 0 )
	Tool_next = TOOL_MOVE_VERTEX;
    else if ( strcmp ( tl, "move_line" ) == 0 )
	Tool_next = TOOL_MOVE_LINE;
    else if ( strcmp ( tl, "delete_line" ) == 0 )
	Tool_next = TOOL_DELETE_LINE;
    else if ( strcmp ( tl, "exit" ) == 0 )
	Tool_next = TOOL_EXIT;
    else if ( strcmp ( tl, "zoom_window" ) == 0 )
	Tool_next = TOOL_ZOOM_WINDOW;
    else if ( strcmp ( tl, "zoom_out_centre" ) == 0 )
	Tool_next = TOOL_ZOOM_OUT_CENTRE;
    else if ( strcmp ( tl, "redraw" ) == 0 )
	Tool_next = TOOL_REDRAW;
    else {
	G_warning ( "c_next_tool(): Unknown tool: %s", tl );
	return TCL_ERROR;
    }
	
    G_debug (2, "  Tool_next = %d", Tool_next);
    
    /* Stop running if any */
    R_set_cancel ( 1 );
    
    return TCL_OK;
}

/* set color */
int
c_set_color ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    int code;

    G_debug (2, "c_set_color()");
    if ( argc < 5 ) {
	G_warning ( "c_set_color(): inicorrect number of parameters" );
	return TCL_ERROR;
    }

    code = get_symb_code ( argv[1] );

    if ( code < 0 ) {
	G_warning ( "c_set_color(): Unknown symb name: %s", argv[1] );
	return TCL_ERROR;
    }
	
    G_debug (2, "symb = %d", code);
    G_debug (2, " %s %s %s", argv[2], argv[3], argv[4]);
    
    Symb[code].r = atoi (argv[2]);
    Symb[code].g = atoi (argv[3]);
    Symb[code].b = atoi (argv[4]);
    
    return TCL_OK;
}

/* set layer on/off */
int
c_set_on ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    int code;

    G_debug (2, "c_set_on()");
    if ( argc < 3 ) {
	G_warning ( "c_set_on(): inicorrect number of parameters" );
	return TCL_ERROR;
    }

    code = get_symb_code ( argv[1] );

    if ( code < 0 ) {
	G_warning ( "c_set_on(): Unknown symb name: %s", argv[1] );
	return TCL_ERROR;
    }
	
    G_debug (2, "symb = %d on = %d", code, atoi (argv[2]) );
    
    Symb[code].on = atoi (argv[2]);
    
    return TCL_OK;
}

/* Set snapping */
int
c_set_snap ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    G_debug (2, "c_set_snap()");
    if ( argc < 3 ) {
	G_warning ( "c_set_snap(): inicorrect number of parameters" );
	return TCL_ERROR;
    }
    if ( strcmp(argv[1], "snap") == 0) {
	Snap = atoi(argv[2]);
    } else if ( strcmp(argv[1], "snap_mode") == 0) { 
	Snap_mode = atoi(argv[2]);
    } else if ( strcmp(argv[1], "snap_screen") == 0) { 
	Snap_screen = atoi(argv[2]);
    } else if ( strcmp(argv[1], "snap_map") == 0) { 
	Snap_map = atof(argv[2]);
    }

    G_debug (2, "Snap = %d, Snap_mode = %d, Snap_screen = %d, Snap_map = %f", Snap, Snap_mode, 
	         Snap_screen, Snap_map);
    
    return TCL_OK;
}

int
c_tool_centre ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    tool_centre();
    return TCL_OK;
}

/* set line cats */
int
c_set_cat ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    int idx, val;

    G_debug (2, "c_set_cat()");
    if ( argc < 4 ) {
	G_warning ( "c_set_cat(): inicorrect number of parameters" );
	return TCL_ERROR;
    }
    
    idx = atoi (argv[2]);
    val = atoi (argv[3]);
    
    G_debug (2, "Set %s[%d] to %d", argv[1], idx, val);

    if ( strcmp(argv[1], "field") == 0 ) {
	FieldCat[idx][0] = val;
	i_new_line_cat_set_next();
    } else if ( strcmp(argv[1], "cat") == 0 ) {
	FieldCat[idx][1] = val;
    } else {
	G_warning ( "c_set_cat(): inicorrect parameter '%s'", argv[1] );
	return TCL_ERROR;
    }
    
    return TCL_OK;
}

/* set line cat mode */
int
c_set_cat_mode ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    G_debug (2, "c_set_cat_mode() mode = %s", argv[1] );
    
    CatMode = atoi ( argv[1] );
    
    i_set_cat_mode ();
    return TCL_OK;
}

