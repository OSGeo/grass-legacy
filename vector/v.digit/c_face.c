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

    if ( strcmp ( tl, "new_line" ) == 0 )
	Tool_next = TOOL_NEW_LINE;
    else if ( strcmp ( tl, "exit" ) == 0 )
	Tool_next = TOOL_EXIT;
    else if ( strcmp ( tl, "delete_line" ) == 0 )
	Tool_next = TOOL_DEL_LINE;
    else {
	G_warning ( "c_next_tool(): Unknown tool: %s", tl );
	return TCL_ERROR;
    }
	
    G_debug (2, "  Tool_next = %d", Tool_next);
    
    /* Stop running if any */
    R_set_cancel ( 1 );
    
    return TCL_OK;
}

int
c_tool_centre ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    tool_centre();
    return TCL_OK;
}

