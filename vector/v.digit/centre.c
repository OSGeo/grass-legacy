/* ***************************************************************
 * *
 * * MODULE:       v.digit
 * * 
 * * AUTHOR(S):    Radim Blazek
 * *               
 * * PURPOSE:      Edit vector
 * *              
 * * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 * *
 * *               This program is free software under the 
 * *               GNU General Public License (>=v2). 
 * *               Read the file COPYING that comes with GRASS
 * *               for details.
 * *
 * **************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "gis.h"
#include "display.h"
#include "global.h"
#include "proto.h"

/* This function is started from the GUI, it regularly updates GUI and checks GUI requirements. 
*  If Tool_next is set by GUI, the tool is started by the tool_centre()
*/
int
tool_centre ( void )
{
    int go = 1;

    symb_init_gui ( );
    
    while ( go ) {
        G_debug (5, "Tool centre: Tool_next = %d", Tool_next);
        i_update(); /* Let GUI set requests */
	switch ( Tool_next ) {
	    case TOOL_EXIT :
                G_debug (2, "Quit" );
		go = 0;
		break;
	    case TOOL_NEW_POINT :
		/* Tool_next = TOOL_NOTHING; */ /* Commented -> Draw next one once first is done */
		new_line ( GV_POINT );
		break;
	    case TOOL_NEW_LINE :
		new_line ( GV_LINE );
		break;
	    case TOOL_NEW_BOUNDARY :
		new_line ( GV_BOUNDARY );
		break;
	    case TOOL_NEW_CENTROID :
		new_line ( GV_CENTROID );
		break;
	    case TOOL_MOVE_VERTEX :
		Tool_next = TOOL_NOTHING;
		move_vertex ();
		break;
	    case TOOL_MOVE_LINE :
		Tool_next = TOOL_NOTHING;
		move_line ();
		break;
	    case TOOL_DELETE_LINE :
		Tool_next = TOOL_NOTHING;
		delete_line ();
		break;
	    case TOOL_ZOOM_WINDOW :
		Tool_next = TOOL_NOTHING;
		zoom_window ();
		break;
	    case TOOL_ZOOM_OUT_CENTRE :
		Tool_next = TOOL_NOTHING;
		zoom_centre ( 2 );
		break;
	    case TOOL_REDRAW :
		Tool_next = TOOL_NOTHING;
		driver_open();
		display_redraw();
		driver_close();
		break;
	    default:
	}
	i_prompt ( "Select tool");
	/* sleep ( 1 ); */
    }
    end();
    return 1;
}

/* This function is regularly called from R_get_location_*() functions to enable GUI to kill running tool */
int update ( int wx, int wy ) {
    double x, y;
    
    G_debug (5, "Update function wx = %d wy = %d", wx, wy);
    i_update ();

    if ( wx != COOR_NULL && wy != COOR_NULL ) {
        x = D_d_to_u_col ( wx ); 
	y = D_d_to_u_row ( wy );
        i_coor ( x, y);
    }
    
    return 1;
}



