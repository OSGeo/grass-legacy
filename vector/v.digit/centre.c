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
#include "global.h"
#include "proto.h"

/* This function is started from the GUI, it regularly updates GUI and checks GUI requirements. 
*  If Tool_next is set by GUI, the tool is started by the tool_centre()
*/
int
tool_centre ( void )
{
    int go = 1;

    while ( go ) {
        G_debug (3, "Tool centre: Tool_next = %d", Tool_next);
        i_update(); /* Let GUI set requests */
	switch ( Tool_next ) {
	    case TOOL_EXIT :
                G_debug (0, "Quit" );
		go = 0;
		break;
	    case TOOL_NEW_LINE :
		/* Tool_next = TOOL_NOTHING; */ /* Draw next one once first is done */
		new_line ();
		break;
	    case TOOL_DEL_LINE :
		delete_line ();
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
    G_debug (3, "Update function wx = %d wy = %d", wx, wy);
    i_update ();
    return 1;
}



