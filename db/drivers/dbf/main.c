/*****************************************************************************
*
* MODULE:       DBF driver 
*   	    	
* AUTHOR(S):    Radim Blazek
*
* PURPOSE:      Simple driver for reading and writing dbf files     
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#define MAIN
#include <stdlib.h>
#include <string.h>
#include <dbmi.h>
#include "globals.h"

int
main(int argc, char *argv[])
{
    char *name;
    
    /* Set pointer to driver name */
    name = argv[0] + strlen ( argv[0] );
    
    while ( name > argv[0] ) {
        if ( name[0] == '/' ) {
	    name++;
	    break;
	}
	name--;
    }
    
    G_debug ( 3, "driver file base name: '%s'", name);

    if ( strcmp ( name, "shp" ) == 0 ) {
	drv_mode = DBF_MODE_SHP;
        G_debug ( 3, "driver mode: DBF_MODE_SHP");
    } else {
	drv_mode = DBF_MODE_DBF;
        G_debug ( 3, "driver mode: DBF_MODE_DBF");
    }
	
    exit (db_driver (argc, argv));
}
