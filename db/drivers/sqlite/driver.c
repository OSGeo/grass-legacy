/***********************************************************
*
* MODULE:       SQLite driver 
*   	    	
* AUTHOR(S):    Radim Blazek
*
* COPYRIGHT:    (C) 2005 by the GRASS Development Team
*
* This program is free software under the GNU General Public
* License (>=v2). Read the file COPYING that comes with GRASS
* for details.
*
**************************************************************/
#include <dbmi.h>
#include "globals.h"
#include "proto.h"

int
db__driver_init (argc, argv) char *argv[];
{
    init_error();
    return DB_OK;
}

int
db__driver_finish()
{
    return DB_OK;
}
