/*****************************************************************************
*
* MODULE:       MySQL driver forked from DBF driver by Radim Blazek 
*   	    	
* AUTHOR(S):    Alex Shevlakov
*
* PURPOSE:      Simple driver for reading and writing data     
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
#include <dbmi.h>
#include "globals.h"
#include "dbdriver.h"

int main(argc, argv)
     char *argv[];
{
	init_dbdriver();
	exit(db_driver(argc, argv));
}
