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

#include <dbmi.h>
#include "globals.h"

void report_error(err)
     char *err;
{
    char msg[MYSQL_MSG];

    snprintf(msg, sizeof(msg), "DBMI-MYSQL driver error: %s", err);
    db_error(msg);
}
