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
#include "proto.h"

int db__driver_execute_immediate(sql)
     dbString *sql;
{
    char *s;
    int ret;

    s = db_get_string(sql);

    ret = execute(s, NULL);

    if (ret == DB_FAILED) {
	sprintf(errMsg, "%sError in db_execute_immediate()", errMsg);
	report_error(errMsg);
	return DB_FAILED;
    }

    return DB_OK;
}
