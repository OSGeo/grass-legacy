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

int db__driver_open_select_cursor(sel, dbc, mode)
     dbString *sel;
     dbCursor *dbc;
     int mode;
{
    int ret;
    cursor *c;
    char *sql;
    dbTable *table;

    /* allocate cursor */
    c = alloc_cursor();
    if (c == NULL)
	return DB_FAILED;

    db_set_cursor_mode(dbc, mode);
    db_set_cursor_type_readonly(dbc);

    sql = db_get_string(sel);

    ret = execute(sql, c);

    if (ret == DB_FAILED) {
	sprintf(errMsg, "%sError in db_open_select_cursor()", errMsg);
	report_error(errMsg);
	return DB_FAILED;
    }

    describe_table(c->table, c->cols, c->ncols, &table);

    /* record table with dbCursor */
    db_set_cursor_table(dbc, table);

    /* set dbCursor's token for my cursor */
    db_set_cursor_token(dbc, c->token);

    return DB_OK;
}
