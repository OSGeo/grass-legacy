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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dbmi.h>
#include "globals.h"
#include "proto.h"

/* add column to table */
int add_column(int tab, int type, char *name, int width)
{
    int c;

    c = db.tables[tab].ncols;

    if (db.tables[tab].ncols == db.tables[tab].acols) {
	db.tables[tab].acols += 15;
	db.tables[tab].cols =
	    (COLUMN *) realloc(db.tables[tab].cols,
			       db.tables[tab].acols * sizeof(TABLE));
    }

    strcpy(db.tables[tab].cols[c].name, name);

    db.tables[tab].cols[c].type = type;
    db.tables[tab].cols[c].width = width;
/*    db.tables[tab].cols[c].decimals = decimals; */

    db.tables[tab].ncols++;

    return DB_OK;
}

/* returns column index or -1 */
int find_column(int tab, char *col)
{
    int i;

    for (i = 0; i < db.tables[tab].ncols; i++) {
	if (strcmp(db.tables[tab].cols[i].name, col) == 0)
	    return (i);
    }
    return (-1);
}
