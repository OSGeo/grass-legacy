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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <gis.h>
#include <dbmi.h>
#include "globals.h"
#include "proto.h" 

/* add column to table */
int add_column (int tab, int type, char *name, int width, int decimals)
{
    int c;

    G_debug (3, "add_column(): tab = %d, type = %d, name = %s, width = %d, decimals = %d",
	                       tab, type, name, width, decimals);

    /* Check if the column exists */
    for ( c = 0; c < db.tables[tab].ncols; c++ ) {
	if ( G_strcasecmp( db.tables[tab].cols[c].name, name ) == 0 ) {
	    append_error( "Column '%s' already exists (duplicate name)\n", name);
	    return DB_FAILED;
	}
    }
	    
    c = db.tables[tab].ncols; 
    
    if ( db.tables[tab].ncols == db.tables[tab].acols )
      {
        db.tables[tab].acols += 15; 
	db.tables[tab].cols = (COLUMN *) realloc ( db.tables[tab].cols, db.tables[tab].acols * sizeof (TABLE) ); 
      }
    
    strncpy ( db.tables[tab].cols[c].name, name, DBF_COL_NAME-1 );
    db.tables[tab].cols[c].name[DBF_COL_NAME-1] = '\0';
    
    db.tables[tab].cols[c].type = type; 
    db.tables[tab].cols[c].width = width; 
    db.tables[tab].cols[c].decimals = decimals; 
    
    db.tables[tab].ncols++; 
    
    return DB_OK;
}

/* returns column index or -1 */
int find_column (int tab, char *col)
{
    int i;
     
    for ( i = 0; i < db.tables[tab].ncols; i++ )
      {
        if ( G_strcasecmp( db.tables[tab].cols[i].name, col ) == 0 )
            return (i);
      } 
    return (-1);
}

