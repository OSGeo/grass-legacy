/*****************************************************************************
*
* MODULE:       PostgreSQL driver forked from DBF driver by Radim Blazek 
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
#include <dirent.h>
#include <gis.h>
#include <dbmi.h>
#include "globals.h"
#include "proto.h" 

/* add table to database */
int add_table (char *table)
{
    if ( db.atables == db.ntables )
      {
        db.atables += 15; 
	db.tables = (TABLE *) realloc ( db.tables, db.atables * sizeof (TABLE) ); 
      }

    
    strcpy ( db.tables[db.ntables].name, table );
    
    sprintf ( db.tables[db.ntables].file, "%s/%s.dbf", db.name, table );
    
    db.tables[db.ntables].alive = TRUE;
    db.tables[db.ntables].described = FALSE;
    db.tables[db.ntables].loaded = FALSE;
    db.tables[db.ntables].updated = FALSE;
    db.tables[db.ntables].cols = NULL;
    db.tables[db.ntables].rows = NULL;
    db.tables[db.ntables].acols = 0;
    db.tables[db.ntables].ncols = 0;
    db.tables[db.ntables].arows = 0;
    db.tables[db.ntables].nrows = 0;

    db.ntables++ ;
    
    return DB_OK;
}


/* returns table index or -1 */
int find_table (char *table)
{
    int i;
	
    for ( i = 0; i < db.ntables; i++ )
      {
         if ( strcmp( db.tables[i].name, table ) == 0 )
	     return (i);   
      }
    
    return (-1);
}

int
load_table_head( int t)
{
    db.tables[t].described = TRUE;
	
    return DB_OK;
}

int
load_table ( int t)
{
    db.tables[t].loaded = TRUE;
    
    return DB_OK;
}

int
save_table ( int t)
{
    return DB_OK;
}

int free_table (int tab)
{
    int i,j;

    for ( i = 0; i < db.tables[tab].nrows; i++ )
      {
	for( j = 0; j < db.tables[tab].ncols; j++ )
	  {
            if ( db.tables[tab].cols[j].type == DBF_CHAR )
	      {	    
                free ( db.tables[tab].rows[i].values[j].c );
	      }
	  }
        free ( db.tables[tab].rows[i].values );
      }
    
    free ( db.tables[tab].rows );
	      
    return DB_OK;
}


