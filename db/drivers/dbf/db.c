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

#include <dirent.h>
#include <gis.h>
#include <dbmi.h>
#include "globals.h"
#include "proto.h" 

db_driver_open_database (handle)
    dbHandle *handle;
{
    char   *name, emsg[DBF_MSG];
    int    len; 
    dbConnection connection;
    char   buf[1024];
    DIR    *dir;
    struct dirent *ent;

    db.name[0] = '\0';
    db.tables = NULL;
    db.atables = 0;
    db.ntables = 0;
    
    db_get_connection( &connection );
    name = db_get_handle_dbname(handle);
    
    /* if name is empty use connection.databaseName*/
    if( strlen(name) == 0 )
    {
        name = connection.databaseName;
    } 

    strcpy ( db.name, name );
    
    /* open database dir and read table ( *.dbf files ) names 
     * to structure */ 
    dir = opendir(db.name);
    if (dir == NULL)
      {
	snprintf( emsg, sizeof(emsg), "Cannot open dbf database: %s\n", name );
	report_error( emsg );
	return DB_FAILED;
      }
    
    while ( ent = readdir (dir) )
      {         
	len = strlen ( ent->d_name ) - 4;      
	if ( (len > 0) && (strcmp ( ent->d_name + len, ".dbf") == 0) )
          {
	    strcpy ( buf, ent->d_name );
	    buf[len] = '\0';
            add_table ( buf );
	  }
      } 
    
    closedir ( dir );
    return DB_OK;
}

int
db_driver_close_database()
{
    int i, j;

    for ( i = 0; i < db.ntables; i++)
      {
	save_table (i);  
	free_table (i);  
      }
    free ( db.tables );
    
    return DB_OK;
}

