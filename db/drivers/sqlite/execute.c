/***********************************************************
*
* MODULE:       SQLite driver 
*   	    	
* AUTHOR(S):    Radim Blazek
*               Transactions by Antonio Galea
*
* COPYRIGHT:    (C) 2005 by the GRASS Development Team
*
* This program is free software under the GNU General Public
* License (>=v2). Read the file COPYING that comes with GRASS
* for details.
*
**************************************************************/
#include <stdlib.h>
#include <grass/gis.h>
#include <grass/dbmi.h>
#include "globals.h"
#include "proto.h"

int db__driver_execute_immediate(dbString *sql)
{
    char *s;
    int  ret;
    sqlite3_stmt *stmt;
    char *rest = NULL;

    s = db_get_string (sql);

    G_debug ( 3, "execute: %s", s );
    
    ret = sqlite3_prepare ( sqlite, s, -1, &stmt, &rest );
    
    if ( ret != SQLITE_OK )
    {
        append_error("Error in sqlite3_prepare():\n");
	append_error ( sqlite3_errmsg(sqlite) );
        report_error( );
        return DB_FAILED;
    }

    ret = sqlite3_step ( stmt );

    if ( ret != SQLITE_DONE )
    {
        append_error("Error in sqlite3_step():\n");
	append_error ( sqlite3_errmsg(sqlite) );
        report_error( );
        return DB_FAILED;
    }

     ret = sqlite3_finalize ( stmt );

     if ( ret != SQLITE_OK )
     {
	 append_error("Error in sqlite3_finalize():\n");
	 append_error ( sqlite3_errmsg(sqlite) );
	 report_error( );
	 return DB_FAILED;
     }

     /*
     if ( rest )
	 G_free ( rest );
     */
     
     return DB_OK;
 }

int db__driver_begin_transaction(void)
{
   int  ret;
   G_debug ( 3, "execute: BEGIN" );
   
   ret = sqlite3_exec(sqlite,"BEGIN",NULL,NULL,NULL);
   if ( ret != SQLITE_OK )
   {
      append_error("Cannot 'BEGIN' transaction:\n");
      append_error ( sqlite3_errmsg(sqlite) );
      report_error( );
      return DB_FAILED;
   }

   return DB_OK;
}

int db__driver_commit_transaction(void)
{
   int  ret;
   G_debug ( 3, "execute: COMMIT" );
   
   ret = sqlite3_exec(sqlite,"COMMIT",NULL,NULL,NULL);
   if ( ret != SQLITE_OK )
   {
       append_error("Cannot 'COMMIT' transaction:\n");
       append_error ( sqlite3_errmsg(sqlite) );
       report_error( );
       return DB_FAILED;
   }

   return DB_OK;
}


