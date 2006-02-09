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
#include <grass/dbmi.h>
#include "globals.h"
#include "proto.h"

int
db__driver_create_index ( dbIndex *index )
{
    int i, ncols;
    sqlite3_stmt *statement;
    dbString sql;
    char  *rest;
    int   ret;
    
    G_debug (3, "db__create_index()");

    db_init_string (&sql);
    init_error();

    ncols = db_get_index_number_of_columns ( index );

    db_set_string ( &sql, "create" );
    if ( db_test_index_type_unique(index) ) 
	db_append_string ( &sql, " unique" );

    db_append_string ( &sql, " index " );
    db_append_string ( &sql, db_get_index_name(index) );
    db_append_string ( &sql, " on " );
    
    db_append_string ( &sql, db_get_index_table_name(index) );
    
    db_append_string ( &sql, " ( " );

    for ( i = 0; i < ncols; i++ ) {
	if ( i > 0 )
            db_append_string ( &sql, ", " );
	
        db_append_string ( &sql, db_get_index_column_name(index,i) );
    }
    
    db_append_string ( &sql, " )" );

    G_debug (3, " SQL: %s", db_get_string(&sql) );
    
    ret = sqlite3_prepare ( sqlite, db_get_string(&sql), -1,
                            &statement, &rest );

    if ( ret != SQLITE_OK ) {
        append_error( "Cannot create index:\n");
	append_error( db_get_string(&sql) );
	append_error( "\n" );
	append_error ( sqlite3_errmsg(sqlite) );
	report_error();
        sqlite3_finalize ( statement );
	db_free_string ( &sql);
	return DB_FAILED;
    }

    ret = sqlite3_step ( statement );

    if ( ret != SQLITE_DONE )
    {
        append_error("Error in sqlite3_step():\n");
        append_error ( sqlite3_errmsg(sqlite) );
        report_error( );
        return DB_FAILED;
    }
    
    sqlite3_finalize ( statement );
    db_free_string ( &sql);
    
    return DB_OK;
}
