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
#include <string.h>
#include <grass/dbmi.h>
#include <grass/datetime.h>
#include "globals.h"
#include "proto.h"
#include <grass/glocale.h>

int db__driver_describe_table (dbString *table_name, dbTable **table)

{
    dbString sql;
    sqlite3_stmt *statement;
    char  *rest;
    int   ret;

    db_init_string ( &sql );

    db_set_string( &sql, "select * from ");
    db_append_string ( &sql, db_get_string(table_name) );
    db_append_string( &sql, " where oid < 0");

    ret = sqlite3_prepare ( sqlite, db_get_string(&sql), -1,
                            &statement, &rest );


    if ( ret != SQLITE_OK )
    {
        append_error("Error in sqlite3_prepare():");
        append_error( db_get_string(&sql) );
        append_error( "\n" );
        append_error ( sqlite3_errmsg(sqlite) );
        report_error( );
        db_free_string ( &sql );
        return DB_FAILED;
    }
        
    db_free_string ( &sql );

    if ( describe_table( statement, table, NULL) == DB_FAILED ) {
	append_error("Cannot describe table\n");
	report_error();
        sqlite3_finalize ( statement );
	return DB_FAILED;
    }

    sqlite3_finalize ( statement );

    return DB_OK;
}

/* describe table, if c is not NULL cur->cols and cur->ncols is also set */
int describe_table( sqlite3_stmt *statement, 
			dbTable **table, cursor *c)
{
    int  i, ncols, nkcols;

    G_debug (3, "describe_table()");

    ncols = sqlite3_column_count ( statement );
    G_debug (3, "ncols = %d", ncols);

    /* Try to get first row */    
    sqlite3_step ( statement );
    
    /* Count columns of known type */
    nkcols = 0;

    for (i = 0; i < ncols; i++) 
    {
        int  litetype, sqltype;
	
	get_column_info ( statement, i, &litetype, &sqltype );

	if ( sqltype == DB_SQL_TYPE_UNKNOWN ) continue;
	    
	nkcols++; /* known types */
    }
            
    G_debug (3, "nkcols = %d", nkcols);

    if ( c ) {
	c->kcols = (int *) G_malloc ( nkcols * sizeof(int) );
	c->nkcols = nkcols;
    }

    if (!(*table = db_alloc_table(nkcols))) {
	return DB_FAILED;
    }


    /* set the table name */
    /* TODO */
    db_set_table_name(*table, "");

    /* set the table description */
    db_set_table_description(*table, "");

    /* TODO */
    /*
    db_set_table_delete_priv_granted (*table);
    db_set_table_insert_priv_granted (*table);
    db_set_table_delete_priv_not_granted (*table);
    db_set_table_insert_priv_not_granted (*table);
    */

    nkcols = 0;
    for (i = 0; i < ncols; i++) 
    {
        const char *fname;
        dbColumn *column;
        int  litetype, sqltype, fsize, precision, scale;

	fname = sqlite3_column_name ( statement, i );

	get_column_info ( statement, i, &litetype, &sqltype );

	G_debug(2, "col: %s, nkcols %d, litetype : %d, sqltype %d", 
		    fname, nkcols, litetype, sqltype );

	if ( sqltype == DB_SQL_TYPE_UNKNOWN ) 
	{
	    /* Warn, ignore and continue */
	    G_warning ( _("SQLite driver: column '%s', SQLite type %d  is not supported"), 
			fname, litetype);
	    continue;
	}

	switch ( litetype) {
	case SQLITE_INTEGER:
	    fsize = 20;
	    break;

	case SQLITE_TEXT:
	    fsize = 255;
	    break;
	    
	case SQLITE_FLOAT:
	    fsize = 20;
	    break;
	    
	default:
	    fsize = 99999; /* sqlite doesn't care, it must be long enough to
                          satisfy tests in GRASS */
	}

	column = db_get_table_column(*table, nkcols);

	db_set_column_name(column, fname);
	db_set_column_length(column, fsize);
	db_set_column_host_type(column, litetype);
	db_set_column_sqltype(column, sqltype);

        /* TODO */
	precision = 0; 
        scale = 0;  
        /*
        db_set_column_precision (column, precision);
 	db_set_column_scale (column, scale);
 	*/

	/* TODO */
	db_set_column_null_allowed(column);
	db_set_column_has_undefined_default_value(column);
	db_unset_column_use_default_value(column);

	/* TODO */
	/*
        db_set_column_select_priv_granted (column);
        db_set_column_update_priv_granted (column);
        db_set_column_update_priv_not_granted (column); 
	*/

	if ( c ) {
	    c->kcols[nkcols] = i;
	}

	nkcols++;
    }

    sqlite3_reset ( statement );

    return DB_OK;
}

void get_column_info ( sqlite3_stmt *statement, int col, 
		int *litetype, int *sqltype )
{
    const char *decltype;
    
    decltype = sqlite3_column_decltype ( statement, col );
    if ( decltype ) 
    {
	G_debug ( 4, "column: %s, decltype = %s", sqlite3_column_name ( statement, col), decltype );
	*litetype = affinity_type ( decltype );
    }
    else
    {
	G_debug ( 4, "this is not a table column" );
	
	/* If there are no results it gives 0 */ 
	*litetype = sqlite3_column_type ( statement, col );
    }

    G_debug ( 3, "litetype = %d", *litetype );

    switch ( *litetype) {
	case SQLITE_INTEGER:
	    *sqltype = DB_SQL_TYPE_INTEGER;
	    break;

	case SQLITE_TEXT:
	    *sqltype = DB_SQL_TYPE_TEXT;
	    break;
	    
	case SQLITE_FLOAT:
	    *sqltype = DB_SQL_TYPE_DOUBLE_PRECISION;
	    break;
	    
	default:
	    *sqltype = DB_SQL_TYPE_UNKNOWN;
    }
}

/*  SQLite documentation:
*
*   The type affinity of a column is determined by the declared 
*   type of the column, according to the following rules:
*
*   1. If the datatype contains the string "INT" 
*      then it is assigned INTEGER affinity.
*
*   2. If the datatype of the column contains any of the strings 
*      "CHAR", "CLOB", or "TEXT" then that column has TEXT affinity. 
*      Notice that the type VARCHAR contains the string "CHAR" 
*      and is thus assigned TEXT affinity.
*
*   3. If the datatype for a column contains the string "BLOB" 
*      or if no datatype is specified then the column has affinity NONE.
*
*   4. Otherwise, the affinity is NUMERIC.
*/

int affinity_type ( const char *declared )
{
    char *lc;
    int aff = SQLITE_FLOAT;

    lc = strdup ( declared );
    G_tolcase ( lc );
    G_debug(4, "affinity_type: %s", lc);

    if ( strstr(lc,"int") )
    {
        aff = SQLITE_INTEGER;
    }
    else if ( strstr(lc,"char") || strstr(lc,"clob")
              || strstr(lc,"text") || strstr(lc,"date") )
    {
        aff = SQLITE_TEXT;
    }
    else if ( strstr(lc,"blob") )
    {
        aff = SQLITE_BLOB;
    }
  
    return aff;	
}

