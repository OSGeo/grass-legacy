#include <stdlib.h>
#include <string.h>
#include "dbmi.h"
#include "macros.h"

static int cmp ( const void *pa, const void *pb)
{
    int *p1 = (int *) pa;
    int *p2 = (int *) pb;

    if( *p1 < *p2 ) return -1;
    if( *p1 > *p2 ) return 1;
    return 0;
}

/* Copy table, used by various db_copy_table* 
 
   Parameters: 
       where: WHERE SQL condition (without where key word) or NULL
       select: full select statement
       selcol: name of column used to select records by values in ivals or NULL
       ivals: pointer to array of integer values or NULL
       nvals: number of values in ivals

   Use either 'where' or 'select' or 'selcol'+'ivals'+'nvals' but never more than one
       
*/
/* Warning, driver opened as second must be closed as first, otherwise it hangs, not sure why */
int
db__copy_table ( char *from_drvname, char *from_dbname, char *from_tblname,
                char *to_drvname, char *to_dbname, char *to_tblname, 
		char *where, char *select,
		char *selcol, int *ivals, int nvals)
{
    int col, ncols, sqltype, ctype, more, selcol_found;
    char buf[1000]; 
    int *ivalues;
    dbHandle from_handle, to_handle;
    dbString tblname, sql;
    dbString value_string;
    dbTable *table;
    dbCursor cursor;
    dbColumn *column;
    dbValue *value;
    char *colname;
    dbDriver *from_driver, *to_driver;

    G_debug ( 3, "db_copy_table():\n  from driver = %s, db = %s, table = %s\n"
	         "  to driver = %s, db = %s, table = %s, where = %s, select = %s", 
		 from_drvname, from_dbname, from_tblname, to_drvname, to_dbname, to_tblname, where, select);

    db_init_handle (&from_handle);
    db_init_handle (&to_handle);
    db_init_string (&tblname);
    db_init_string (&sql);
    db_init_string (&value_string);

    /* Make a copy of input values and sort it */
    if ( ivals ) {
        ivalues = (int*) G_malloc ( nvals * sizeof(int) );
	memcpy ( ivalues, ivals, nvals * sizeof(int) );
	qsort( (void *)ivalues, nvals, sizeof(int), cmp);
    }

    /* Open input driver and database */
    from_driver = db_start_driver(from_drvname);
    if ( from_driver == NULL) {
	G_warning ( "Cannot open driver '%s'", from_drvname);
	return DB_FAILED;
    }
    db_set_handle (&from_handle, from_dbname, NULL);
    if (db_open_database(from_driver, &from_handle) != DB_OK) {
	G_warning ( "Cannot open database '%s'", from_dbname);
	db_close_database_shutdown_driver(from_driver);
	return DB_FAILED;
    }
    
    /* Open output driver and database */
    to_driver = db_start_driver(to_drvname);
    if ( to_driver == NULL) {
	G_warning ( "Cannot open driver '%s'", to_drvname);
	db_close_database_shutdown_driver(from_driver);
	return DB_FAILED;
    }
    db_set_handle (&to_handle, to_dbname, NULL);
    if (db_open_database(to_driver, &to_handle) != DB_OK) {
	G_warning ( "Cannot open database '%s'", to_dbname);
	db_close_database_shutdown_driver(to_driver);
	db_close_database_shutdown_driver(from_driver);
	return DB_FAILED;
    }

    db_begin_transaction ( to_driver );

    /* Create new table */
    /* TODO test if the tables exist */
    if ( select ) {
        db_set_string ( &sql, select );
    } else { 
	db_set_string ( &sql, "select * from ");
	db_append_string ( &sql, from_tblname);
	if ( where ) {
	    db_append_string ( &sql, " where ");
	    db_append_string ( &sql, where);
	}
    }
    
    G_debug ( 3, db_get_string(&sql) );
    if (db_open_select_cursor(from_driver, &sql, &cursor, DB_SEQUENTIAL) != DB_OK) {
	G_warning ( "Cannot open select cursor: '%s'", db_get_string(&sql) );
	db_close_database_shutdown_driver(to_driver);
	db_close_database_shutdown_driver(from_driver);
	return DB_FAILED;
    }
    G_debug ( 3, "Select cursor opened" );
   
    table = db_get_cursor_table (&cursor);
    ncols = db_get_table_number_of_columns(table);
    G_debug ( 3, "ncols = %d", ncols );
    sprintf ( buf, "create table %s ( ", to_tblname );
    db_set_string ( &sql, buf);  
    selcol_found = 0;
    for ( col = 0; col < ncols; col++ ) {
        column = db_get_table_column (table, col);
	colname = db_get_column_name (column);
	sqltype = db_get_column_sqltype (column);
	ctype = db_sqltype_to_Ctype(sqltype);
	G_debug ( 3, "%s (%s)", colname, db_sqltype_name(sqltype) );

	if ( selcol && G_strcasecmp ( colname, selcol) == 0 ) {
	    if ( ctype != DB_C_TYPE_INT )
		G_fatal_error ("Column '%s' is not integer", colname);
	    selcol_found = 1;
	}

	if ( col > 0 ) db_append_string ( &sql, ", " );
	db_append_string ( &sql, colname );
	db_append_string ( &sql, " " );
	/* Note: I found on Web:
	*  These are the ANSI data types: BIT, CHARACTER, DATE, DECIMAL, DOUBLE PRECISION, FLOAT, 
	*  INTEGER, INTERVAL, NUMERIC, REAL, SMALLINT, TIMESTAMP, TIME, VARBIT, VARCHAR, CHAR
	*  ...
	*  Thus, the only data types you can use with the assurance that they will 
	*  work everywhere are as follows:
	*  DOUBLE PRECISION, FLOAT, INTEGER, NUMERIC, REAL, SMALLINT, VARCHAR, CHAR */
	switch ( ctype ) {
	    case DB_C_TYPE_STRING:
		sprintf (buf, "varchar(%d)", db_get_column_length (column) );
		db_append_string ( &sql, buf);
                break;
	    case DB_C_TYPE_INT:
		db_append_string ( &sql, "integer");
		break;
	    case DB_C_TYPE_DOUBLE:
		db_append_string ( &sql, "double precision");
		break;
	    case DB_C_TYPE_DATETIME:
		switch ( sqltype ) {
                    case DB_SQL_TYPE_DATE:
		        db_append_string ( &sql, "date");
			break;
                    case DB_SQL_TYPE_TIME:
		        db_append_string ( &sql, "time");
			break;
		    default:
		        db_append_string ( &sql, "datetime");
		}
		break;
 	    default:
                G_warning ( "Unknown column type (%s)", colname);
	        db_close_cursor(&cursor);
		db_close_database_shutdown_driver(to_driver);
		db_close_database_shutdown_driver(from_driver);
		return DB_FAILED;
	}
    }
    db_append_string ( &sql, ")" );
    G_debug ( 3, db_get_string(&sql) );
 
    if ( selcol && !selcol_found) 
	G_fatal_error ("Column '%s' not found", selcol);
    
    if (db_execute_immediate (to_driver, &sql) != DB_OK ) {
	G_warning ( "Cannot create new table: '%s'", db_get_string(&sql) );
	db_close_cursor(&cursor);
	db_close_database_shutdown_driver(to_driver);
	db_close_database_shutdown_driver(from_driver);
	return DB_FAILED;
    }

    /* Copy all rows */
    while ( 1 ) {
	int select;
	
	if ( db_fetch (&cursor, DB_NEXT, &more ) != DB_OK ) { 
	    G_warning ( "Cannot fetch row" );
	    db_close_cursor(&cursor);
	    db_close_database_shutdown_driver(to_driver);
	    db_close_database_shutdown_driver(from_driver);
	    return DB_FAILED;
	}
	if (!more) break;

	sprintf ( buf, "insert into %s values ( ", to_tblname );
	db_set_string ( &sql, buf);  
	select = 1;
	for ( col = 0; col < ncols; col++ ) {
	    column = db_get_table_column (table, col);
	    colname = db_get_column_name (column);
	    sqltype = db_get_column_sqltype (column);
	    ctype = db_sqltype_to_Ctype(sqltype);
	    value  = db_get_column_value(column);

	    if ( selcol && G_strcasecmp ( colname, selcol) == 0 ) {
		if ( db_test_value_isnull(value) ) continue;
		if ( !bsearch(&(value->i), ivalues, nvals, sizeof(int), cmp) ) {
		    select = 0;
		    break;
		}
	    }
	    if ( col > 0 ) db_append_string ( &sql, ", " );
	    db_convert_value_to_string( value, sqltype, &value_string); 
	    switch ( ctype ) {
		case DB_C_TYPE_STRING:
		case DB_C_TYPE_DATETIME:
		    if ( db_test_value_isnull(value) ) {
		        db_append_string ( &sql, "null" );
		    } else {
			db_double_quote_string ( &value_string );
			sprintf (buf, "'%s'", db_get_string(&value_string) );
			db_append_string ( &sql, buf);
		    }
		    break;
		case DB_C_TYPE_INT:
		case DB_C_TYPE_DOUBLE:
		    if ( db_test_value_isnull(value) ) {
		        db_append_string ( &sql, "null" );
		    } else {
		        db_append_string ( &sql, db_get_string(&value_string) );
		    }
		    break;
		default:
		    G_warning ( "Unknown column type (%s)", colname);
		    db_close_cursor(&cursor);
		    db_close_database_shutdown_driver(to_driver);
		    db_close_database_shutdown_driver(from_driver);
		    return DB_FAILED;
	    }
	}
	if ( !select ) continue;
	db_append_string ( &sql, ")" );
	G_debug ( 3, db_get_string(&sql) );
	if (db_execute_immediate (to_driver, &sql) != DB_OK ) {
	    G_warning ( "Cannot insert new record: '%s'", db_get_string(&sql) );
	    db_close_cursor(&cursor);
	    db_close_database_shutdown_driver(to_driver);
	    db_close_database_shutdown_driver(from_driver);
	    return DB_FAILED;
	}
    }
    if ( selcol ) free (ivalues);
    G_debug ( 3, "Table copy OK" );

    db_close_cursor(&cursor);
    db_commit_transaction ( to_driver );
    db_close_database_shutdown_driver(to_driver);
    db_close_database_shutdown_driver(from_driver);

    return DB_OK;
}

/*!
 \fn int db_copy_table ()
 \brief Copy a table
 \return 
 \param
*/
int
db_copy_table ( char *from_drvname, char *from_dbname, char *from_tblname,
                char *to_drvname, char *to_dbname, char *to_tblname )
{
    return db__copy_table ( from_drvname, from_dbname, from_tblname, 
	                    to_drvname, to_dbname, to_tblname,
			    NULL, NULL,
	     		    NULL, NULL, 0 );
}

/*!
 \fn int db_copy_table_where ()
 \brief Copy a table
 \return 
 \param where WHERE SQL condition (without where key word) or NULL
*/
int
db_copy_table_where ( char *from_drvname, char *from_dbname, char *from_tblname,
                char *to_drvname, char *to_dbname, char *to_tblname, char *where )
{
    return db__copy_table ( from_drvname, from_dbname, from_tblname, 
	                    to_drvname, to_dbname, to_tblname,
			    where, NULL,
	     		    NULL, NULL, 0 );
}

/*!
 \fn int db_copy_table_select ()
 \brief Copy a table
 \return 
 \param select is full select statement or NULL
*/
int
db_copy_table_select ( char *from_drvname, char *from_dbname, char *from_tblname,
                char *to_drvname, char *to_dbname, char *to_tblname, char *select )
{
    return db__copy_table ( from_drvname, from_dbname, from_tblname, 
	                    to_drvname, to_dbname, to_tblname,
			    NULL, select,
	     		    NULL, NULL, 0 );
}

/*!
 \fn int db_copy_table_by_ints ()
 \brief Copy a table, but only records where value of column 'selcol'
        is in 'ivals' 
 \return 
 \param selcol name of column used to select records by values in ivals or NULL
 \param ivals pointer to array of integer values or NULL
 \param nvals number of values in ivals
*/
int
db_copy_table_by_ints ( char *from_drvname, char *from_dbname, char *from_tblname,
                char *to_drvname, char *to_dbname, char *to_tblname, 
		char *selcol, int *ivals, int nvals )
{
    return db__copy_table ( from_drvname, from_dbname, from_tblname, 
	                    to_drvname, to_dbname, to_tblname,
			    NULL, NULL,
	     		    selcol, ivals, nvals );
}
