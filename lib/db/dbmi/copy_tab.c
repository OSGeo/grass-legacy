#include "dbmi.h"
#include "macros.h"

dbDriver *from_driver, *to_driver;

/* Warning, driver opened as second must be closed as first, otherwise it hangs, not sure why */
void closedb ( void ) 
{
    db_close_database(to_driver);
    db_shutdown_driver(to_driver);
    db_close_database(from_driver);
    db_shutdown_driver(from_driver);
}

/*!
 \fn int db_copy_table ()
 \brief 
 \return 
 \param 
*/
int
db_copy_table ( char *from_drvname, char *from_dbname, char *from_tblname,
                char *to_drvname, char *to_dbname, char *to_tblname )
{
    int col, ncols, sqltype, ctype, more;
    char buf[1000]; 
    dbHandle from_handle, to_handle;
    dbString tblname, sql;
    dbString value_string;
    dbTable *table;
    dbCursor cursor;
    dbColumn *column;
    dbValue *value;
    char *colname;


    G_debug ( 3, "db_copy_table():\n  from driver = %s, db = %s, table = %s\n"
	         "  to driver = %s, db = %s, table = %s", 
		 from_drvname, from_dbname, from_tblname, to_drvname, to_dbname, to_tblname);

    db_init_handle (&from_handle);
    db_init_handle (&to_handle);
    db_init_string (&tblname);
    db_init_string (&sql);
    db_init_string (&value_string);

    /* Open input driver and database */
    from_driver = db_start_driver(from_drvname);
    if ( from_driver == NULL) {
	G_warning ( "Cannot open driver '%s'", from_drvname);
	return DB_FAILED;
    }
    db_set_handle (&from_handle, from_dbname, NULL);
    if (db_open_database(from_driver, &from_handle) != DB_OK) {
	G_warning ( "Cannot open database '%s'", from_dbname);
	db_shutdown_driver(from_driver);
	return DB_FAILED;
    }
    
    /* Open output driver and database */
    to_driver = db_start_driver(to_drvname);
    if ( to_driver == NULL) {
	G_warning ( "Cannot open driver '%s'", to_drvname);
	db_close_database(from_driver);
	db_shutdown_driver(from_driver);
	return DB_FAILED;
    }
    db_set_handle (&to_handle, to_dbname, NULL);
    if (db_open_database(to_driver, &to_handle) != DB_OK) {
	G_warning ( "Cannot open database '%s'", to_dbname);
	db_shutdown_driver(to_driver);
	db_close_database(from_driver);
	db_shutdown_driver(from_driver);
	return DB_FAILED;
    }

    /* Create new table */
    /* TODO test if the tables exist */
    db_set_string ( &sql, "select * from ");
    db_append_string ( &sql, from_tblname);
    G_debug ( 3, db_get_string(&sql) );
    if (db_open_select_cursor(from_driver, &sql, &cursor, DB_SEQUENTIAL) != DB_OK) {
	G_warning ( "Cannot open select cursor: '%s'", db_get_string(&sql) );
	closedb();
	return DB_FAILED;
    }
    G_debug ( 3, "Select cursor opened" );
   
    table = db_get_cursor_table (&cursor);
    ncols = db_get_table_number_of_columns(table);
    G_debug ( 3, "ncols = %d", ncols );
    sprintf ( buf, "create table %s ( ", to_tblname );
    db_set_string ( &sql, buf);  
    for ( col = 0; col < ncols; col++ ) {
        column = db_get_table_column (table, col);
	colname = db_get_column_name (column);
	sqltype = db_get_column_sqltype (column);
	ctype = db_sqltype_to_Ctype(sqltype);
	G_debug ( 3, "%s (%s)", colname, db_sqltype_name(sqltype) );
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
		closedb();
		return DB_FAILED;
	}
    }
    db_append_string ( &sql, ")" );
    G_debug ( 3, db_get_string(&sql) );
    if (db_execute_immediate (to_driver, &sql) != DB_OK ) {
	G_warning ( "Cannot create new table: '%s'", db_get_string(&sql) );
	db_close_cursor(&cursor);
	closedb();
	return DB_FAILED;
    }

    /* Copy all rows */
    while ( 1 ) {
	if ( db_fetch (&cursor, DB_NEXT, &more ) != DB_OK ) { 
	    G_warning ( "Cannot fetch row" );
	    db_close_cursor(&cursor);
	    closedb();
	    return DB_FAILED;
	}
	if (!more) break;

	sprintf ( buf, "insert into %s values ( ", to_tblname );
	db_set_string ( &sql, buf);  
	for ( col = 0; col < ncols; col++ ) {
	    column = db_get_table_column (table, col);
	    colname = db_get_column_name (column);
	    sqltype = db_get_column_sqltype (column);
	    ctype = db_sqltype_to_Ctype(sqltype);
	    value  = db_get_column_value(column);
	    if ( col > 0 ) db_append_string ( &sql, ", " );
	    db_convert_value_to_string( value, sqltype, &value_string); 
	    switch ( ctype ) {
		case DB_C_TYPE_STRING:
		case DB_C_TYPE_DATETIME:
		    db_double_quote_string ( &value_string );
		    sprintf (buf, "'%s'", db_get_string(&value_string) );
		    db_append_string ( &sql, buf);
		    break;
		case DB_C_TYPE_INT:
		case DB_C_TYPE_DOUBLE:
		    db_append_string ( &sql, db_get_string(&value_string) );
		    break;
		default:
		    G_warning ( "Unknown column type (%s)", colname);
		    db_close_cursor(&cursor);
		    closedb();
		    return DB_FAILED;
	    }
	}
	db_append_string ( &sql, ")" );
	G_debug ( 3, db_get_string(&sql) );
	if (db_execute_immediate (to_driver, &sql) != DB_OK ) {
	    G_warning ( "Cannot insert new record: '%s'", db_get_string(&sql) );
	    db_close_cursor(&cursor);
	    closedb();
	    return DB_FAILED;
	}
    }
    G_debug ( 3, "Table copy OK" );

    db_close_cursor(&cursor);
    closedb();

    return DB_OK;
}

