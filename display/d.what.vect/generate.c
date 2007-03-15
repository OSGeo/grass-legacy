#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/dbmi.h>

/* Generate form in text format.
*  Pointer to resulting string is stored to 'form'. This string must be freed by application.
*
*  returns: -1 error 
*            0 success
*/
int generate(char *drvname, char *dbname, char *tblname, char *key, int keyval)
{
    int      col, ncols, ctype, sqltype, more; 
    char     buf[5000], *colname;
    dbString sql, str;
    dbDriver *driver;
    dbHandle handle;
    dbCursor cursor;
    dbTable  *table;
    dbColumn *column;
    dbValue  *value;

    G_debug ( 2, "generate(): drvname = '%s', dbname = '%s' tblname = '%s', key = '%s', keyval = %d",
	      drvname, dbname, tblname, key, keyval);
    
    db_init_string (&sql);
    db_init_string (&str);

    G_debug ( 2, "Open driver" );
    driver = db_start_driver(drvname);
    if (driver == NULL) { 
	G_warning("Cannot open driver '%s'", drvname); 
	return -1; 
    }
    G_debug ( 2, "Driver opened" );

    db_init_handle (&handle);
    db_set_handle (&handle, dbname, NULL);
    G_debug ( 2, "Open database" );
    if (db_open_database(driver, &handle) != DB_OK){
	G_warning("Cannot open database '%s' by driver '%s'", dbname, drvname);
	db_shutdown_driver(driver); 
	return -1;
    }
    G_debug ( 2, "Database opened" );

    /* TODO: test if table exist first, but this should be tested by application befor
    *        F_generate() is called, because it may be correct (connection defined in DB
    *        but table does not exist) */
    
    sprintf (buf, "select * from %s where %s = %d", tblname, key, keyval);
    G_debug ( 2, "%s", buf);
    db_set_string (&sql, buf);  
    if ( db_open_select_cursor(driver, &sql, &cursor, DB_SEQUENTIAL) != DB_OK) {
	G_warning("Cannot open select cursor: '%s' on database '%s' by driver '%s'", 
		  db_get_string(&sql), dbname, drvname);
        db_close_database(driver);
	db_shutdown_driver(driver); 
	return -1;
    }
    G_debug ( 2, "Select Cursor opened" );
	
    table = db_get_cursor_table (&cursor);

    if ( db_fetch (&cursor, DB_NEXT, &more ) != DB_OK ) {
	G_warning ("Cannot fetch next record"); 
	db_close_cursor(&cursor);
	db_close_database(driver);
	db_shutdown_driver(driver); 
	return -1;
    }

    if ( !more ) {
	G_warning ( "No database record" );
    } else {
	ncols = db_get_table_number_of_columns (table);

	/* Start form */
	for( col = 0; col < ncols; col++) {
	    column = db_get_table_column(table, col);
	    sqltype = db_get_column_sqltype (column);
	    ctype = db_sqltype_to_Ctype(sqltype);
	    value  = db_get_column_value(column);
	    db_convert_value_to_string( value, sqltype, &str);
	    colname = db_get_column_name (column);

	    G_debug ( 2, "%s: %s", colname, db_get_string (&str) );

	    printf ("%s : %s\n", colname, db_get_string(&str) );
	}
    }

    db_close_cursor(&cursor);
    db_close_database(driver);
    db_shutdown_driver(driver); 

    db_free_string (&sql);
    db_free_string (&str);

    return 0;
}


