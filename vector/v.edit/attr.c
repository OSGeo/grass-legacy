#include "global.h"
#include <grass/dbmi.h>

int attr_new(struct Map_info *Map, int field, int cat, const char *vals)
{
    int ret;
    dbValue value;
    dbString sql;
    dbDriver *driver;
    struct field_info *Fi;
    char buf[1024];

    db_init_string (&sql);

    G_debug (2, "new_record() field = %d cat = %d", field, cat );
    
    Fi = Vect_get_field( Map, field );
    if ( Fi == NULL ) { 
	G_warning("Database table for this layer is not defined");
	return 0;
    }

    /* Note: some drivers (dbf) writes date when db is closed so it is better open
     * and close database for each record, so that data may not be lost later */

    /* First check if already exists */
    driver = db_start_driver_open_database ( Fi->driver, Fi->database );
    if ( driver == NULL ) {
	G_warning("Cannot open database %s by driver %s", Fi->database, Fi->driver );
	return 0;
    }
    ret = db_select_value ( driver, Fi->table, Fi->key, cat, Fi->key, &value );
    if ( ret < 0 ) {
	db_close_database_shutdown_driver ( driver );
	G_warning(buf, "Cannot select record from table %s", Fi->table );
	return 0;
    }
    else if ( ret == 0 ) { /* insert new record */
	sprintf ( buf, "insert into %s (%s) values (%d)", Fi->table, Fi->key, cat );
	db_set_string ( &sql, buf);
	G_debug ( 2, db_get_string ( &sql ) );
	ret = db_execute_immediate (driver, &sql);
	if ( ret != DB_OK ) {	
	    db_close_database_shutdown_driver ( driver );
	    G_warning("Cannot insert new record: %s", db_get_string(&sql) );
	    return 0;
	}
    } 
    else { /* record already existed */
	G_warning("Category %d already exists in database\n");
    }
    
    if(vals != NULL) {
	sprintf ( buf, "update %s set %s where %s=%d", 
		  Fi->table, vals, Fi->key, cat );
	db_set_string ( &sql, buf);
	G_debug ( 2, db_get_string ( &sql ) );
	ret = db_execute_immediate (driver, &sql);
	if ( ret != DB_OK ) {	
	    db_close_database_shutdown_driver ( driver );
	    G_warning("Cannot update record: %s", db_get_string(&sql) );
	    return 0;
	}	
    }
    db_close_database_shutdown_driver ( driver );
    return 1;
    
}

int attr_edit(struct Map_info *Map, int field, int cat, const char *vals)
{
    int ret;
    dbValue value;
    dbString sql;
    dbDriver *driver;
    struct field_info *Fi;
    char buf[1024];

    db_init_string (&sql);

    G_debug (2, "new_record() field = %d cat = %d", field, cat );
    
    Fi = Vect_get_field( Map, field );
    if ( Fi == NULL ) { 
	G_warning("Database table for this layer is not defined");
	return 0;
    }

    /* Note: some drivers (dbf) writes date when db is closed so it is better open
     * and close database for each record, so that data may not be lost later */

    /* First check if already exists */
    driver = db_start_driver_open_database ( Fi->driver, Fi->database );
    if ( driver == NULL ) {
	G_warning("Cannot open database %s by driver %s", Fi->database, Fi->driver );
	return 0;
    }
    ret = db_select_value ( driver, Fi->table, Fi->key, cat, Fi->key, &value );
    if ( ret < 0 ) {
	db_close_database_shutdown_driver ( driver );
	G_warning(buf, "Cannot select record from table %s", Fi->table );
	return 0;
    }
    else if ( ret == 0 ) { /* insert new record */
	sprintf ( buf, "update %s set %s where %s=%d", 
		  Fi->table, vals, Fi->key, cat );
	db_set_string ( &sql, buf);
	G_debug ( 2, db_get_string ( &sql ) );
	ret = db_execute_immediate (driver, &sql);
	if ( ret != DB_OK ) {	
	    db_close_database_shutdown_driver ( driver );
	    G_warning("Cannot update record: %s", db_get_string(&sql) );
	    return 0;
	}
    } 
    else { /* record already existed */
	db_close_database_shutdown_driver ( driver );
	G_warning("Cannot insert attibutes, cat %d does not exist", cat);
	return 0;
    }
    db_close_database_shutdown_driver ( driver );
    return 1;
}

int attr_del(struct Map_info *Map, int field, int cat)
{
    int ret;
    dbValue value;
    dbString sql;
    dbDriver *driver;
    struct field_info *Fi;
    char buf[1024];

    db_init_string (&sql);

    G_debug (2, "new_record() field = %d cat = %d", field, cat );
    
    Fi = Vect_get_field( Map, field );
    if ( Fi == NULL ) { 
	G_warning("Database table for this layer is not defined");
	return 0;
    }

    /* Note: some drivers (dbf) writes date when db is closed so it is better open
     * and close database for each record, so that data may not be lost later */

    /* First check if already exists */
    driver = db_start_driver_open_database ( Fi->driver, Fi->database );
    if ( driver == NULL ) {
	G_warning("Cannot open database %s by driver %s", Fi->database, Fi->driver );
	return 0;
    }
    ret = db_select_value ( driver, Fi->table, Fi->key, cat, Fi->key, &value );
    if ( ret < 0 ) {
	db_close_database_shutdown_driver ( driver );
	G_warning(buf, "Cannot select record from table %s", Fi->table );
	return 0;
    }
    else if ( ret == 0 ) { /* insert new record */
	sprintf ( buf, "delete from %s where %s=%d", Fi->table, Fi->key, cat );
	db_set_string ( &sql, buf);
	G_debug ( 2, db_get_string ( &sql ) );
	ret = db_execute_immediate (driver, &sql);
	if ( ret != DB_OK ) {	
	    db_close_database_shutdown_driver ( driver );
	    G_warning("Cannot update record: %s", db_get_string(&sql) );
	    return 0;
	}
    } 
    else { /* record already existed */
	db_close_database_shutdown_driver ( driver );
	G_warning("Category %d does not exist.", cat);
	return 0;
    }
    db_close_database_shutdown_driver ( driver );
    return 1;
}
