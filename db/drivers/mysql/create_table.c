#include "dbmi.h"
#include "globals.h"
#include "proto.h"

int
db__driver_create_table (dbTable *table)
{
    dbString sql;
    int ret;
    
    G_debug (3, "db__driver_create_table()");

    db_init_string (&sql);

    db_table_to_sql ( table, &sql );

    G_debug (3, " SQL: %s", db_get_string(&sql) );

    ret = execute ( db_get_string(&sql), NULL);    

    if ( ret == DB_FAILED ) {
        sprintf(errMsg, "%sError in db_execute_immediate()", errMsg);
        report_error(errMsg);
	return DB_FAILED;
    }

    /* TODO: Grant privileges */
    
    return DB_OK;
}
