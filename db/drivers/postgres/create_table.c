#include "dbmi.h"
#include "globals.h"
#include "proto.h"

int
db__driver_create_table (dbTable *table)
{
    PGresult *res;
    dbString sql;
    dbConnection connection;
    
    G_debug (3, "db__driver_create_table()");

    db_init_string (&sql);

    db_table_to_sql ( table, &sql );

    init_error();

    G_debug (3, " SQL: %s", db_get_string(&sql) );
    
    res = PQexec(pg_conn, db_get_string(&sql) ); 

    if (!res || PQresultStatus(res) != PGRES_COMMAND_OK) {
        append_error( "Cannot create table:\n");
	append_error( db_get_string(&sql) );
	append_error( "\n" );
	append_error(PQerrorMessage(pg_conn));
	report_error();
	PQclear(res);
	db_free_string ( &sql);
	return DB_FAILED;
    }

    PQclear(res);

    /* Grant privileges */
    db_get_connection(&connection);

    db_set_string ( &sql, "grant select on " );
    db_append_string ( &sql, db_get_table_name ( table ) );
    db_append_string ( &sql, " to public" );

    if ( connection.group ) {
        db_append_string ( &sql, ", group " );
	db_append_string ( &sql, connection.group );
    }
    
    G_debug (3, " SQL: %s", db_get_string(&sql) );

    res = PQexec(pg_conn, db_get_string(&sql) ); 

    if (!res || PQresultStatus(res) != PGRES_COMMAND_OK) {
        append_error( "Cannot grant select on table:\n");
	append_error( db_get_string(&sql) );
	append_error( "\n" );
	append_error(PQerrorMessage(pg_conn));
	report_error();
	PQclear(res);
	db_free_string ( &sql);
	return DB_FAILED;
    }
    
    PQclear(res);
    db_free_string ( &sql);
    
    return DB_OK;
}
