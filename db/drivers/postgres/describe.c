#include <dbmi.h>
#include <datetime.h>
#include "globals.h"
#include "proto.h"

int db_driver_describe_table(table_name, table)
     dbString *table_name;
     dbTable **table;
{
    dbString sql;
    PGresult *res;

    db_init_string ( &sql );

    db_set_string( &sql,"select * from ");
    db_append_string ( &sql, db_get_string(table_name) );
    db_append_string( &sql, " where oid < 0");

    res = PQexec(pg_conn, db_get_string(&sql));

    if (!res || PQresultStatus(res) != PGRES_TUPLES_OK) {
	append_error ( db_get_string(&sql) );
	append_error ( "\n" );
	append_error ( PQerrorMessage(pg_conn) );
	report_error();
	PQclear(res);
	return DB_FAILED;
    }

    if ( describe_table( res, table, NULL) == DB_FAILED ) {
	append_error("Cannot describe table\n");
	report_error();
	PQclear(res);
	return DB_FAILED;
    }

    PQclear(res);

    return DB_OK;
}

/* describe table, if c is not NULL cur->cols and cur->ncols is also set */
int describe_table( PGresult *res, dbTable **table, cursor *c)
{
    int i, ncols, kcols;
    int pgtype;
    char *fname;
    int sqltype, fsize, precision, scale;
    dbColumn *column;

    G_debug (3, "describe_table()");

    ncols = PQnfields(res);

    
    /* Count columns of known type */
    kcols = 0;
    for (i = 0; i < ncols; i++) {
	get_column_info (res, i, &pgtype, &sqltype, &fsize );

	if ( sqltype == DB_SQL_TYPE_UNKNOWN ) continue;
	    
	kcols++; /* known types */
    }

    G_debug (3, "kcols = %d", kcols);

    if (!(*table = db_alloc_table(kcols))) {
	return DB_FAILED;
    }

    if ( c ) {
	c->ncols = kcols;
	c->cols = (int *) G_malloc ( kcols * sizeof(int) );
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

    kcols = 0;
    for (i = 0; i < ncols; i++) {
	fname = PQfname(res, i);
	get_column_info (res, i, &pgtype, &sqltype, &fsize );
	G_debug(3, "col: %s, kcols %d, pgtype : %d, sqltype %d, fsize : %d", 
		    fname, kcols, pgtype, sqltype, fsize);

	if ( sqltype == DB_SQL_TYPE_UNKNOWN ) {
	    /* Warn, ignore and continue */
	    G_warning ( "pg driver: column '%s', type %d  is not supported", fname, pgtype);
	    continue;
	}
	
	column = db_get_table_column(*table, kcols);

	db_set_column_name(column, fname);
	db_set_column_length(column, fsize);
	db_set_column_host_type(column, pgtype);
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
	    c->cols[kcols] = i;
	}

	kcols++;
    }

    return DB_OK;
}

int get_column_info ( PGresult *res, int col, int *pgtype, int *sqltype, int *size)
{
    *pgtype = get_pg_type ( (int) PQftype(res, col) );

    /* Convert internal type to PG_TYPE_* */
    
    /* TODO: we should load field names from pg_type table instead of using copy of #defines */
    switch ( *pgtype) {
	case PG_TYPE_INT2:
	case PG_TYPE_INT4:
	case PG_TYPE_SERIAL:
	case PG_TYPE_OID:
	    *sqltype = DB_SQL_TYPE_INTEGER;
	    *size = PQfsize(res, col);
	    break;

	case PG_TYPE_CHAR:
	case PG_TYPE_VARCHAR:
	    *sqltype = DB_SQL_TYPE_CHARACTER;
	    *size = PQfmod(res, col) - 4; /* Looks strange but works, something better? */
	    break;
	    
	/*
	case PG_TYPE_TEXT: 
	    *sqltype = DB_SQL_TYPE_CHARACTER;
	    *size = -1; 
	    break;
	*/

	case PG_TYPE_REAL:
	case PG_TYPE_FLOAT8:
	case PG_TYPE_NUMERIC:
	    *sqltype = DB_SQL_TYPE_DOUBLE_PRECISION;
	    *size = PQfsize(res, col);
	    break;
	    
	/* I'm not sure if text length is correct for size */
	case PG_TYPE_DATE:
	    *sqltype = DB_SQL_TYPE_DATE;
	    *size = 10; /* YYYY-MM-DD */
	    break;

	case PG_TYPE_TIME:
	    *sqltype = DB_SQL_TYPE_TIME;
	    *size = 8; /* HH:MM:SS */
	    break;

	case PG_TYPE_TIMESTAMP: 
	    *sqltype = DB_SQL_TYPE_TIMESTAMP;
	    *size = 22; /* YYYY-MM-DD HH:MM:SS+TZ */
	    break;

	default:
	    *sqltype = DB_SQL_TYPE_UNKNOWN;
	    *size = 0;
    }

    return 0;
}

/* for given internal postgres type returns one of PG_TYPE_* defined in GRASS */
int get_pg_type (int pgtype )
{
    int i;

    for ( i = 0; i < pg_ntypes; i++ ){
	if ( pg_types[i][0] == pgtype )
	    return pg_types[i][1];
    }
    return PG_TYPE_UNKNOWN;
}
