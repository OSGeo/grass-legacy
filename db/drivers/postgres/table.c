/*****************************************************************************
*
* MODULE:       PostgreSQL driver forked from DBF driver by Radim Blazek 
*   	    	
* AUTHOR(S):    Alex Shevlakov
*
* PURPOSE:      Simple driver for reading and writing data     
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dbmi.h>
#include "globals.h"
#include "proto.h"

/* add table to database */
int add_table(char *table)
{
    if (db.atables == db.ntables) {
	db.atables += 15;
	db.tables = (TABLE *) realloc(db.tables, db.atables * sizeof(TABLE));
    }


    strcpy(db.tables[db.ntables].name, table);

    db.tables[db.ntables].alive = TRUE;
    db.tables[db.ntables].described = FALSE;
    db.tables[db.ntables].loaded = FALSE;
    db.tables[db.ntables].updated = FALSE;
    db.tables[db.ntables].cols = NULL;
    db.tables[db.ntables].rows = NULL;
    db.tables[db.ntables].acols = 0;
    db.tables[db.ntables].ncols = 0;
    db.tables[db.ntables].arows = 0;
    db.tables[db.ntables].nrows = 0;

    db.ntables++;

    return DB_OK;
}

/* strip table of all data and attribs */
int make_table_brand_new(int i)
{

    free_table(i);

    db.tables[i].alive = TRUE;
    db.tables[i].described = FALSE;
    db.tables[i].loaded = FALSE;
    db.tables[i].updated = FALSE;
    db.tables[i].cols = NULL;
    db.tables[i].rows = NULL;
    db.tables[i].acols = 0;
    db.tables[i].ncols = 0;
    db.tables[i].arows = 0;
    db.tables[i].nrows = 0;

    return DB_OK;
}

/* returns table index or -1 */
int find_table(char *table)
{
    int i;

    for (i = 0; i < db.ntables; i++) {
	if ( G_strcasecmp (db.tables[i].name, table) == 0)
	    return (i);
    }

    return (-1);
}

/*
int
load_table_head( int t)
{
    db.tables[t].described = TRUE;
	
    return DB_OK;
}
*/

int load_table(int t, char *stmt)
{
    PGresult *res;
    Oid dtype;
    char *fname;
    int fsize=0;
    int type;
    int nflds = 0;
    int nrws = 0;
    char *buf;
    char stmtbuf[NAMELEN + 16];
    ROW *rows;
    VALUE *val;
    int i, j;
    int header_only = 0;
    char **tokens;
    int ntokens;

    G_debug(3, "load_table()");
        
    if (db.tables[t].loaded == TRUE)
	return DB_OK;
    G_debug(3, "load_table() - not loaded");

   if (stmt == NULL) {
	memset(stmtbuf, '\0', sizeof(stmtbuf));
	sprintf(stmtbuf, "select * from %s", db.tables[t].name);
	stmt = stmtbuf;
	header_only = 1;
        G_debug(3, "load_table() - only header");
 	
    }

    res = PQexec(pg_conn, stmt);

    if (!res || PQresultStatus(res) != PGRES_TUPLES_OK) {
        
	snprintf(errMsg, sizeof(errMsg), "Error: select Postgres:%s\n",
		 PQerrorMessage(pg_conn));
	PQclear(res);
	PQfinish(pg_conn);
	return DB_FAILED;
    }
    nflds = PQnfields(res);
    nrws = PQntuples(res);

    for (i = 0; i < nflds; i++) {
	dtype = PQftype(res, i);
	fname = PQfname(res, i);

	switch (dtype) {
	case INT8OID:
	case INT2OID:
	case INT4OID:
	case OIDOID:
	case POSTGISUNKNOWNOID:
	    type = PG_INT;
	    fsize = PQfsize(res, i);
	    break;
	case CHAROID:
	case BPCHAROID:
	case VARCHAROID:
	case TEXTOID:
	case POSTGISPOINTOID:
	    type = PG_CHAR;
	    fsize = PQfmod(res, i) - 4; /* Looks strange but works, something better? */
	    break;
	case FLOAT4OID:
	case FLOAT8OID:
	    type = PG_DOUBLE;
	    fsize = PQfsize(res, i);
	    break;
	case DATEOID:
	    type = PG_DATE;
	    fsize = 10; /* YYYY-MM-DD */
	    break;
	case TIMEOID:
	    type = PG_TIME;
	    fsize = 8; /* HH-MM-SS */
	    break;
	default:
	    if(!header_only) {
	    snprintf(errMsg, sizeof(errMsg),
		     "Field %s can not be selected for query output: type %d not supported yet\n",
		     fname, dtype);
	    return DB_FAILED;
	    }
	    
	    type = PG_UNKNOWN;
	    break;

	}

	G_debug(3, "col: %s type : %d width :%d", fname, type, fsize);
	add_column(t, type, fname, fsize);
	
	G_debug(3, "load_table() - number of cols is %d", db.tables[t].ncols);
    }

    if (!header_only) {

	rows = db.tables[t].rows;
	rows = (ROW *) malloc(nrws * sizeof(ROW));
	db.tables[t].arows = nrws;

	for (i = 0; i < nrws; i++) {
	    rows[i].alive = TRUE;
	    rows[i].values = (VALUE *) calloc(nflds, sizeof(VALUE));

	    for (j = 0; j < nflds; j++) {
		val = &(rows[i].values[j]);
		switch (db.tables[t].cols[j].type) {
		case PG_INT:
		    val->i = atoi(PQgetvalue(res, i, j));
		    break;
		case PG_CHAR:
		    buf = (char *) PQgetvalue(res, i, j);
		    save_string(val, buf);
		    break;
		case PG_DOUBLE:
		    val->d = atof(PQgetvalue(res, i, j));
		    break;
		case PG_DATE:
		    val->t.year  = 0;
		    val->t.month = 0;
		    val->t.day   = 0;
		    val->t.hour   = 0;
		    val->t.minute = 0;
		    val->t.seconds = 0;

		    if ( ! PQgetisnull(res, i, j) ) {
			buf = (char *) PQgetvalue(res, i, j);
			tokens = G_tokenize (buf, "-"); /* depends on LOCALE ? */
			ntokens = G_number_of_tokens (tokens);
			if ( ntokens >= 3 ) {
			    val->t.year  = atoi(tokens[0]);
			    val->t.month = atoi(tokens[1]);
			    val->t.day   = atoi(tokens[2]);
			} else {
			    G_warning ("Cannot parse type date (%s)", buf );
			}	
			G_free_tokens( tokens );
		    }
		    break;
		case PG_TIME:
		    val->t.year  = 0;
		    val->t.month = 0;
		    val->t.day   = 0;
		    val->t.hour   = 0;
		    val->t.minute = 0;
		    val->t.seconds = 0;

		    if ( ! PQgetisnull(res, i, j) ) {
			buf = (char *) PQgetvalue(res, i, j);
			tokens = G_tokenize (buf, ":"); /* depends on LOCALE ? */
			ntokens = G_number_of_tokens (tokens);
			if ( ntokens >= 3 ) {
			    val->t.hour   = atoi(tokens[0]);
			    val->t.minute = atoi(tokens[1]);
			    val->t.seconds = atoi(tokens[2]);
			} else {
			    G_warning ("Cannot parse type date (%s)", buf );
			}
			G_free_tokens( tokens );
		    }
		    break;
		}

	    }
	}

	db.tables[t].rows = rows;
	db.tables[t].nrows = nrws;
	db.tables[t].loaded = TRUE;
    }

    db.tables[t].described = TRUE;
    PQclear(res);

    return DB_OK;
}

int free_table(int tab)
{
    int i, j;

    for (i = 0; i < db.tables[tab].nrows; i++) {
	for (j = 0; j < db.tables[tab].ncols; j++) {
	    if (db.tables[tab].cols[j].type == PG_CHAR) {
		free(db.tables[tab].rows[i].values[j].c);
	    }
	}
	free(db.tables[tab].rows[i].values);
    }

    free(db.tables[tab].rows);
    free(db.tables[tab].cols);

    return DB_OK;
}
