/*****************************************************************************
*
* MODULE:       MySQL driver forked from DBF driver by Radim Blazek 
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
#include "gis.h"
#include "globals.h"
#include "proto.h"

/* add table to database */
int add_table(char *table)
{
    if (db.atables == db.ntables) {
	db.atables += 15;
	db.tables = G_realloc(db.tables, db.atables * sizeof(TABLE));
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
	if (strcmp(db.tables[i].name, table) == 0)
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
    MYSQL_RES *res;
    MYSQL_ROW c_row;
    MYSQL_FIELD *field;
    enum enum_field_types dtype;
    char *fname;
    int fsize;
    int fdecimals;
    int type;
    int nflds = 0;
    int nrws = 0;
    char *buf;
    char stmtbuf[NAMELEN + 16];
    ROW *rows;
    VALUE *val;
    int i, j;
    int header_only = 0;

    G_debug(3, "load_table()");

    if (db.tables[t].loaded == TRUE)	/*already loaded */
	return DB_OK;
    G_debug(3, "load_table() - not loaded");

    if (stmt == NULL) {
	memset(stmtbuf, '\0', sizeof(stmtbuf));
	sprintf(stmtbuf, "select * from %s", db.tables[t].name);
	stmt = stmtbuf;
	header_only = 1;
        G_debug(3, "load_table() - only header");

    }

    if (mysql_query(&mysql_conn, stmt) < 0) {
        char *emsg;

	G_asprintf(&emsg, "Error:select MySQL: %s\n",
		 mysql_error(&mysql_conn));
        strncpy(&errMsg, emsg, sizeof(errMsg));
        G_free(emsg);

	return DB_FAILED;
    }

    if ((res = mysql_store_result(&mysql_conn)) == NULL)
	if (mysql_num_fields(res) != 0)
	    return DB_FAILED;


    nflds = mysql_num_fields(res);
    nrws = mysql_num_rows(res);

    for (i = 0; i < nflds; i++) {

	field = mysql_fetch_field_direct(res, i);

	dtype = field->type;
	fname = field->name;
	fsize = field->length;
	fdecimals = field->decimals;


	switch (dtype) {
	case FIELD_TYPE_SHORT:
	case FIELD_TYPE_LONG:
	case FIELD_TYPE_LONGLONG:
	    type = MYSQL_INT;
	    break;
	case FIELD_TYPE_TINY:
	case FIELD_TYPE_VAR_STRING:
	case FIELD_TYPE_STRING:
	case FIELD_TYPE_BLOB:
	    type = MYSQL_CHAR;
	    break;
	case FIELD_TYPE_DOUBLE:
	case FIELD_TYPE_FLOAT:
	    type = MYSQL_DOUBLE;
	    break;
	default:
	    if (!header_only) {
                char *emsg;

		G_asprintf(&emsg,
			 "Field %s can not be selected for query output: type %d not supported yet\n",
			 fname, dtype);
                strncpy(&errMsg, emsg, sizeof(errMsg));
                G_free(emsg);

		return DB_FAILED;
	    }

	    type = MYSQL_UNKNOWN;
	    break;

	}

	add_column(t, type, fname, fsize, fdecimals);

	G_debug(3, "load_table() - number of cols is %d", db.tables[t].ncols);
    }

    if (!header_only) {

	rows = db.tables[t].rows;
	rows = G_malloc(nrws * sizeof(ROW));
	db.tables[t].arows = nrws;

	i = 0;
	while ((c_row = mysql_fetch_row(res)) != NULL) {
	    rows[i].alive = TRUE;
	    rows[i].values = G_calloc(nflds, sizeof(VALUE));

	    for (j = 0; j < nflds; j++) {
		val = &(rows[i].values[j]);
		switch (db.tables[t].cols[j].type) {
		case MYSQL_INT:
		    val->i = atoi(c_row[j]);
		    break;
		case MYSQL_CHAR:
		    buf = (char *) c_row[j];
		    save_string(val, buf);
		    break;
		case MYSQL_DOUBLE:
		    val->d = atof(c_row[j]);
		    break;
		}

	    }
	    i++;
	}

	db.tables[t].rows = rows;
	db.tables[t].nrows = nrws;
	db.tables[t].loaded = TRUE;
    }

    db.tables[t].described = TRUE;
    mysql_free_result(res);

    return DB_OK;
}

int free_table(int tab)
{
    int i, j;

    for (i = 0; i < db.tables[tab].nrows; i++) {
	for (j = 0; j < db.tables[tab].ncols; j++) {
	    if (db.tables[tab].cols[j].type == MYSQL_CHAR) {
		free(db.tables[tab].rows[i].values[j].c);
	    }
	}
	free(db.tables[tab].rows[i].values);
    }

    free(db.tables[tab].rows);
    free(db.tables[tab].cols);

    return DB_OK;
}
