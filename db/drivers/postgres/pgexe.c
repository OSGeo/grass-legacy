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
#include <unistd.h>
#include <dbmi.h>
#include "globals.h"
#include "proto.h"

int fire_pg_cmd(char *pg_cmd);
int sel(SQLPSTMT * st, int tab, int **set, int *n_cols, int **colset);

int execute(char *sql, cursor * c)
{
    int tab;
    SQLPSTMT *st;

    /* parse sql statement */
    st = sqpInitStmt();
    st->stmt = sql;
    sqpInitParser(st);

    if (yyparse() != 0) {
/*
 * 	sqpFreeStmt(st);
 * 	sprintf( errMsg, "SQL parser error in statement:\n%s\n", sql);
 * 	return DB_FAILED;
 */
    }

/* sqpPrintStmt(st); *//* debug output only */

    /* find table */
    tab = find_table(st->table);
/*
 *     if (tab < 0 && st->command != SQLP_CREATE) {
 * 	sprintf(errMsg, "Table '%s' doesn't exist.\n", st->table);
 * 	return DB_FAILED;
 *     }
 */

/*REMOVE THIS UGLY HACK AS SOON AS PARSER UNDERSTANDS OTHER SELECT QUERIES*/
    if (!strncmp(st->stmt, "SELECT ", 7) || !strncmp(st->stmt, "select ", 7))
	st->command = SQLP_SELECT;

    /* do command */


    switch (st->command) {
    case (SQLP_SELECT):
	c->st = st;
	c->table = tab;
/*
 * 	    c->cols = cols;
 * 	    c->ncols = ncols; 
 */
	c->nrows = sel(st, tab, &(c->set), &(c->ncols), &(c->cols));
	if (c->nrows < 0) {
	    sprintf(errMsg, "%sError in selecting rows\n", errMsg);
	    return DB_FAILED;
	}
	c->cur = -1;
	break;
    default:


	if (fire_pg_cmd(st->stmt) != 0) {
	    sqpFreeStmt(st);
	    sprintf(errMsg, "SQL error in statement:\n%s\n", sql);
	    return DB_FAILED;
	}

	break;


    }

    return DB_OK;
}

int fire_pg_cmd(char *stmt)
{

    PGresult *res;
    char emsg[PG_MSG];

    res = PQexec(pg_conn, stmt);

    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
	snprintf(emsg, sizeof(emsg), "Error:executing Postgres command %s\n",
		 PQerrorMessage(pg_conn));
	report_error(emsg);
	return DB_FAILED;
    }

    PQclear(res);

    return DB_OK;
}

int sel(SQLPSTMT * st, int tab, int **selset, int *n_cols, int **colset)
{
    int i;
    int *set, *cols;		/* pointers to arrays of indexes to rows and cols */
    int nrws = 0;

    int nflds = 0;


    load_table(tab, st->stmt);
    nflds = db.tables[tab].ncols;

    if (nflds) {
	cols = (int *) malloc(nflds * sizeof(int));
	for (i = 0; i < nflds; i++)
	    cols[i] = i;
    }


    nrws = db.tables[tab].nrows;
    set = (int *) malloc(nrws * sizeof(int));
    for (i = 0; i < db.tables[tab].nrows; i++) {
	set[i] = i;
    }

    n_cols = &(db.tables[tab].ncols);
    *colset = cols;
    *selset = set;

    return nrws;
}
