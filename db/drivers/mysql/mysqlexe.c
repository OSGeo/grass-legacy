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
#include <unistd.h>
#include <dbmi.h>
#include "gis.h"
#include "globals.h"
#include "proto.h"

/*REMOVE THIS UGLY HACK AS SOON AS PARSER UNDERSTANDS OTHER SELECT QUERIES*/
#define UGLYHACK

int fire_mysql_cmd(char *mysql_cmd);
int sel(SQLPSTMT * st, int tab, int **set, int *n_cols, int **colset);

int execute(char *sql, cursor * c)
{
    int tab;
    SQLPSTMT *st;

    char tb[SQLP_MAX_TABLE];
    char *fstr;

    /* parse sql statement */
    st = sqpInitStmt();
    st->stmt = sql;
    sqpInitParser(st);

/*    if (yyparse() != 0) {
 *
 * 	sqpFreeStmt(st);
 * 	sprintf( errMsg, "SQL parser error in statement:\n%s\n", sql);
 * 	return DB_FAILED;
 *
 *    }
 */

/* sqpPrintStmt(st); *//* debug output only */

#ifdef UGLYHACK
    if (!strncmp(st->stmt, "SELECT ", (size_t)7) || !strncmp(st->stmt, "select ", (size_t)7))
	st->command = SQLP_SELECT;

    memset(tb, '\0', sizeof(tb));
    fstr = strstr(st->stmt, " from ");
    if (fstr == NULL)
	fstr = strstr(st->stmt, " FROM ");
    if (fstr != NULL) {
	sscanf(fstr, "%*s%s ", tb);
	strcpy(st->table, tb);
    }
#endif
    /* find table */
    tab = find_table(st->table);
/*
 *     if (tab < 0 && st->command != SQLP_CREATE) {
 * 	sprintf(errMsg, "Table '%s' doesn't exist.\n", st->table);
 * 	return DB_FAILED;
 *     }
 */

    /* do command */


    switch (st->command) {
    case (SQLP_SELECT):

	if (tab < 0) {
#ifdef UGLYHACK
	    sprintf(errMsg,
		    "Table '%s' doesn't exist. \n(FROM/from keyword?) \n",
		    st->table);
#else
	    sprintf(errMsg, "Table '%s' doesn't exist. \n", st->table);
#endif
	    return DB_FAILED;
	}

	c->st = st;
	c->table = tab;
/*
 * 	    c->cols = cols;
 * 	    c->ncols = ncols; 
 */
	c->nrows = sel(st, tab, &(c->set), &(c->ncols), &(c->cols));
	if (c->nrows < 0) {
	    sprintf(errMsg, "%sError in selecting rows\n", errMsg);
	    sqpFreeStmt(st);
	    return DB_FAILED;
	}
	c->cur = -1;
	break;
    default:


	if (fire_mysql_cmd(st->stmt) != 0) {
	    sqpFreeStmt(st);
	    return DB_FAILED;
	}

	break;


    }

    return DB_OK;
}

int fire_mysql_cmd(char *stmt)
{
    char *emsg;

    if (mysql_query(&mysql_conn, stmt) < 0) {
	G_asprintf(&emsg, "Error: sending MySQL command: %s\n",
		 mysql_error(&mysql_conn));
        strncpy(&errMsg, emsg, sizeof(errMsg));
        G_free(emsg);

	return DB_FAILED;
    }

    return DB_OK;
}

int sel(SQLPSTMT * st, int tab, int **selset, int *n_cols, int **colset)
{
    int i;
    int *set, *cols;		/* pointers to arrays of indexes to rows and cols */
    int nrws = 0;

    int nflds = 0;

    make_table_brand_new(tab);

    if (load_table(tab, st->stmt) == DB_FAILED)
	return -1;

    nflds = db.tables[tab].ncols;

    if (nflds) {
	cols = (int *) malloc(nflds * sizeof(int));
	for (i = 0; i < nflds; i++)
	    cols[i] = i;
    }
    else 
        return -1;


    nrws = db.tables[tab].nrows;
    set = (int *) malloc(nrws * sizeof(int));
    for (i = 0; i < db.tables[tab].nrows; i++) {
	set[i] = i;
    }

    *n_cols = db.tables[tab].ncols;
    *colset = cols;
    *selset = set;

    return nrws;
}
