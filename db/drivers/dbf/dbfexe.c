/*****************************************************************************
*
* MODULE:       DBF driver 
*   	    	
* AUTHOR(S):    Radim Blazek
*
* PURPOSE:      Simple driver for reading and writing dbf files     
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
#include <shapefil.h>
#include "globals.h"
#include "proto.h"

int sel(SQLPSTMT * st, int tab, int **set);
int set_val(int tab, int row, int col, SQLPVALUE * val);
int eval_node(Node * nodeptr, int tab, int row);

int execute(char *sql, cursor * c)
{
    int i, j, tab;
    SQLPSTMT *st;
    ROW *dbrows;
    int row, nrows;
    int *cols, ncols, col;
    int *selset;
    int dtype, stype;
    int width, decimals;

    /* parse sql statement */
    st = sqpInitStmt();
    st->stmt = sql;
    sqpInitParser(st);

    if (yyparse() != 0) {
	sqpFreeStmt(st);
	sprintf(errMsg, "SQL parser error in statement:\n%s\n", sql);
	return DB_FAILED;
    }

/* sqpPrintStmt(st); *//* debug output only */

    /* find table */
    tab = find_table(st->table);
    if (tab < 0 && st->command != SQLP_CREATE) {
	sprintf(errMsg, "Table '%s' doesn't exist.\n", st->table);
	return DB_FAILED;
    }

    if ((st->command != SQLP_CREATE) && (st->command != SQLP_DROP))
	load_table_head(tab);

    /* find columns */
    ncols = st->nCol;
    if (st->command == SQLP_INSERT || st->command == SQLP_SELECT
	|| st->command == SQLP_UPDATE) {
	if (ncols > 0) {	/* colums were specified */
	    cols = (int *) malloc(ncols * sizeof(int));
	    for (i = 0; i < ncols; i++)
		cols[i] = find_column(tab, st->Col[i].s);
	}
	else {			/* all columns */

	    ncols = db.tables[tab].ncols;
	    cols = (int *) malloc(ncols * sizeof(int));
	    for (i = 0; i < ncols; i++)
		cols[i] = i;
	}
    }

    /* check column types */
    if (st->command == SQLP_INSERT || st->command == SQLP_UPDATE) {
	for (i = 0; i < st->nVal; i++) {
	    col = cols[i];
	    dtype = db.tables[tab].cols[col].type;
	    stype = st->Val[i].type;
	    if ((dtype == DBF_INT && stype != SQLP_I)
		|| (dtype == DBF_DOUBLE && stype == SQLP_S)
		|| (dtype == DBF_CHAR && stype != SQLP_S)) {
		sprintf(errMsg, "Incompatible value type.\n");
		return DB_FAILED;
	    }
	}
    }

    /* do command */
    switch (st->command) {
    case (SQLP_CREATE):
	if (tab >= 0) {
	    sprintf(errMsg, "Table %s already exists\n", st->table);
	    return DB_FAILED;
	}
	add_table(st->table);
	tab = find_table(st->table);

	for (i = 0; i < ncols; i++) {
	    switch (st->ColType[i]) {
	    case (SQLP_INTEGER):
		dtype = DBF_INT;
		width = 11;
		decimals = 0;
		break;
	    case (SQLP_VARCHAR):
		dtype = DBF_CHAR;
		width = st->ColWidth[i];
		decimals = 0;
		break;
	    case (SQLP_DOUBLE):
		dtype = DBF_DOUBLE;
		width = 20;
		decimals = 6;
		break;
	    }
	    add_column(tab, dtype, st->Col[i].s, width, decimals);
	}
	db.tables[tab].described = TRUE;
	db.tables[tab].loaded = TRUE;
	db.tables[tab].updated = TRUE;
	break;

    case (SQLP_DROP):
	unlink(db.tables[tab].file);
	db.tables[tab].alive = FALSE;
	break;

    case (SQLP_INSERT):
	load_table(tab);

	/* add row */
	if (db.tables[tab].nrows == db.tables[tab].arows) {
	    db.tables[tab].arows += 1000;
	    db.tables[tab].rows =
		(ROW *) realloc(db.tables[tab].rows,
				db.tables[tab].arows * sizeof(ROW));
	}
	dbrows = db.tables[tab].rows;
	row = db.tables[tab].nrows;
	dbrows[row].values =
	    (VALUE *) calloc(db.tables[tab].ncols, sizeof(VALUE));
	dbrows[row].alive = TRUE;

	/* set values */
	for (i = 0; i < st->nVal; i++) {
	    col = cols[i];
	    set_val(tab, row, col, &(st->Val[i]));
	}

	db.tables[tab].nrows++;
	db.tables[tab].updated = TRUE;
	break;

    case (SQLP_SELECT):
	c->st = st;
	c->table = tab;
	c->cols = cols;
	c->ncols = ncols;
	c->nrows = sel(st, tab, &(c->set));
	if (c->nrows < 0) {
	    sprintf(errMsg, "%sError in selecting rows\n", errMsg);
	    return DB_FAILED;
	}
	c->cur = -1;
	break;

    case (SQLP_UPDATE):
	nrows = sel(st, tab, &selset);
	if (nrows < 0) {
	    sprintf(errMsg, "%sError in selecting rows\n", errMsg);
	    return DB_FAILED;
	}
	dbrows = db.tables[tab].rows;

	/* update rows */
	for (i = 0; i < nrows; i++) {
	    row = selset[i];
	    for (j = 0; j < st->nVal; j++) {
		col = cols[j];
		set_val(tab, row, col, &(st->Val[j]));
		db.tables[tab].updated = TRUE;
	    }
	}
	break;

    case (SQLP_DELETE):
	nrows = sel(st, tab, &selset);
	if (nrows < 0) {
	    sprintf(errMsg, "%sError in selecting rows\n", errMsg);
	    return DB_FAILED;
	}
	dbrows = db.tables[tab].rows;

	/* delete rows */
	for (i = 0; i < nrows; i++) {
	    row = selset[i];
	    dbrows[row].alive = FALSE;
	    db.tables[tab].updated = TRUE;
	}
	break;

    }

    return DB_OK;
}

int set_val(int tab, int row, int col, SQLPVALUE * val)
{
    VALUE *dbval;

    dbval = &(db.tables[tab].rows[row].values[col]);

    switch (db.tables[tab].cols[col].type) {
    case DBF_INT:
	dbval->i = val->i;
	break;
    case DBF_CHAR:
	save_string(dbval, val->s);
	break;
    case DBF_DOUBLE:
	if (val->type == SQLP_I)
	    dbval->d = val->i;
	else if (val->type == SQLP_D)
	    dbval->d = val->d;
	break;
    }
    return (1);
}

int sel(SQLPSTMT * st, int tab, int **selset)
{
    int i, group_condition;
    int *set;			/* pointer to array of indexes to rows */
    int aset, nset = 0;


    load_table(tab);

    aset = 1;
    set = (int *) malloc(aset * sizeof(int));

    if (st->upperNodeptr) {
	for (i = 0; i < db.tables[tab].nrows; i++) {


	    if ((group_condition = eval_node( st->upperNodeptr, tab, i)) < 0)
		return (-1);

	    G_debug(3, "for row %d total condition is %d", i,
		    group_condition);

	    if (group_condition == TRUE) {
		if (nset == aset) {
		    aset += 1000;
		    set = (int *) realloc(set, aset * sizeof(int));
		}
		set[nset] = i;
		nset++;
	    }
	}
    }
    else {
	aset = db.tables[tab].nrows;
	set = (int *) realloc(set, aset * sizeof(int));
	for (i = 0; i < db.tables[tab].nrows; i++) {
	    set[i] = i;
	}
	nset = db.tables[tab].nrows;
    }
    *selset = set;

    return nset;
}

int eval_node(Node * nptr, int tab, int i)
{

    int ccol, condition, leval = 0, reval = 0;
    COLUMN *col = NULL;
    VALUE *val = NULL;
    double dc, dv;

    Comparison *cmpptr = NULL;
    A_Expr *aexprptr = NULL;
    
    if ( nptr == NULL) return 1; /* empty is true */

    condition = TRUE;

    switch (nptr->type) {

    case T_Comparison:

	cmpptr = (Comparison *) nptr;

	ccol = find_column(tab, cmpptr->lexpr->s);
	col = &(db.tables[tab].cols[ccol]);

	if (((cmpptr->rexpr->type == SQLP_I) && (col->type == DBF_CHAR))
	    || ((cmpptr->rexpr->type == SQLP_D) && (col->type == DBF_CHAR))
	    || ((cmpptr->rexpr->type == SQLP_S) && (col->type == DBF_INT))
	    || ((cmpptr->rexpr->type == SQLP_S) && (col->type == DBF_DOUBLE))) {
	    sprintf(errMsg, "Incompatible types for column: %s\n", col->name);
	    return (-1);
	}

	col = &(db.tables[tab].cols[ccol]);
	val = &(db.tables[tab].rows[i].values[ccol]);

	if (cmpptr->rexpr->type == SQLP_I)
	    dc = cmpptr->rexpr->i;
	else if (cmpptr->rexpr->type == SQLP_D)
	    dc = cmpptr->rexpr->d;

	if (col->type == DBF_INT)
	    dv = val->i;
	else if (col->type == DBF_DOUBLE)
	    dv = val->d;

	switch (cmpptr->rexpr->type) {
	case (SQLP_S):
	    if (cmpptr->opname != SQLP_EQ) {
		sprintf(errMsg, "Operator not supported for strings\n");
		return (-1);
	    }
	    if (strcmp(val->c, cmpptr->rexpr->s) != 0)
		condition = FALSE;
	    break;
	case (SQLP_I):
	case (SQLP_D):
	    switch (cmpptr->opname) {
	    case (SQLP_EQ):
		if (!(dv == dc))
		    condition = FALSE;
		break;
	    case (SQLP_LT):
		if (!(dv < dc))
		    condition = FALSE;
		break;
	    case (SQLP_LE):
		if (!(dv <= dc))
		    condition = FALSE;
		break;
	    case (SQLP_GT):
		if (!(dv > dc))
		    condition = FALSE;
		break;
	    case (SQLP_GE):
		if (!(dv >= dc))
		    condition = FALSE;
		break;
	    case (SQLP_NE):
		if (!(dv != dc))
		    condition = FALSE;
		break;
	    }
	    break;
	}
	break;
     case T_A_Expr:
	aexprptr = (A_Expr *) nptr;
	
	if ( (leval = eval_node(aexprptr->lexpr, tab, i)) != 0 && leval != 1) return (-1);
	if ( (reval = eval_node(aexprptr->rexpr, tab, i)) != 0 && reval != 1) return (-1);
	
	switch (aexprptr->oper) {
	case OR:	    
	    condition = leval | reval;		
	    break;
	case AND:
	    condition = leval & reval;		
	    break;
	case NOT:
	    condition = !reval;		
	    break;
	}
	break;
    }				/* switch type */
    return condition;
}
