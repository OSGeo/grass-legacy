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

int sel(int tab, int **set, char *sql);
int set_val(int tab, int row, int col, SQLPVALUE * val);
int ParseSqlStmt( SQLPSTMT *st, char *sql);  

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
    st_allocated = 1;
    st->stmt = sql;
    sqpInitParser(st);

    if (yyparse() != 0) {
	sqpFreeStmt(st);
	st_allocated=0;
	sprintf(errMsg, "SQL parser error in statement:\n%s\n", sql);
	return DB_FAILED;
    }


    /* find table */
    tab = find_table(st->table);
    if (tab < 0 && st->command != SQLP_CREATE) {
	sprintf(errMsg, "Table '%s' doesn't exist.\n", st->table);
        sqpFreeStmt(st);
	st_allocated=0;
	return DB_FAILED;
    }

    if ((st->command != SQLP_CREATE) && (st->command != SQLP_DROP))
	load_table_head(tab);
    
    /* find columns */
    ncols = st->nCol;
    if (st->command == SQLP_INSERT || st->command == SQLP_SELECT
	|| st->command == SQLP_UPDATE) {
	if (ncols > 0) {	
	    cols = (int *) malloc(ncols * sizeof(int));
	    for (i = 0; i < ncols; i++)
		cols[i] = find_column(tab, st->Col[i].s);
	}
	else {			

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
        	sqpFreeStmt(st);
		st_allocated=0;
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

	sqpFreeStmt(st); /* need this because of the malloc conflict in DBFOpen,.. etc  --alex*/
	st_allocated=0;
	
	load_table(tab);
	
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

	/* parse sql statement */
    	st = sqpInitStmt();
	st_allocated = 1;
	if ( ParseSqlStmt(st, sql) != DB_OK) 
	    return DB_FAILED;

	
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
	
	sqpFreeStmt(st); /* need this because of the malloc conflict in DBFOpen,.. etc  --alex*/
	st_allocated=0;
		
	c->nrows = sel(tab, &(c->set), sql);
	if (c->nrows < 0) {
	    sprintf(errMsg, "%sError in selecting rows\n", errMsg);
	    return DB_FAILED;
	}
	c->cur = -1;
	break;

    case (SQLP_UPDATE):

	sqpFreeStmt(st); /* need this because of the malloc conflict in DBFOpen,.. etc  --alex*/
	st_allocated=0;
	
	nrows = sel(tab, &selset, sql);
	if (nrows < 0) {
	    sprintf(errMsg, "%sError in selecting rows\n", errMsg);
	    return DB_FAILED;
	}
	dbrows = db.tables[tab].rows;

	
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

	sqpFreeStmt(st); /* need this because of the malloc conflict in DBFOpen,.. etc  --alex*/
	st_allocated=0;
	
	nrows = sel(tab, &selset, sql);
	if (nrows < 0) {
	    sprintf(errMsg, "%sError in selecting rows\n", errMsg);
	    return DB_FAILED;
	}
	dbrows = db.tables[tab].rows;

	
	for (i = 0; i < nrows; i++) {
	    row = selset[i];
	    dbrows[row].alive = FALSE;
	    db.tables[tab].updated = TRUE;
	}
	break;

    }

    if (st_allocated) {
        sqpFreeStmt(st);
	st_allocated=0;
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

int sel(int tab, int **selset, char *sql)
{
    int i, j, ccol, condition, group_condition, g_count;
    int *comcol;		/* array of indexes of comparison cols */
    int *set;			/* pointer to array of indexes to rows */
    int aset, nset = 0;
    COLUMN *col;
    VALUE *val;
    double dc, dv;
    SQLPSTMT *st;

    load_table(tab);

    aset = db.tables[tab].nrows;
    /* aset = 1; */
    /* avoid realloc because of the memory conflict with alive SQLPSTMT struct --alex*/
    set = (int *) malloc(aset * sizeof(int));
 
    /* parse sql statement */
    st = sqpInitStmt();
    st_allocated = 1;
    if ( ParseSqlStmt(st, sql) != DB_OK) 
        return DB_FAILED;

    comcol = (int *) malloc(st->nCom);

    /* find comparison cols and check the type */
    for (i = 0; i < st->nCom; i++) {
	ccol = find_column(tab, st->ComCol[i].s);
	comcol[i] = ccol;
	col = &(db.tables[tab].cols[ccol]);

	if (((st->ComVal[i].type == SQLP_I) && (col->type == DBF_CHAR))
	    || ((st->ComVal[i].type == SQLP_D) && (col->type == DBF_CHAR))
	    || ((st->ComVal[i].type == SQLP_S) && (col->type == DBF_INT))
	    || ((st->ComVal[i].type == SQLP_S) && (col->type == DBF_DOUBLE))) {
	    sprintf(errMsg, "Incompatible types for column: %s\n", col->name);
	    return (-1);
	}
    }

    if (st->nCom > 0) {
	for (i = 0; i < db.tables[tab].nrows; i++) {

	    group_condition = FALSE;

	    for (g_count = 0; g_count < st->numGroupCom; g_count++) {

		condition = TRUE;

		for (j = 0; j < st->nCom; j++) {
		    if (st->ComGrp[j] != g_count)
			continue;

		    ccol = comcol[j];
		    col = &(db.tables[tab].cols[ccol]);
		    val = &(db.tables[tab].rows[i].values[ccol]);

		    if (st->ComVal[j].type == SQLP_I)
			dc = st->ComVal[j].i;
		    else if (st->ComVal[j].type == SQLP_D)
			dc = st->ComVal[j].d;

		    if (col->type == DBF_INT)
			dv = val->i;
		    else if (col->type == DBF_DOUBLE)
			dv = val->d;

		    switch (st->ComVal[j].type) {
		    case (SQLP_S):
			if (st->ComOpe[j] != SQLP_EQ) {
			    sprintf(errMsg,
				    "Operator not supported for strings\n");
			    return (-1);
			}
			if (strcmp(val->c, st->ComVal[j].s) != 0)
			    condition = FALSE;
			break;
		    case (SQLP_I):
		    case (SQLP_D):
			switch (st->ComOpe[j]) {
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

		}		/*end for num of comparisons in group */

		group_condition |= condition;

	    }			/*end for num of groups */
	    
	    G_debug(3, "for row number %d total condition is %d",
			i, group_condition);


	    if (group_condition == TRUE) {

		if (nset == aset) {

       	            sqpFreeStmt(st);
		    st_allocated=0;
		    aset += 100;
		    set = (int *) realloc(set, aset * sizeof(int));
		    
		    /* parse sql statement */
    		    st = sqpInitStmt();
		    st_allocated = 1;
		    if (ParseSqlStmt(st, sql) != DB_OK) 
		        return DB_FAILED;
		}
		set[nset] = i;
		nset++;
	    }
	}
    }
    else {
    
        sqpFreeStmt(st);
	st_allocated=0;
	aset = db.tables[tab].nrows;
	set = (int *) realloc(set, aset * sizeof(int));
	for (i = 0; i < db.tables[tab].nrows; i++) {
	    set[i] = i;
	}
	nset = db.tables[tab].nrows;
    }

    *selset = set;
    if (st_allocated) {
        sqpFreeStmt(st);
	st_allocated=0;
    }

    return nset;
}

int ParseSqlStmt( SQLPSTMT * st, char *sql) 
{
    
    st->stmt = sql;
    sqpInitParser(st);

    if (yyparse() != 0) {
	sqpFreeStmt(st);
	st_allocated=0;
	sprintf(errMsg, "SQL parser error in statement:\n%s\n", sql);
	return DB_FAILED;
    }
/* sqpPrintStmt(st); *//* debug output only */
    return DB_OK;
}
