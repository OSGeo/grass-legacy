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

int yyparse(void);
int sel(SQLPSTMT * st, int tab, int **set);
int set_val(int tab, int row, int col, SQLPVALUE * val);
double eval_node(Node * nodeptr, int tab, int row);
int eval_arithmvalue_type(Node * nodeptr, int tab);
double get_arithmvalue(Node * nodeptr, int tab, int row, SQLPVALUE * res);
void free_all_nodes(Node * nodeptr);

int execute(char *sql, cursor * c)
{
    int i, j, tab, ret;
    SQLPSTMT *st;
    ROW *dbrows;
    int row, nrows;
    int *cols, ncols, col;
    int *selset;
    int dtype, stype;
    int width, decimals;
    char *tmpsql, name[500];

    /* parse sql statement */
    /* I don't know why, but if the statement ends by string in quotes 'xxx' and is not 
    *  followed by space or '\n' it is not parsed properly -> */
    tmpsql = (char*) G_malloc ( strlen(sql) + 2 );
    sprintf ( tmpsql, "%s ", sql );
    st = sqpInitStmt();
    st->stmt = tmpsql;
    sqpInitParser(st);

    if (yyparse() != 0) {
	sqpFreeStmt(st);
	free ( tmpsql) ;
	sprintf(errMsg, "SQL parser error in statement:\n%s\n", sql);
	return DB_FAILED;
    }
    free ( tmpsql) ;

/* sqpPrintStmt(st); *//* debug output only */

    /* find table */
    tab = find_table(st->table);
    if (tab < 0 && st->command != SQLP_CREATE) {
	sprintf(errMsg, "Table '%s' doesn't exist.\n", st->table);
	return DB_FAILED;
    }

    /* For DROP we have to call load_table_head() because it reads permissions */
    if ((st->command != SQLP_CREATE)) {
	ret = load_table_head(tab);
	if ( ret == DB_FAILED ) { 
	    sprintf(errMsg, "%sCannot load table head.\n", errMsg);
	    return DB_FAILED;
	}
    }

    if ((st->command == SQLP_DROP) || (st->command == SQLP_DELETE) ||
        (st->command == SQLP_INSERT) || (st->command == SQLP_UPDATE)
    ) {
	if ( db.tables[tab].write == FALSE ) {
  	    sprintf(errMsg, "Cannot modify table, don't have write permission for DBF file.\n");
	    return DB_FAILED;
	}
    }
    
    /* find columns */
    ncols = st->nCol;
    if (st->command == SQLP_INSERT || st->command == SQLP_SELECT
	|| st->command == SQLP_UPDATE) {
	if (ncols > 0) {	/* colums were specified */
	    cols = (int *) malloc(ncols * sizeof(int));
	    for (i = 0; i < ncols; i++) {
		cols[i] = find_column(tab, st->Col[i].s);
		if ( cols[i] == -1 ) {
	            sprintf(errMsg, "Column '%s' not found\n", st->Col[i].s);
	            return DB_FAILED;
		}
	    }
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
	sprintf ( name, "%s.dbf", st->table );
	add_table(st->table, name );
	
	tab = find_table(st->table);
	db.tables[tab].read = TRUE;
	db.tables[tab].write = TRUE;

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
	G_debug ( 2, "SELECT");
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

/* Select records, sets 'selset' to new array of items and returns
*  number of items or -1 for error */
int sel(SQLPSTMT * st, int tab, int **selset)
{
    int i, ret, group_condition;
    int *set;			/* pointer to array of indexes to rows */
    int aset, nset = 0;

    G_debug ( 2, "sel(): tab = %d", tab);

    ret = load_table(tab);
    if ( ret == DB_FAILED ) {
	sprintf(errMsg, "%sCannot load table.\n", errMsg);
	return -1;
    }

    aset = 1;
    set = (int *) malloc(aset * sizeof(int));

    if (st->upperNodeptr) {
	for (i = 0; i < db.tables[tab].nrows; i++) {


	    if ((group_condition = (int) eval_node( st->upperNodeptr, tab, i)) < 0) {
		free_all_nodes ( st->upperNodeptr);
		return (-1);
	    }
	    
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
		    free_all_nodes ( st->upperNodeptr);

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

/* TODO: don't use exit, set error message and return DB_FAILED to application */
	
/* Returns -1 on error */
double eval_node(Node * nptr, int tab, int i)
{
    int leval = 0, reval = 0;
    double condition;
    double dleval, dreval;
    SQLPVALUE lstr, rstr;
    int lres = 0, rres = 0;
    
    A_Expr *aexprptr = NULL;
    ArithmExpr *arithmptr = NULL;
    
    if ( nptr == NULL) return 1; /* empty is true */

    memset (&lstr, '\0', sizeof(SQLPVALUE));
    memset (&rstr, '\0', sizeof(SQLPVALUE));

    condition = TRUE;

    switch (nptr->type) {

     case T_A_Expr:
	aexprptr = (A_Expr *) nptr;
	
	if (aexprptr->oper != SQLP_OP){
	    if ( (leval = (int) eval_node(aexprptr->lexpr, tab, i)) != 0 && leval != 1) return (-1);
	    if ( (reval = (int) eval_node(aexprptr->rexpr, tab, i)) != 0 && reval != 1) return (-1);
	}
	
	switch (aexprptr->oper) {
	case SQLP_OR:	    
	    condition = leval | reval;		
	    break;
	case SQLP_AND:
	    condition = leval & reval;		
	    break;
	case SQLP_NOT:
	    condition = !reval;		
	    break;
	case SQLP_OP:

	leval = eval_arithmvalue_type(aexprptr->lexpr, tab);
	reval = eval_arithmvalue_type(aexprptr->rexpr, tab);
	
	if ((leval != reval) && (leval/reval !=2) && (reval/leval !=2) ) {
	    G_debug(0,"Incompatible types in comparison."); 
	    G_debug(3,"Exiting in eval_node, T_A_Expr."); 
	    exit (-1);
	}
	if ( leval == reval && leval == SQLP_S) {
	    if ( aexprptr->opname != SQLP_EQ) {
	        G_debug(0,"Impossible operator for string values."); 
		exit(-1);
	    } 

	    lres = (int) get_arithmvalue(aexprptr->lexpr, tab, i, &lstr);
	    rres = (int) get_arithmvalue(aexprptr->rexpr, tab, i, &rstr);
	    
	    if (!lres && !rres)
	        if ( strcmp(lstr.s,rstr.s) != 0) condition = FALSE;
	    
	} else {
	    dleval = eval_node(aexprptr->lexpr, tab, i);
	    dreval = eval_node(aexprptr->rexpr, tab, i);
	
	    switch (aexprptr->opname) {
	    case (SQLP_EQ):
		if (!(dleval == dreval))
		    condition = FALSE;
		break;
	    case (SQLP_LT):
		if (!(dleval < dreval))
		    condition = FALSE;
		break;
	    case (SQLP_LE):
		if (!(dleval <= dreval))
		    condition = FALSE;
		break;
	    case (SQLP_GT):
		if (!(dleval > dreval))
		    condition = FALSE;
		break;
	    case (SQLP_GE):
		if (!(dleval >= dreval))
		    condition = FALSE;
		break;
	    case (SQLP_NE):
		if (!(dleval != dreval))
		    condition = FALSE;
		break;
	    }
	 } 
	    break;
	}		/* case OP*/
	break;
    case T_ArithmExpr:
        arithmptr = (ArithmExpr *) nptr;
	leval = eval_arithmvalue_type(arithmptr->lexpr, tab);
	dleval = get_arithmvalue(arithmptr->lexpr, tab, i, &lstr);

	if (arithmptr->rexpr != NULL ) {
	    reval = eval_arithmvalue_type(arithmptr->rexpr, tab);
	    if ((leval != reval) && (leval/reval !=2) && (reval/leval !=2) ) {
	        G_debug(0,"Incompatible types in comparison."); 
		G_debug(3,"Exiting in eval_node, T_ArithmExpr."); 
		exit (-1);
	    }
	}

	if (arithmptr->rexpr != NULL ) 
	    dreval = get_arithmvalue(arithmptr->rexpr, tab, i, &rstr);
	else dreval = 0.0;
	switch (arithmptr->opname) {
	case SQLP_ADD:	    
	    condition = dleval + dreval;		
	    break;
	case SQLP_SUBTR:
	    condition = dleval - dreval;		
	    break;
	case SQLP_MLTP:
	    condition = dleval * dreval;		
	    break;
	case SQLP_DIV:
	    if (dreval != 0.0) condition = dleval / dreval;
	    else {
	    G_debug(0,"Floating point exception - division by zero inside comparison\n");
	    return ((double) 0xfffffffe);
	    }		
	    break;
	}
    }				/* switch node type */
    return condition;
}

int eval_arithmvalue_type(Node * nptr, int tab ) {
    int leval = 0, reval = 0;
    int ccol;
    COLUMN *col = NULL;

    ArithmExpr *arithmptr = NULL;
    ArithmValue *valueptr = NULL;
    
    switch (nptr->type) {
    case T_ArithmExpr:
        arithmptr = (ArithmExpr *) nptr;
	leval = eval_arithmvalue_type(arithmptr->lexpr, tab);

	if (arithmptr->rexpr != NULL ) {
	    reval = eval_arithmvalue_type(arithmptr->rexpr, tab);
	    if ((leval != reval) && (leval/reval !=2) && (reval/leval !=2) ) {
	        fprintf(stderr,"Incompatible types in comparison."); 
	        G_debug(3,"Exiting in eval_arithmvalue_type"); 
		exit (-1);
	    }
	}

	return (leval);
	break;
    case T_ArithmValue:
        valueptr = (ArithmValue *) nptr;
	leval = valueptr->vtype;
	if (leval != SQLP_COL) return leval;
	else {
	    ccol = find_column(tab, valueptr->s);
	    if ( ccol == -1 ) {
		fprintf(stderr, "Column '%s' not found\n", valueptr->s);
		exit (-1);
	    }
	    col = &(db.tables[tab].cols[ccol]);
	    switch (col->type) {
	    case DBF_CHAR:
	        return (SQLP_S);
		break;
	    case DBF_INT:
	    case DBF_DOUBLE:
	        return (SQLP_D);
		break;
	    }
	}
	break;
    }
    
    return 0;  /* OK ? */
}

double get_arithmvalue(Node * nptr, int tab, int i, SQLPVALUE * res) {
    int ccol;
    COLUMN *col = NULL;
    VALUE *val = NULL;
    double dleval, dreval;
    
    ArithmExpr *arithmptr = NULL;
    ArithmValue *valueptr = NULL;
    switch (nptr->type) {
    case T_ArithmExpr:
        arithmptr = (ArithmExpr *) nptr;
	dleval = get_arithmvalue(arithmptr->lexpr, tab, i, res);
	
	if (arithmptr->rexpr != NULL )
	    dreval = get_arithmvalue(arithmptr->rexpr, tab, i, res);
	else dreval = 0.0;
	
	switch (arithmptr->opname) {
	case SQLP_ADD:	    
	    return (dleval + dreval);		
	    break;
	case SQLP_SUBTR:
	    return (dleval - dreval);		
	    break;
	case SQLP_MLTP:
	    return (dleval * dreval);		
	    break;
	case SQLP_DIV:
	    if (dreval != 0.0) return (dleval / dreval);
	    else {
	    G_debug(0,"Floating point exception - division by zero inside comparison\n");
	    return ((double) 0xfffffffe);
	    }		
	    break;
	}
	break;
    case T_ArithmValue:
        valueptr = (ArithmValue *) nptr;
	switch (valueptr->vtype) {
	case SQLP_D: 
	    return (valueptr->d);
	    break;
	case SQLP_I: 
	    return ((double) valueptr->i);
	    break;
	case SQLP_S:
	    res->s = valueptr->s; 
	    return (0);
	    break;
	case SQLP_COL:
	    ccol = find_column(tab, valueptr->s);
	    col = &(db.tables[tab].cols[ccol]);
	    val = &(db.tables[tab].rows[i].values[ccol]);
	    switch (col->type) {
	    case DBF_CHAR:

		res->s = val->c; 
	        return (0);
		break;
	    case DBF_INT:
	        return ((double) val->i);
		break;
	    case DBF_DOUBLE:
	        return (val->d);
		break;
	    }
	    break;
	}
	break;
    }

    return 0;  /* OK ? */
}

void free_all_nodes(Node * nptr)
{
    A_Expr *aexprptr = NULL;
    ArithmExpr *arithmptr = NULL;
    ArithmValue *valueptr = NULL;

    if ( nptr == NULL) return;

    switch (nptr->type) {

     case T_A_Expr:
	aexprptr = (A_Expr *) nptr;
	
	free_all_nodes(aexprptr->lexpr);
	free_all_nodes(aexprptr->rexpr);
	
	free (aexprptr);
	break;
	
    case T_ArithmExpr:
        arithmptr = (ArithmExpr *) nptr;
	
	free_all_nodes(arithmptr->lexpr);
	free_all_nodes(arithmptr->rexpr);
	
	free (arithmptr);
	break;
    case T_ArithmValue:
        valueptr = (ArithmValue *) nptr;
	
	free (valueptr);
	break;
   }
}
