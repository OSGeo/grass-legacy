/*****************************************************************************
*
* MODULE:       SQL statement parser library 
*   	    	
* AUTHOR(S):    lex.l and yac.y were originaly taken from unixODBC and
*               probably written by Peter Harvey <pharvey@codebydesigns.com>,
*               modifications and other code by Radim Blazek
*
* PURPOSE:      Parse input string containing SQL statement to 
*               SQLPSTMT structure.
*               SQL parser may be used by simple database drivers. 
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#define SQLP_MAIN 

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "sqlp.h"

/* save string to value */
int sqpSaveStr(SQLPVALUE *val, char *c )
{
    int len = 0;

    len = strlen ( c ) + 1;
    val->s = (char *) realloc (val->s, len);

    strcpy ( val->s, c );

    return (1);
}
    
int sqpInitParser(SQLPSTMT *st)
{
    sqlpStmt = st;
    sqlpStmt->cur = sqlpStmt->stmt;

    sqlpStmt->errmsg[0] = '\0';
    sqlpStmt->table[0] = '\0';
    sqlpStmt->nCol = 0;
    sqlpStmt->nVal = 0;
    sqlpStmt->upperNodeptr = NULL;

    return (1);
}

void sqpCommand( int command )
{
    sqlpStmt->command = command;          
    return;
}

void sqpTable( char *tbl )
{
    strncpy (sqlpStmt->table,  tbl, SQLP_MAX_TABLE );
    return;
}

void sqpColumn( char *col )
{
    int i;

    i = sqlpStmt->nCol;
    sqpAllocCol(sqlpStmt, i + 1 );
    sqpSaveStr ( &(sqlpStmt->Col[i]), col );

    sqlpStmt->nCol++;
    return;
}

void sqpColumnDef( char *col, int type, int width, int decimals )
{
    int i;
    
    i = sqlpStmt->nCol;
    sqpAllocCol(sqlpStmt, i + 1 );
    sqpSaveStr ( &(sqlpStmt->Col[i]), col );
    sqlpStmt->ColType[i] = type;
    sqlpStmt->ColWidth[i] = width;
    sqlpStmt->ColDecim[i] = decimals;

    sqlpStmt->nCol++;
    return;
}

void sqpValue( char *strval, int intval, double dblval, int type )
{
    int i;
    
    i = sqlpStmt->nVal;
    sqpAllocVal(sqlpStmt, i + 1 );
    /* allocate space for cols because if in INSERT cols were not
     * specified array for ColNum would not be allocated */
    sqpAllocCol(sqlpStmt, i + 1 );
    
    sqlpStmt->Val[i].type = type;
    switch ( type  )
      {
        case (SQLP_S):
            sqpSaveStr ( &(sqlpStmt->Val[i]), strval );
            break;	
        case (SQLP_I):
            sqlpStmt->Val[i].i = intval;
            break;	
        case (SQLP_D):
            sqlpStmt->Val[i].d = dblval;
            break;	
      }

    sqlpStmt->nVal++;
    return;
}

void sqpAssignment( char *col, char *strval, int intval, double dblval, int type )
{
    int i;
    
    i = sqlpStmt->nCol;
    sqpAllocCol(sqlpStmt, i + 1 );
    sqpAllocVal(sqlpStmt, i + 1 );
    
    sqpSaveStr ( &(sqlpStmt->Col[i]), col );
    sqlpStmt->Val[i].type = type;
    switch ( type  )
      {
        case (SQLP_S):
            sqpSaveStr ( &(sqlpStmt->Val[i]), strval );
            break;	
        case (SQLP_I):
	    sqlpStmt->Val[i].i = intval;
            break;	
        case (SQLP_D):
            sqlpStmt->Val[i].d = dblval;
            break;	
      }

    sqlpStmt->nCol++;
    sqlpStmt->nVal++;
    return;
}

Node *
parseComparison( char *col, char *oper, char *strval, int intval, double dblval, int type)
{
    int ComparisonOperator;
    static SQLPVALUE ComparisonLeftValue, ComparisonRightValue;
    Node *nptr;
    
    memset(&ComparisonLeftValue, '\0', sizeof(ComparisonLeftValue));
    memset(&ComparisonRightValue, '\0', sizeof(ComparisonRightValue));
        
    sqpSaveStr ( &ComparisonLeftValue, col );

    if ( strcmp ( oper, "=") == 0 )
	ComparisonOperator = SQLP_EQ;
    else if ( strcmp ( oper, "<") == 0 )
	ComparisonOperator = SQLP_LT;
    else if ( strcmp ( oper, "<=") == 0 )
	ComparisonOperator = SQLP_LE;
    else if ( strcmp ( oper, ">") == 0 )
	ComparisonOperator = SQLP_GT;
    else if ( strcmp ( oper, ">=") == 0 )
	ComparisonOperator = SQLP_GE;
    else if ( strcmp ( oper, "<>") == 0 )
	ComparisonOperator = SQLP_NE;
    
    ComparisonRightValue.type = type;
    
    switch ( type  )
      {
        case (SQLP_S):
            sqpSaveStr ( &ComparisonRightValue, strval );
            break;	
        case (SQLP_I):
            ComparisonRightValue.i = intval;
            break;	
        case (SQLP_D):
            ComparisonRightValue.d = dblval;
            break;	
      }

	nptr = makeComparison(OP, ComparisonOperator, &ComparisonLeftValue, &ComparisonRightValue);
	if (sqlpStmt->upperNodeptr == NULL) 
	    sqlpStmt->upperNodeptr = nptr;

	return nptr;
}

Node *
makeA_Expr(int oper, int opname, Node *lexpr, Node *rexpr)
{
	A_Expr *a = makeNode(A_Expr);
	a->oper = oper;
	a->opname = opname;
	a->lexpr = lexpr;
	a->rexpr = rexpr;
	return (Node *)a;
}

static Node *
newNode(int size, NodeTag tag)
{
	Node	   *newNode;

	assert(size >= sizeof(Node));		/* need the tag, at least */

	newNode = (Node *) malloc(size);
	memset(newNode, '\0', size);
	newNode->type = tag;
	return newNode;
}

static Node *
makeComparison(int oper, int opname, SQLPVALUE *lexpr, SQLPVALUE *rexpr)
{
    SQLPVALUE * ComparisonField, * ComparisonValue;
    Comparison * a;
    
    ComparisonField  = ( SQLPVALUE *) malloc(sizeof(*lexpr));
    ComparisonValue  = ( SQLPVALUE *) malloc(sizeof(*rexpr));
    
	a = makeNode(Comparison);
	a->oper = oper;
	a->opname = opname;
	a->lexpr = memcpy(ComparisonField, lexpr, sizeof(*lexpr));
	a->rexpr = memcpy(ComparisonValue, rexpr, sizeof(*rexpr));
	return (Node *)a;
}
