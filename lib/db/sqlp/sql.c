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
    sqlpStmt->orderCol = NULL;

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

void sqpOrderColumn( char *col )
{
    sqlpStmt->orderCol = (char *) realloc (sqlpStmt->orderCol, strlen(col)+1);
    strcpy ( sqlpStmt->orderCol, col );
    return;
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

Node *
makeArithmExpr(int opname, Node *lexpr, Node *rexpr)
{
	ArithmExpr *a = makeNode(ArithmExpr);
	a->oper = SQLP_OP;
	a->opname = opname;
	a->lexpr = lexpr;
	a->rexpr = rexpr;
	return (Node *)a;
}

Node *
makeArithmValue(char *strval, int intval, double dblval, int type, int factor)
{
	ArithmValue *a = makeNode(ArithmValue);
	a->vtype = type;
	a->s = strval;
	a->i = factor * intval;
	a->d = (double) (factor * dblval);	
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

int
translate_Operator( char *oper)
{

    if ( strcmp ( oper, "=") == 0 )
	return SQLP_EQ;
    else if ( strcmp ( oper, "<") == 0 )
	return SQLP_LT;
    else if ( strcmp ( oper, "<=") == 0 )
	return SQLP_LE;
    else if ( strcmp ( oper, ">") == 0 )
	return SQLP_GT;
    else if ( strcmp ( oper, ">=") == 0 )
	return SQLP_GE;
    else if ( strcmp ( oper, "<>") == 0 )
	return SQLP_NE;
    else if ( strcmp ( oper, "~") == 0 )
	return SQLP_MTCH;

    else return 0;
}
