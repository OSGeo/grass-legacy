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

#include "sqlp.h"
#include <stdio.h>
#include <stdlib.h>

/* save string to value */
int sqpSaveStr(SQLPVALUE *val, char *c )
{
    int len;

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
    sqlpStmt->nCom = 0;
    
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

void sqpComparison( char *col, char *oper, char *strval, int intval, double dblval, int type )
{
    int i;
    
    i = sqlpStmt->nCom;
    sqpAllocCom(sqlpStmt, i + 1 );
    
    sqpSaveStr ( &(sqlpStmt->ComCol[i]), col );

    if ( strcmp ( oper, "=") == 0 )
	sqlpStmt->ComOpe[i] = SQLP_EQ;
    else if ( strcmp ( oper, "<") == 0 )
	sqlpStmt->ComOpe[i] = SQLP_LT;
    else if ( strcmp ( oper, "<=") == 0 )
	sqlpStmt->ComOpe[i] = SQLP_LE;
    else if ( strcmp ( oper, ">") == 0 )
	sqlpStmt->ComOpe[i] = SQLP_GT;
    else if ( strcmp ( oper, ">=") == 0 )
	sqlpStmt->ComOpe[i] = SQLP_GE;
    else if ( strcmp ( oper, "<>") == 0 )
	sqlpStmt->ComOpe[i] = SQLP_NE;
    
    sqlpStmt->ComVal[i].type = type;
    switch ( type  )
      {
        case (SQLP_S):
            sqpSaveStr ( &(sqlpStmt->ComVal[i]), strval );
            break;	
        case (SQLP_I):
            sqlpStmt->ComVal[i].i = intval;
            break;	
        case (SQLP_D):
            sqlpStmt->ComVal[i].d = dblval;
            break;	
      }

    sqlpStmt->nCom++;
    return;
}



