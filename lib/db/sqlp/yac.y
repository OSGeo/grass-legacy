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

%{
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "sqlp.h"

#define YYDEBUG 1

%}
	
	/* symbolic tokens */

%union {
	int intval;
	double floatval;
	char *strval;
	int subtok;
}

	/* operators */


	/* literal keyword tokens */
%token <strval> COMPARISON
%token <strval> NAME
%token <strval> STRING
%token <intval> INTNUM 
%token <floatval> FLOATNUM

%token EQUAL
%token SELECT FROM WHERE
%token DELETE
%token INSERT INTO VALUES
%token UPDATE SET
%token AND
%token CREATE TABLE
%token DROP TABLE
%token VARCHAR
%token INT
%token INTEGER
%token DOUBLE

%{
 
extern int yylex(void);

%}

%%

y_sql:	
		y_create
	|	y_drop
	|	y_insert
	|	y_select
	|	y_update
	|	y_delete
	;
	
y_create:
		CREATE TABLE y_table '(' y_columndefs ')'	{ sqpCommand(SQLP_CREATE); }
	;
	
y_drop:
		DROP TABLE y_table				{ sqpCommand(SQLP_DROP); }
	;

y_select:
		SELECT y_columns FROM y_table y_condition	{ sqpCommand(SQLP_SELECT); }
	;
	
y_delete:
		DELETE FROM y_table y_condition			{ sqpCommand(SQLP_DELETE); }
	;

y_insert:
		INSERT INTO y_table y_values			{ sqpCommand(SQLP_INSERT); }
        |	INSERT INTO y_table '(' y_columns ')' y_values	{ sqpCommand(SQLP_INSERT); }
	;

y_update:
		UPDATE y_table SET y_assignments y_condition	{ sqpCommand(SQLP_UPDATE); }
	;
	
y_columndefs:
		y_columndef
	|	y_columndefs ',' y_columndef
	;

y_columndef:
		NAME VARCHAR '(' INTNUM ')'	{ sqpColumnDef( $1, SQLP_VARCHAR, $4, 0 ); }
	|	NAME INT 			{ sqpColumnDef( $1, SQLP_INTEGER,  0, 0 ); }
	|	NAME INTEGER 			{ sqpColumnDef( $1, SQLP_INTEGER,  0, 0 ); }
	|	NAME DOUBLE			{ sqpColumnDef( $1, SQLP_DOUBLE,   0, 0 ); }
	;

y_columns:
	'*'
        |	y_column_list
	;
	
y_column_list:
		NAME				{ sqpColumn( $1 ); }
	|	y_column_list ',' NAME		{ sqpColumn( $3 ); }
	;

y_table:
		NAME 				{ sqpTable( $1 ); }
	;
	
y_values:
		VALUES '(' y_value_list ')'
	;

y_value_list:
		STRING				{ sqpValue( $1, 0, 0, SQLP_S ); }
        |	INTNUM				{ sqpValue( NULL, $1, 0, SQLP_I ); }
	|	FLOATNUM			{ sqpValue( NULL, 0, $1, SQLP_D ); }
	|	y_value_list ',' STRING		{ sqpValue( $3, 0, 0, SQLP_S ); }
	|	y_value_list ',' INTNUM		{ sqpValue( NULL, $3, 0, SQLP_I ); }
	|	y_value_list ',' FLOATNUM	{ sqpValue( NULL, 0, $3, SQLP_D ); }
	;

y_assignments:
		y_assignment
	|	y_assignments ',' y_assignment
	;
	
y_assignment:
		NAME EQUAL STRING	{ sqpAssignment( $1,   $3,  0, 0, SQLP_S ); }
        |	NAME EQUAL INTNUM	{ sqpAssignment( $1, NULL, $3, 0, SQLP_I ); }
        |	NAME EQUAL FLOATNUM	{ sqpAssignment( $1, NULL,  0,$3, SQLP_D ); }
	;

y_condition:
		
	|	WHERE y_comparisons
	;
	
y_comparisons:
		y_comparison
	|	y_comparisons AND y_comparison
	;
	
y_comparison:
		NAME EQUAL STRING		{ sqpComparison( $1, "=", $3,    0,  0, SQLP_S ); }
        |	NAME EQUAL INTNUM		{ sqpComparison( $1, "=", NULL, $3,  0, SQLP_I ); }
        |	NAME EQUAL FLOATNUM		{ sqpComparison( $1, "=", NULL,  0, $3, SQLP_D ); }
        |	NAME COMPARISON INTNUM		{ sqpComparison( $1, $2,  NULL, $3,  0, SQLP_I ); }
        |	NAME COMPARISON FLOATNUM	{ sqpComparison( $1, $2,  NULL,  0, $3, SQLP_D ); }
	;


%%


