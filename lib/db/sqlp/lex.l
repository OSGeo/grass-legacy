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

/****************     C-CODE   *****************/

%{
#include "sqlp.h"
#include "y.tab.h"
#include <string.h>

#undef YY_INPUT
#define YY_INPUT(b, r, ms) (r = my_yyinput(b, ms))

%}

/***************       LEX HEADER  **************/

%e 1200

/****************      LEX BODY  ****************/
%%

 /***************************************
  * LITERALS KEYWORDS TOKENS
  ***************************************/

  /* following case insensitives are ugly
     but I do not know better at this time */

[Dd][Ee][Ll][Ee][Tt][Ee]	{ return DELETE; }
[Ff][Rr][Oo][Mm]		{ return FROM; }
[Ii][Nn][Ss][Ee][Rr][Tt]	{ return INSERT; }
[Ii][Nn][Tt][Oo]		{ return INTO; }
[Ss][Ee][Ll][Ee][Cc][Tt]	{ return SELECT; }
[Ss][Ee][Tt]			{ return SET; }
[Uu][Pp][Dd][Aa][Tt][Ee]	{ return UPDATE; }
[Vv][Aa][Ll][Uu][Ee][Ss]	{ return VALUES; }
[Ww][Hh][Ee][Rr][Ee]		{ return WHERE; }
[Aa][Nn][Dd]			{ return AND; }
[Cc][Rr][Ee][Aa][Tt][Ee]	{ return CREATE; }
[Dd][Rr][Oo][Pp]		{ return DROP; }
[Tt][Aa][Bb][Ll][Ee]		{ return TABLE; }
[Vv][Aa][Rr][Cc][Hh][Aa][Rr]	{ return VARCHAR; }
[Ii][Nn][Tt]			{ return INT; }
[Ii][Nn][Tt][Ee][Gg][Ee][Rr]	{ return INTEGER; }
[Dd][Oo][Uu][Bb][Ll][Ee]	{ return DOUBLE; }
[Oo][Rr]			{ return OR; }
[Nn][Oo][Tt]			{ return NOT; }
 /***************************************
  * EQUAL
  ***************************************/
  
"="		{
			return EQUAL;
		}
 
 /***************************************
  * COMPARISON
  ***************************************/

"<>" 	|
"<"	|
">"	|
"<="	|
">="		{
			yylval.strval = (char*)strdup(yytext);
			return COMPARISON;
		}

 /***************************************
  * PUNCTUATION
  ***************************************/

[-+*/:(),.;]	{
			yylval.strval = (char*)strdup(yytext);
			return yytext[0];
		}

 /***************************************
  * NAMES
  ***************************************/

[A-Za-z][A-Za-z0-9_]*	{
				yylval.strval = (char*)strdup(yytext);
				return NAME;
			}


 /***************************************
  * INTEGER
  ***************************************/

[+-]?[0-9]+  {
			yylval.intval = atoi(yytext);
			/* yylval.strval = (char*)strdup(yytext); */
			return INTNUM;
	      }

 /***************************************
  * FLOATING POINT NUM
  ***************************************/

"."[0-9]* |
[+-]?[0-9]+"."[0-9]* |
[+-]?[0-9]+[eE][+-]?[0-9]+	|
[+-]?[0-9]+"."[0-9]*[eE][+-]?[0-9]+ |
"."[0-9]*[eE][+-]?[0-9]+	{
					yylval.floatval = atof(yytext);
			                /* yylval.strval = (char*)strdup(yytext); */
					return FLOATNUM;
				}


 /***************************************
  * STRINGS (single quotes)
  ***************************************/

'[^'\n]*'	{
			char	*Buffer;
			int 	c = input();
			int     len;
			
			Buffer = (char*)strdup(yytext);	/* store here because we lose it when unput()			*/
			unput( c );				/* just peeking - checking for a double quote... embedded quote */

			if ( c != '\'' )
			{
			        len = strlen (Buffer);
				Buffer[len-1] = '\0';
				yylval.strval = (char*)strdup(Buffer+1);
				free( Buffer );
				return STRING;
			}
			else
			{
				free( Buffer );
				yymore();
			}				
		}
		
 /***************************************
  * STRINGS (unterminated)
  ***************************************/

'[^'\n]*$	{ yyerror("Unterminated string"); }

 /***************************************
  * NEW LINE (ignored)
  ***************************************/

\n		;

 /***************************************
  * WHITE SPACE (ignored)
  ***************************************/

[ \t\r]+	;	/* white space */

 /***************************************
  * COMMENTS (ignored)
  ***************************************/

"--".*$		;	/* comment */

%%
/**********************************************************************
 *
 *                             C-CODE
 *
 **********************************************************************/



/**********************************************************************
 * my_yyinput
 *
 * Lexer will ask this function for input when it requires more.
 *
 **********************************************************************/
int my_yyinput(char *buf, int max_size)
{
        int rest, n;

	rest = sqlpStmt->stmt + strlen( sqlpStmt->stmt) - sqlpStmt->cur;
	n = ( max_size < rest ? max_size : rest );

        if ( n > 0 )
	{
		memcpy( buf, sqlpStmt->cur, n );
		sqlpStmt->cur += n;
	}
		
	return n;		
}

/**********************************************************************
 * yyerror
 *
 * This should be called just before failing. It formats a meaningfull
 * message and deposits it in a usefull place.
 *
 **********************************************************************/
void yyerror( char *s )
{
	snprintf( sqlpStmt->errmsg, 500, "%s processing '%s'", s, yytext );
	
	YY_FLUSH_BUFFER;
}

/**********************************************************************
 * yywrap
 *
 * We are not doing any buffer switching but lets not use the Flex version of
 * of this func anyway so we can avoid the link dependency.
 *
 **********************************************************************/
int yywrap()
{
        return 1;
}

