# define NAME 257
# define STRING 258
# define INTEGER 259
# define FLOAT 260
# define FUNCTION 261
# define GT 262
# define GE 263
# define EQ 264
# define LT 265
# define LE 266
# define AND 267
# define OR 268
# define COLOR_GRAY 269
# define COLOR_RED 270
# define COLOR_BLUE 271
# define COLOR_GREEN 272
# define NE 273
# define UMINUS 274
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval, yyval;
typedef int yytabelem;
# define YYERRCODE 256

# line 65 "pol.y"


yywrap()
{
    return 1;
}
yyerror(s) char *s;
{
    printf ("??\n");
}

static int nstored = 0;
static char **storage = 0;
char *malloc(), *realloc();

store (s) char *s;
{
    int i;
    for (i = 0; i < nstored; i++)
	if (strcmp (s, storage[i]) == 0)
	    return i;

    if(nstored++)
	storage = (char **) realloc (storage, nstored * sizeof (*storage));
    else
	storage = (char **) malloc (nstored * sizeof (*storage));

    storage[nstored-1] = malloc (strlen(s)+1);
    strcpy (storage[nstored-1],s);
    return nstored - 1;
}

static int function_level = 0;
static int nfuncs = 0;
static int *nargs = 0;

begin_function()
{
    function_level++;
    if (function_level > nfuncs)
    {
	if (nfuncs)
	    nargs = (int *) realloc (nargs, function_level * sizeof (int));
	else
	    nargs = (int *) malloc (function_level * sizeof (int));
	nfuncs = function_level;
    }
    nargs[function_level-1] = 0;
}
another_arg ()
{
    nargs[function_level-1]++;
}
function (n)
{
    function_level--;
    printf ("F%s %d\n", storage[n], nargs[function_level]);
}

name(n)
{
    int i;

    if (i = find_variable(n))
	printf ("v%d\n", i-1);
    else
	mapname (n,'M',0,0);
}

mapname (n, code, row, col)
    char code;
{
    printf ("M%c %d %d %s\n", code, row, col, storage[n]);
}
integer (n)
{
    printf ("I%d\n",n);
}
floating_point (n)
{
    printf ("D%s\n",storage[n]);
}
unary_opcode (s) char *s;
{
    printf ("1%s\n",s);
}
binary_opcode (s) char *s;
{
    printf ("2%s\n",s);
}
compare (s) char *s;
{
    printf ("C%s\n",s);
}
logical (s) char *s;
{
    printf ("L%s\n",s);
}

assign (n)
{
    printf ("=%s\n", storage[n]);
}

static int *vars ;
static int nvars = 0;

find_variable (n)
{
    int i;

    for (i = 0; i < nvars; i++)
	if (vars[i] == n)
	    return(i+1);
    return 0;
}

define_variable (n)
{
    int i;

    if (i = find_variable(n))
    {
	printf ("V%d\n",i-1);
	return i;
    }
    if (nvars++)
	vars = (int *) realloc (vars, nvars * sizeof(int));
    else
	vars = (int *) malloc (sizeof(int));
    
    vars[nvars-1] = n;
    printf ("V%d\n",nvars-1);
    return nvars;
}


#include "lex.yy.c"
yytabelem yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 9,
	91, 37,
	-2, 19,
-1, 10,
	91, 38,
	-2, 20,
	};
# define YYNPROD 41
# define YYLAST 300
yytabelem yyact[]={

    32,    21,    60,    67,    56,    30,    33,    77,    34,    73,
    31,    66,    32,    40,    41,     3,     2,    30,    33,    75,
    34,    61,    31,    38,    42,    37,    69,     4,    32,    70,
    58,    11,    32,    30,    33,    32,    34,    30,    31,     5,
    30,    33,    31,    34,    39,    31,    63,    12,     7,    62,
     1,     0,     0,     8,     7,     0,     6,     0,    72,     8,
    65,     0,     0,    32,    35,    36,     0,     0,    30,    33,
     0,    34,    16,    31,     0,     0,     0,     0,    16,    43,
    44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
    54,    55,    68,     0,    57,     0,     0,    71,     0,    64,
     0,     0,     0,     0,    76,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    59,     0,     0,     0,
     0,     0,     0,     0,     0,    24,    25,    28,    26,    27,
    22,    23,     0,     0,     0,     0,    29,    24,    25,    28,
    26,    27,    22,    23,     0,     0,     0,     0,    29,     0,
     0,     0,     0,    24,    25,    28,    26,    27,    22,    23,
     0,     0,     0,     0,    29,     9,    10,    14,    15,    13,
     0,     9,    10,    14,    15,    13,     0,    17,    18,    20,
    19,     0,     0,    17,    18,    20,    19,     0,    24,    25,
    28,    26,    27,     0,     0,     0,     0,     0,     0,    29 };
yytabelem yypact[]={

  -241, -1000,   -34,    29,    14, -1000,    -9,    14,    14,   -36,
 -1000,   -68,  -244,   -16, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000,    14,    14,    14,    14,    14,    14,    14,    14,
    14,    14,    14,    14,    14,   -37, -1000,    14,   -43,   -70,
 -1000, -1000,     8,    26,    26,    -2,    -2,    -2,    -2,    -2,
    -2, -1000, -1000, -1000,    -5,    -5, -1000,   -25,   -33, -1000,
  -256,   -43, -1000,   -15,   -25, -1000,   -43, -1000,   -35, -1000,
    14,   -74, -1000,   -43,   -25, -1000,   -86, -1000 };
yytabelem yypgo[]={

     0,    50,    56,    31,    30,    47,    46 };
yytabelem yyr1[]={

     0,     1,     1,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     5,     5,     5,     5,     5,     6,     6,     3,     3,     4,
     4 };
yytabelem yyr2[]={

     0,     9,     5,     6,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     5,     7,     3,
     3,     9,    13,     5,    11,    15,     7,     9,     3,     3,
     3,     3,     3,     3,     3,     3,     7,     2,     2,     3,
     5 };
yytabelem yychk[]={

 -1000,    -1,   257,   256,    61,    10,    -2,    40,    45,   257,
   258,    -3,    -5,   261,   259,   260,    64,   269,   270,   272,
   271,    10,   267,   268,   262,   263,   265,   266,   264,   273,
    42,    47,    37,    43,    45,    -2,    -2,    61,    91,    -3,
   257,   258,    40,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
    -2,    -2,    -2,    -2,    -2,    -2,    41,    -2,    -4,   259,
    45,    91,    41,    -6,    -2,    93,    44,   259,    -4,    41,
    44,    -4,    93,    44,    -2,    93,    -4,    93 };
yytabelem yydef[]={

     0,    -2,     0,     0,     0,     2,     0,     0,     0,    -2,
    -2,     0,     0,     0,    28,    29,    30,    31,    32,    33,
    34,     1,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    17,     0,     0,    23,
    37,    38,     0,     4,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,     3,    18,     0,    39,
     0,     0,    26,     0,    35,    21,     0,    40,     0,    27,
     0,     0,    24,     0,    36,    22,     0,    25 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"NAME",	257,
	"STRING",	258,
	"INTEGER",	259,
	"FLOAT",	260,
	"FUNCTION",	261,
	"GT",	262,
	"GE",	263,
	"EQ",	264,
	"LT",	265,
	"LE",	266,
	"AND",	267,
	"OR",	268,
	"COLOR_GRAY",	269,
	"COLOR_RED",	270,
	"COLOR_BLUE",	271,
	"COLOR_GREEN",	272,
	"NE",	273,
	"+",	43,
	"-",	45,
	"*",	42,
	"/",	47,
	"%",	37,
	"UMINUS",	274,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"stmt : NAME '=' exp '\n'",
	"stmt : error '\n'",
	"exp : '(' exp ')'",
	"exp : exp AND exp",
	"exp : exp OR exp",
	"exp : exp GT exp",
	"exp : exp GE exp",
	"exp : exp LT exp",
	"exp : exp LE exp",
	"exp : exp EQ exp",
	"exp : exp NE exp",
	"exp : exp '*' exp",
	"exp : exp '/' exp",
	"exp : exp '%' exp",
	"exp : exp '+' exp",
	"exp : exp '-' exp",
	"exp : '-' exp",
	"exp : NAME '=' exp",
	"exp : NAME",
	"exp : STRING",
	"exp : map '[' index ']'",
	"exp : map '[' index ',' index ']'",
	"exp : mapmod map",
	"exp : mapmod map '[' index ']'",
	"exp : mapmod map '[' index ',' index ']'",
	"exp : FUNCTION '(' ')'",
	"exp : FUNCTION '(' exp_list ')'",
	"exp : INTEGER",
	"exp : FLOAT",
	"mapmod : '@'",
	"mapmod : COLOR_GRAY",
	"mapmod : COLOR_RED",
	"mapmod : COLOR_GREEN",
	"mapmod : COLOR_BLUE",
	"exp_list : exp",
	"exp_list : exp_list ',' exp",
	"map : NAME",
	"map : STRING",
	"index : INTEGER",
	"index : '-' INTEGER",
};
#endif /* YYDEBUG */
/* 
 *	Copyright 1987 Silicon Graphics, Inc. - All Rights Reserved
 */

/* #ident	"@(#)yacc:yaccpar	1.10" */
#ident	"$Header: /d2/3.1Esource/root/usr/src/cmd/yacc/RCS/yaccpar,v 1.4 87/11/30 17:32:21 deb Exp $"

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	return(0)
#define YYABORT		return(1)
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-1000)

/*
** global variables used by the parser
*/
YYSTYPE yyv[ YYMAXDEPTH ];	/* value stack */
int yys[ YYMAXDEPTH ];		/* state stack */

YYSTYPE *yypv;			/* top of value stack */
int *yyps;			/* top of state stack */

int yystate;			/* current state */
int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */
int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */



/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
int
yyparse()
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

	goto yystack;
	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ YYMAXDEPTH ] )	/* room on stack? */
		{
			yyerror( "yacc stack overflow" );
			YYABORT;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			printf( "Received token " );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				printf( "Received token " );
				if ( yychar == 0 )
					printf( "end-of-file\n" );
				else if ( yychar < 0 )
					printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
			skip_init:
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					printf( "Error recovery discards " );
					if ( yychar == 0 )
						printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 1:
# line 12 "pol.y"
{ assign(yypvt[-3]); return 1; } break;
case 2:
# line 13 "pol.y"
{ return 0; } break;
case 4:
# line 17 "pol.y"
{ logical ("&"); } break;
case 5:
# line 18 "pol.y"
{ logical ("|"); } break;
case 6:
# line 19 "pol.y"
{ compare (">"); } break;
case 7:
# line 20 "pol.y"
{ compare (">="); } break;
case 8:
# line 21 "pol.y"
{ compare ("<"); } break;
case 9:
# line 22 "pol.y"
{ compare ("<="); } break;
case 10:
# line 23 "pol.y"
{ compare ("="); } break;
case 11:
# line 24 "pol.y"
{ compare ("!"); } break;
case 12:
# line 25 "pol.y"
{ binary_opcode ("*"); } break;
case 13:
# line 26 "pol.y"
{ binary_opcode ("/"); } break;
case 14:
# line 27 "pol.y"
{ binary_opcode ("%"); } break;
case 15:
# line 28 "pol.y"
{ binary_opcode ("+"); } break;
case 16:
# line 29 "pol.y"
{ binary_opcode ("-"); } break;
case 17:
# line 30 "pol.y"
{ unary_opcode ("-"); } break;
case 18:
# line 31 "pol.y"
{ define_variable(yypvt[-2]); } break;
case 19:
# line 32 "pol.y"
{ name (yypvt[-0]); } break;
case 20:
# line 33 "pol.y"
{ mapname (yypvt[-0],'M',0,0); } break;
case 21:
# line 34 "pol.y"
{ mapname (yypvt[-3],'M',yypvt[-1],0); } break;
case 22:
# line 36 "pol.y"
{ mapname (yypvt[-5],'M',yypvt[-3],yypvt[-1]); } break;
case 23:
# line 37 "pol.y"
{ mapname (yypvt[-0],yypvt[-1],0,0); } break;
case 24:
# line 38 "pol.y"
{ mapname (yypvt[-3],yypvt[-4],yypvt[-1],0); } break;
case 25:
# line 40 "pol.y"
{ mapname (yypvt[-5],yypvt[-6],yypvt[-3],yypvt[-1]); } break;
case 26:
# line 41 "pol.y"
{ function (yypvt[-2]); } break;
case 27:
# line 42 "pol.y"
{ function (yypvt[-3]); } break;
case 28:
# line 43 "pol.y"
{ integer (yypvt[-0]); } break;
case 29:
# line 44 "pol.y"
{ floating_point (yypvt[-0]); } break;
case 30:
# line 47 "pol.y"
{ yyval = '@'; } break;
case 31:
# line 48 "pol.y"
{ yyval = '#'; } break;
case 32:
# line 49 "pol.y"
{ yyval = 'r'; } break;
case 33:
# line 50 "pol.y"
{ yyval = 'g'; } break;
case 34:
# line 51 "pol.y"
{ yyval = 'b'; } break;
case 35:
# line 54 "pol.y"
{ another_arg(); } break;
case 36:
# line 55 "pol.y"
{ another_arg(); } break;
case 39:
# line 62 "pol.y"
{ yyval = yypvt[-0]; } break;
case 40:
# line 63 "pol.y"
{ yyval = -yypvt[-0]; } break;
	}
	goto yystack;		/* reset registers in driver code */
}
