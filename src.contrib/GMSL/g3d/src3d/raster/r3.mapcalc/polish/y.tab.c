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

# line 11 "pol.y"
#include <stdlib.h>
#include <string.h>
#include "local_proto.h"

static int nstored = 0;
static char **storage = 0;

#include <malloc.h>
#include <memory.h>
#include <unistd.h>
#include <values.h>

#ifdef __cplusplus
extern "C" {
#endif
extern char *gettxt(const char *, const char *);
#if !defined(yylex) && !defined(__my_yylex)
	extern int yylex(void);
#endif

#ifdef __cplusplus
}
#endif

#if (defined(__cplusplus) || defined(_XOPEN_SOURCE)) && !defined(yyerror) && !defined(__my_yyerror)
	void yyerror(const char *);
#endif
int yyparse(void);
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval;
YYSTYPE yyval;
typedef int yytabelem;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#if YYMAXDEPTH > 0
int yy_yys[YYMAXDEPTH], *yys = yy_yys;
YYSTYPE yy_yyv[YYMAXDEPTH], *yyv = yy_yyv;
#else	/* user does initial allocation */
int *yys;
YYSTYPE *yyv;
#endif
static int yymaxdepth = YYMAXDEPTH;
# define YYERRCODE 256

# line 85 "pol.y"

#include "lex.yy.c"

int yywrap()
{
    return 1;
}

int yyerror(char *s)
{
    printf ("??\n");
    return 0;
}

int store (char *s)
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

int begin_function(void)
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
    return 0;
}
int another_arg (void)
{
    nargs[function_level-1]++;
    return 0;
}
int function (int n)
{
    function_level--;
    printf ("F%s %d\n", storage[n], nargs[function_level]);
    return 0;
}

int name(int n)
{
    int i;

    if (i = find_variable(n))
	printf ("v%d\n", i-1);
    else
	mapname (n,'M',0,0,0);
    return 0;
}

int mapname (int n,char code, int row,int col, int depth)
{
    printf ("M%c %d %d %d %s\n", code, row, col, depth,  storage[n]);
    return 0;
}

int integer (int n)
{
    printf ("I%d\n",n);
    return 0;
}
int floating_point (int n)
{
    printf ("D%s\n",storage[n]);
    return 0;
}
int unary_opcode (char *s)
{
    printf ("1%s\n",s);
    return 0;
}
int binary_opcode (char *s)
{
    printf ("2%s\n",s);
    return 0;
}
int compare (char *s)
{
    printf ("C%s\n",s);
    return 0;
}
int logical (char *s)
{
    printf ("L%s\n",s);
    return 0;
}

int assign (int n)
{
    printf ("=%s\n", storage[n]);
    return 0;
}

static int *vars ;
static int nvars = 0;

int find_variable (int n)
{
    int i;

    for (i = 0; i < nvars; i++)
	if (vars[i] == n)
	    return(i+1);
    return 0;
}

int define_variable (int n)
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


yytabelem yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 9,
	91, 34,
	-2, 20,
	};
# define YYNPROD 45
# define YYLAST 293
yytabelem yyact[]={

    33,    21,    63,    70,    58,    31,    34,    60,    35,    42,
    32,    33,    33,     3,     2,    83,    31,    31,    34,    33,
    35,    32,    32,    79,    31,    34,    30,    35,    33,    32,
    76,    69,    86,    31,    34,    84,    35,    64,    32,    40,
    33,    38,    39,    11,    39,    31,    34,    43,    35,     4,
    32,    61,    72,     7,    65,    73,    41,    30,     8,     5,
     7,    66,    12,     1,    82,     8,     0,     0,    30,    30,
     0,     0,    78,     0,     0,     0,    30,    16,     6,    75,
    68,     0,     0,     0,    16,    30,    36,    37,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    30,     0,     0,
     0,    44,    45,    46,    47,    48,    49,    50,    51,    52,
    53,    54,    55,    56,    57,     0,    71,    59,     0,     0,
     0,    74,    67,     0,     0,     0,     0,     0,    80,     0,
     0,    81,     0,     0,     0,    85,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    77,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    62,     0,     0,     0,
     0,     0,     0,     0,     0,    24,    25,    28,    26,    27,
    22,    23,     0,     0,     0,     0,    29,    24,    25,    28,
    26,    27,    22,    23,     0,     0,     0,     0,    29,     0,
     0,     0,     0,    24,    25,    28,    26,    27,    22,    23,
     0,     0,     0,     0,    29,    24,    25,    28,    26,    27,
     9,    10,    14,    15,    13,     0,    29,     9,    10,    14,
    15,    13,    17,    18,    20,    19,     0,     0,     0,    17,
    18,    20,    19 };
yytabelem yypact[]={

  -243,-10000000,   -12,    49,    20,-10000000,    -9,    20,    20,   -20,
-10000000,   -52,  -248,     7,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,-10000000,    20,    20,    20,    20,    20,    20,    20,    20,
    20,    20,    20,    20,    20,    20,   -37,-10000000,    20,  -250,
   -43,   -54,   -22,    13,     3,     3,   -18,   -18,   -18,   -18,
   -18,   -18,   -68,   -68,   -68,   -68,   -26,   -26,-10000000,   -25,
-10000000,   -13,-10000000,  -256,   -43,-10000000,    11,   -25,-10000000,   -43,
-10000000,   -14,-10000000,    20,   -21,-10000000,   -43,   -25,-10000000,   -43,
   -29,   -58,-10000000,   -43,-10000000,   -61,-10000000 };
yytabelem yypgo[]={

     0,    63,    78,    43,    51,    62,    61 };
yytabelem yyr1[]={

     0,     1,     1,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     3,     3,     5,     5,     5,     5,
     5,     6,     6,     4,     4 };
yytabelem yyr2[]={

     0,     9,     5,     6,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     5,     7,
     3,     3,     9,    13,    17,     3,     5,    11,    15,    19,
     7,     9,     3,     3,     2,     7,     3,     3,     3,     3,
     3,     3,     7,     3,     5 };
yytabelem yychk[]={

-10000000,    -1,   257,   256,    61,    10,    -2,    40,    45,   257,
   258,    -3,    -5,   261,   259,   260,    64,   269,   270,   272,
   271,    10,   267,   268,   262,   263,   265,   266,   264,   273,
    94,    42,    47,    37,    43,    45,    -2,    -2,    61,    64,
    91,    -3,   257,    40,    -2,    -2,    -2,    -2,    -2,    -2,
    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    41,    -2,
   257,    -4,   259,    45,    91,    41,    -6,    -2,    93,    44,
   259,    -4,    41,    44,    -4,    93,    44,    -2,    93,    44,
    -4,    -4,    93,    44,    93,    -4,    93 };
yytabelem yydef[]={

     0,    -2,     0,     0,     0,     2,     0,     0,     0,    -2,
    21,    25,     0,     0,    32,    33,    36,    37,    38,    39,
    40,     1,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    18,     0,     0,
     0,    26,    34,     0,     4,     5,     6,     7,     8,     9,
    10,    11,    12,    13,    14,    15,    16,    17,     3,    19,
    35,     0,    43,     0,     0,    30,     0,    41,    22,     0,
    44,     0,    31,     0,     0,    27,     0,    42,    23,     0,
     0,     0,    28,     0,    24,     0,    29 };
typedef struct
#ifdef __cplusplus
	yytoktype
#endif
{ char *t_name; int t_val; } yytoktype;
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
	"^",	94,
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
	"exp : exp '^' exp",
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
	"exp : map '[' index ',' index ',' index ']'",
	"exp : map",
	"exp : mapmod map",
	"exp : mapmod map '[' index ']'",
	"exp : mapmod map '[' index ',' index ']'",
	"exp : mapmod map '[' index ',' index ',' index ']'",
	"exp : FUNCTION '(' ')'",
	"exp : FUNCTION '(' exp_list ')'",
	"exp : INTEGER",
	"exp : FLOAT",
	"map : NAME",
	"map : NAME '@' NAME",
	"mapmod : '@'",
	"mapmod : COLOR_GRAY",
	"mapmod : COLOR_RED",
	"mapmod : COLOR_GREEN",
	"mapmod : COLOR_BLUE",
	"exp_list : exp",
	"exp_list : exp_list ',' exp",
	"index : INTEGER",
	"index : '-' INTEGER",
};
#endif /* YYDEBUG */
/* 
 *	Copyright 1987 Silicon Graphics, Inc. - All Rights Reserved
 */

/* #ident	"@(#)yacc:yaccpar	1.10" */
#ident	"$Revision$"

/*
** Skeleton parser driver for yacc output
*/
#include "stddef.h"

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	return(0)
#define YYABORT		return(1)
#ifdef __cplusplus
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( gettxt("uxlibc:78", "syntax error - cannot backup") );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#else
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( gettxt("uxlibc:78", "Syntax error - cannot backup") );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#endif
#define YYRECOVERING()	(!!yyerrflag)
#define YYNEW(type)	malloc((size_t)(sizeof(type) * yynewmax))
#define YYCOPY(to, from, type) \
	(type *) memcpy(to, (char *) from, (size_t)(yynewmax * sizeof(type)))
#define YYENLARGE( from, type) \
	(type *) realloc((char *) from, (size_t)(yynewmax * sizeof(type)))
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
#define YYFLAG		(-10000000)

/*
** global variables used by the parser
*/
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
#if defined(__STDC__) || defined(__cplusplus)
int yyparse(void)
#else
int yyparse()
#endif
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

#if YYMAXDEPTH <= 0
	if (yymaxdepth <= 0)
	{
		if ((yymaxdepth = YYEXPAND(0)) <= 0)
		{
#ifdef __cplusplus
			yyerror(gettxt("uxlibc:79", "yacc initialization error"));
#else
			yyerror(gettxt("uxlibc:79", "Yacc initialization error"));
#endif
			YYABORT;
		}
	}
#endif

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
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			int yynewmax;
			ptrdiff_t yys_off;

			/* The following pointer-differences are safe, since
			 * yypvt, yy_pv, and yypv all are a multiple of
			 * sizeof(YYSTYPE) bytes from yyv.
			 */
			ptrdiff_t yypvt_off = yypvt - yyv;
			ptrdiff_t yy_pv_off = yy_pv - yyv;
			ptrdiff_t yypv_off = yypv - yyv;

			int *yys_base = yys;
#ifdef YYEXPAND
			yynewmax = YYEXPAND(yymaxdepth);
#else
			yynewmax = 2 * yymaxdepth;	/* double table size */
			if (yymaxdepth == YYMAXDEPTH)	/* first time growth */
			{
				void *newyys = YYNEW(int);
				void *newyyv = YYNEW(YYSTYPE);
				if (newyys != 0 && newyyv != 0)
				{
					yys = YYCOPY(newyys, yys, int);
					yyv = YYCOPY(newyyv, yyv, YYSTYPE);
				}
				else
					yynewmax = 0;	/* failed */
			}
			else				/* not first time */
			{
				yys = YYENLARGE(yys, int);
				yyv = YYENLARGE(yyv, YYSTYPE);
				if (yys == 0 || yyv == 0)
					yynewmax = 0;	/* failed */
			}
#endif
			if (yynewmax <= yymaxdepth)	/* tables not expanded */
			{
#ifdef __cplusplus
				yyerror( gettxt("uxlibc:80", "yacc stack overflow") );
#else
				yyerror( gettxt("uxlibc:80", "Yacc stack overflow") );
#endif
				YYABORT;
			}
			yymaxdepth = yynewmax;

			/* reset pointers into yys */
			yys_off = yys - yys_base;
			yy_ps = yy_ps + yys_off;
			yyps = yyps + yys_off;

			/* reset pointers into yyv */
			yypvt = yyv + yypvt_off;
			yy_pv = yyv + yy_pv_off;
			yypv = yyv + yypv_off;
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
#ifdef __cplusplus
				yyerror( gettxt("uxlibc:81", "syntax error") );
#else
				yyerror( gettxt("uxlibc:81", "Syntax error") );
#endif
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
				/* FALLTHRU */
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
# line 22 "pol.y"
{ assign(yypvt[-3]); return 1; } break;
case 2:
# line 23 "pol.y"
{ return 0; } break;
case 4:
# line 27 "pol.y"
{ logical ("&"); } break;
case 5:
# line 28 "pol.y"
{ logical ("|"); } break;
case 6:
# line 29 "pol.y"
{ compare (">"); } break;
case 7:
# line 30 "pol.y"
{ compare (">="); } break;
case 8:
# line 31 "pol.y"
{ compare ("<"); } break;
case 9:
# line 32 "pol.y"
{ compare ("<="); } break;
case 10:
# line 33 "pol.y"
{ compare ("="); } break;
case 11:
# line 34 "pol.y"
{ compare ("!"); } break;
case 12:
# line 35 "pol.y"
{ binary_opcode ("^"); } break;
case 13:
# line 36 "pol.y"
{ binary_opcode ("*"); } break;
case 14:
# line 37 "pol.y"
{ binary_opcode ("/"); } break;
case 15:
# line 38 "pol.y"
{ binary_opcode ("%"); } break;
case 16:
# line 39 "pol.y"
{ binary_opcode ("+"); } break;
case 17:
# line 40 "pol.y"
{ binary_opcode ("-"); } break;
case 18:
# line 41 "pol.y"
{ unary_opcode ("-"); } break;
case 19:
# line 42 "pol.y"
{ define_variable(yypvt[-2]); } break;
case 20:
# line 43 "pol.y"
{ name (yypvt[-0]); } break;
case 21:
# line 44 "pol.y"
{ mapname (yypvt[-0],'M',0,0,0); } break;
case 22:
# line 45 "pol.y"
{ mapname (yypvt[-3],'M',yypvt[-1],0,0); } break;
case 23:
# line 47 "pol.y"
{ mapname (yypvt[-5],'M',yypvt[-3],yypvt[-1],0); } break;
case 24:
# line 49 "pol.y"
{ mapname (yypvt[-7],'M',yypvt[-5],yypvt[-3],yypvt[-1]); } break;
case 25:
# line 50 "pol.y"
{ mapname (yypvt[-0],'M',0,0,0); } break;
case 26:
# line 51 "pol.y"
{ mapname (yypvt[-0],yypvt[-1],0,0,0); } break;
case 27:
# line 52 "pol.y"
{ mapname (yypvt[-3],yypvt[-4],yypvt[-1],0,0); } break;
case 28:
# line 54 "pol.y"
{ mapname (yypvt[-5],yypvt[-6],yypvt[-3],yypvt[-1],0); } break;
case 29:
# line 56 "pol.y"
{ mapname (yypvt[-7],yypvt[-8],yypvt[-5],yypvt[-3],yypvt[-1]); } break;
case 30:
# line 57 "pol.y"
{ function (yypvt[-2]); } break;
case 31:
# line 58 "pol.y"
{ function (yypvt[-3]); } break;
case 32:
# line 59 "pol.y"
{ integer (yypvt[-0]); } break;
case 33:
# line 60 "pol.y"
{ floating_point (yypvt[-0]); } break;
case 35:
# line 65 "pol.y"
{ char buf[1024];
			    sprintf (buf, "%s@%s", storage[yypvt[-2]], storage[yypvt[-0]]);
			    yyval = store(buf);
			  } break;
case 36:
# line 71 "pol.y"
{ yyval = '@'; } break;
case 37:
# line 72 "pol.y"
{ yyval = '#'; } break;
case 38:
# line 73 "pol.y"
{ yyval = 'r'; } break;
case 39:
# line 74 "pol.y"
{ yyval = 'g'; } break;
case 40:
# line 75 "pol.y"
{ yyval = 'b'; } break;
case 41:
# line 78 "pol.y"
{ another_arg(); } break;
case 42:
# line 79 "pol.y"
{ another_arg(); } break;
case 43:
# line 82 "pol.y"
{ yyval = yypvt[-0]; } break;
case 44:
# line 83 "pol.y"
{ yyval = -yypvt[-0]; } break;
	}
	goto yystack;		/* reset registers in driver code */
}
