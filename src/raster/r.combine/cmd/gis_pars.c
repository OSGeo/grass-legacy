extern char *malloc(), *realloc();

# line 2 "gis_pars.y"
#include "tree.h"
#include "lvw.h"

struct Cell_head *get_cur_win() ;
extern struct Node *yytree ;
extern struct Node *e_expr_list[512] ;
extern FILE *yyin ;

struct Group e_yygroup[256]   ;/* limit of 256 nested group expressions */
int   e_yygrp_i=0      ;/* index into current array              */

# line 14 "gis_pars.y"
typedef union 
{
	int              int_val;
	struct Node     *nod_val;
	char            *str_val;
} YYSTYPE;
# define AND_TKN 257
# define OR_TKN 258
# define NOT_TKN 259
# define GRP_TKN 260
# define CATS_TKN 261
# define EXPR_TKN 262
# define RANGE_TKN 263
# define NAM_TKN 264
# define OVR_TKN 265
# define COV_TKN 266
# define WIN_TKN 267
# define BYE_TKN 268
# define ERA_TKN 269
# define HST_TKN 270
# define HLP_TKN 271
# define NAM_STR 272
# define INUMBER 273
# define LP 274
# define RP 275
# define SEMI 276
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 271 "gis_pars.y"

/* ========================================================================= */
yyerror(message) char *message ;
{
/*
	fprintf (stderr, "parser: %s, try again:\n", message);
*/

}
/* ========================================================================= */
struct Node *
make_node (oper, left, rite, name)
int oper ;
struct Node *left, *rite ;
char *name ;
{
	struct Node *node ;

	node = (struct Node *)falloc (1, sizeof (struct Node),
		"falloc: make_node") ;

	node->oper = oper ;
	node->left = left ;
	node->rite = rite ;
	if (name)
		strcpy(node->name,name) ;
	else
		*node->name = NULL ;

	/* fprintf(stderr, "making node for %s\n", name) ; */

	return (node) ;
}
/* ========================================================================= */
int yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 50
# define YYLAST 135
int yyact[]={

    32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
    42,    43,    44,    45,    30,    31,     9,    11,    76,    51,
    80,    13,    79,    78,    77,    74,    73,    12,    17,    16,
    15,    14,    11,    72,    10,    21,    22,    23,    24,    71,
    25,    68,    18,    19,    20,    11,    67,    51,    81,    27,
    26,    21,    22,    23,    24,    65,    25,    57,    18,    19,
    20,    60,    56,    51,    47,    48,    26,     2,    59,    55,
    49,    46,    29,    28,    66,    54,     8,     7,     6,     5,
     4,     3,     1,     0,     0,     0,     0,     0,     0,    50,
    52,    53,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    58,     0,    61,    62,    63,     0,
    64,     0,     0,     0,     0,     0,     0,    69,    70,     0,
     0,     0,     0,     0,    75 };
int yypact[]={

  -240, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
  -222, -1000,  -199,  -200,  -257, -1000, -1000, -1000,  -201,  -208,
  -202,  -227,  -227,  -227, -1000,  -204,  -213,  -218, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000,  -227,  -205,  -211,  -227,
  -227,  -206,  -227,  -220, -1000,  -229, -1000, -1000,  -234,  -227,
  -227,  -236,  -242,  -249,  -250, -1000,  -255, -1000, -1000,  -251,
  -252, -1000, -1000, -1000, -1000,  -253,  -243, -1000, -1000, -1000,
  -225, -1000 };
int yypgo[]={

     0,    67,    82,    81,    80,    79,    78,    77,    76,    75,
    74 };
int yyr1[]={

     0,     2,     2,     2,     2,     2,     2,     2,     2,     1,
     1,     1,     1,     1,     1,     1,     1,     9,     1,     1,
     1,     1,     3,     3,    10,    10,    10,     4,     6,     7,
     5,     5,     5,     5,     5,     5,     5,     5,     5,     5,
     5,     5,     5,     5,     5,     5,     5,     5,     5,     8 };
int yyr2[]={

     0,     3,     3,     3,     3,     3,     3,     3,     3,    11,
    13,    13,    11,    11,    11,    11,     9,     1,    13,     9,
     7,     3,     5,     3,     1,     5,     9,     5,     3,     3,
     7,     3,     5,     5,     5,     5,     5,     5,     5,     5,
     5,     5,     5,     5,     5,     5,     5,     5,     5,     3 };
int yychk[]={

 -1000,    -2,    -1,    -3,    -4,    -5,    -6,    -7,    -8,   256,
   274,   272,   267,   261,   271,   270,   269,   268,   264,   265,
   266,   257,   258,   259,   260,   262,   272,   271,   272,   272,
   271,   272,   257,   258,   259,   260,   261,   262,   263,   264,
   265,   266,   267,   268,   269,   270,   272,   272,   273,   272,
    -1,   274,    -1,    -1,    -9,   273,   275,   275,    -1,   273,
   272,    -1,    -1,    -1,    -1,   275,   -10,   275,   275,    -1,
    -1,   275,   275,   275,   275,    -1,   273,   275,   275,   275,
   263,   273 };
int yydef[]={

     0,    -2,     1,     2,     3,     4,     5,     6,     7,     8,
     0,    21,    23,     0,    31,    28,    29,    49,     0,     0,
     0,     0,     0,     0,    17,     0,     0,     0,    22,    27,
    32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
    42,    43,    44,    45,    46,    47,     0,     0,     0,     0,
     0,     0,     0,     0,    24,     0,    20,    30,     0,     0,
    21,     0,     0,     0,     0,    16,     0,    19,     9,     0,
     0,    12,    13,    14,    15,     0,    25,    10,    11,    18,
     0,    26 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"AND_TKN",	257,
	"OR_TKN",	258,
	"NOT_TKN",	259,
	"GRP_TKN",	260,
	"CATS_TKN",	261,
	"EXPR_TKN",	262,
	"RANGE_TKN",	263,
	"NAM_TKN",	264,
	"OVR_TKN",	265,
	"COV_TKN",	266,
	"WIN_TKN",	267,
	"BYE_TKN",	268,
	"ERA_TKN",	269,
	"HST_TKN",	270,
	"HLP_TKN",	271,
	"NAM_STR",	272,
	"INUMBER",	273,
	"LP",	274,
	"RP",	275,
	"SEMI",	276,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"map_cmnd : map_expr",
	"map_cmnd : win_expr",
	"map_cmnd : cats_expr",
	"map_cmnd : help_expr",
	"map_cmnd : hist_expr",
	"map_cmnd : eras_expr",
	"map_cmnd : exit_expr",
	"map_cmnd : error",
	"map_expr : LP NAM_TKN NAM_STR map_expr RP",
	"map_expr : LP OVR_TKN NAM_STR INUMBER map_expr RP",
	"map_expr : LP OVR_TKN INUMBER NAM_STR map_expr RP",
	"map_expr : LP OVR_TKN INUMBER map_expr RP",
	"map_expr : LP COV_TKN NAM_STR map_expr RP",
	"map_expr : LP AND_TKN map_expr map_expr RP",
	"map_expr : LP OR_TKN map_expr map_expr RP",
	"map_expr : LP NOT_TKN map_expr RP",
	"map_expr : LP GRP_TKN",
	"map_expr : LP GRP_TKN grp_expr map_expr RP",
	"map_expr : LP EXPR_TKN INUMBER RP",
	"map_expr : LP NAM_STR RP",
	"map_expr : NAM_STR",
	"win_expr : WIN_TKN NAM_STR",
	"win_expr : WIN_TKN",
	"grp_expr : /* empty */",
	"grp_expr : grp_expr INUMBER",
	"grp_expr : grp_expr INUMBER RANGE_TKN INUMBER",
	"cats_expr : CATS_TKN NAM_STR",
	"hist_expr : HST_TKN",
	"eras_expr : ERA_TKN",
	"help_expr : LP HLP_TKN RP",
	"help_expr : HLP_TKN",
	"help_expr : HLP_TKN HLP_TKN",
	"help_expr : HLP_TKN NAM_STR",
	"help_expr : HLP_TKN AND_TKN",
	"help_expr : HLP_TKN OR_TKN",
	"help_expr : HLP_TKN NOT_TKN",
	"help_expr : HLP_TKN GRP_TKN",
	"help_expr : HLP_TKN CATS_TKN",
	"help_expr : HLP_TKN EXPR_TKN",
	"help_expr : HLP_TKN RANGE_TKN",
	"help_expr : HLP_TKN NAM_TKN",
	"help_expr : HLP_TKN OVR_TKN",
	"help_expr : HLP_TKN COV_TKN",
	"help_expr : HLP_TKN WIN_TKN",
	"help_expr : HLP_TKN BYE_TKN",
	"help_expr : HLP_TKN ERA_TKN",
	"help_expr : HLP_TKN HST_TKN",
	"help_expr : HLP_TKN HLP_TKN",
	"exit_expr : BYE_TKN",
};
#endif /* YYDEBUG */
#line 1 "/usr/lib/yaccpar"
/*	@(#)yaccpar 1.10 89/04/04 SMI; from S5R3 1.10	*/

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	{ free(yys); free(yyv); return(0); }
#define YYABORT		{ free(yys); free(yyv); return(1); }
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
** static variables used by the parser
*/
static YYSTYPE *yyv;			/* value stack */
static int *yys;			/* state stack */

static YYSTYPE *yypv;			/* top of value stack */
static int *yyps;			/* top of state stack */

static int yystate;			/* current state */
static int yytmp;			/* extra var (lasts between blocks) */

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
	unsigned yymaxdepth = YYMAXDEPTH;

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yyv = (YYSTYPE*)malloc(yymaxdepth*sizeof(YYSTYPE));
	yys = (int*)malloc(yymaxdepth*sizeof(int));
	if (!yyv || !yys)
	{
		yyerror( "out of memory" );
		return(1);
	}
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

			(void)printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			/*
			** reallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			yymaxdepth += YYMAXDEPTH;
			yyv = (YYSTYPE*)realloc((char*)yyv,
				yymaxdepth * sizeof(YYSTYPE));
			yys = (int*)realloc((char*)yys,
				yymaxdepth * sizeof(int));
			if (!yyv || !yys)
			{
				yyerror( "yacc stack overflow" );
				return(1);
			}
			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
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

			(void)printf( "Received token " );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
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

				(void)printf( "Received token " );
				if ( yychar == 0 )
					(void)printf( "end-of-file\n" );
				else if ( yychar < 0 )
					(void)printf( "-none-\n" );
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
					(void)printf( "%s\n", yytoks[yy_i].t_name );
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
						(void)printf( _POP_, *yy_ps,
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

					(void)printf( "Error recovery discards " );
					if ( yychar == 0 )
						(void)printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						(void)printf( "token -none-\n" );
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
						(void)printf( "token %s\n",
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
			(void)printf( "Reduce by (%d) \"%s\"\n",
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
# line 46 "gis_pars.y"
{
			yytree = yypvt[-0].nod_val ;
			return(MAP_EXPR) ;
		} break;
case 2:
# line 51 "gis_pars.y"
{
			return(WIN_EXPR) ;
		} break;
case 3:
# line 55 "gis_pars.y"
{
			return(CATS_EXPR) ;
		} break;
case 4:
# line 59 "gis_pars.y"
{
			return(HELP_EXPR) ;
		} break;
case 5:
# line 63 "gis_pars.y"
{
			return(HIST_EXPR) ;
		} break;
case 6:
# line 67 "gis_pars.y"
{
			return(ERAS_EXPR) ;
		} break;
case 7:
# line 71 "gis_pars.y"
{
			return(EXIT_EXPR) ;
		} break;
case 8:
# line 75 "gis_pars.y"
{
			return(ERR_EXPR) ;
		} break;
case 9:
# line 80 "gis_pars.y"
{
			yyval.nod_val = make_node(NAM_OPR,     yypvt[-1].nod_val, NUL_EXP,      yypvt[-2].str_val) ;
		} break;
case 10:
# line 84 "gis_pars.y"
{
			yyval.nod_val = make_node(OVR_OPR+yypvt[-2].int_val,     yypvt[-1].nod_val, NUL_EXP,      yypvt[-3].str_val) ;
		} break;
case 11:
# line 88 "gis_pars.y"
{
			yyval.nod_val = make_node(OVR_OPR+yypvt[-3].int_val,     yypvt[-1].nod_val, NUL_EXP,      yypvt[-2].str_val) ;
		} break;
case 12:
# line 92 "gis_pars.y"
{
			yyval.nod_val = make_node(OVR_OPR+yypvt[-2].int_val,     yypvt[-1].nod_val, NUL_EXP,    NULL) ;
		} break;
case 13:
# line 96 "gis_pars.y"
{
			yyval.nod_val = make_node(COV_OPR,     yypvt[-1].nod_val, NUL_EXP,      yypvt[-2].str_val) ;
		} break;
case 14:
# line 100 "gis_pars.y"
{
			yyval.nod_val = make_node(AND_OPR,      yypvt[-2].nod_val,      yypvt[-1].nod_val,   "and") ;
		} break;
case 15:
# line 104 "gis_pars.y"
{
			yyval.nod_val = make_node( OR_OPR,      yypvt[-2].nod_val,      yypvt[-1].nod_val,    "or") ;
		} break;
case 16:
# line 108 "gis_pars.y"
{
			yyval.nod_val = make_node(GRP_OPR,      yypvt[-1].nod_val, NUL_EXP, "group") ;
			init_group (&yyval.nod_val->group);
			mark_group (&yyval.nod_val->group, 0, 0);
		} break;
case 17:
# line 114 "gis_pars.y"
{
			/* GRAB CONTROL EARLY */
			/* multiple group arrays necessary for nested expr */
			init_group(&e_yygroup[e_yygrp_i]);

			/* set index to next array for nested expression   */
			e_yygrp_i++ ;
		} break;
case 18:
# line 123 "gis_pars.y"
{
			yyval.nod_val = make_node(GRP_OPR,      yypvt[-1].nod_val, NUL_EXP, "group") ;

			/* set index back for this array */
			e_yygrp_i-- ;

			/* set the 'which group' table,min,max in the node itself */
			yyval.nod_val->group.max   = e_yygroup[e_yygrp_i].max;
			yyval.nod_val->group.min   = e_yygroup[e_yygrp_i].min;
			yyval.nod_val->group.table = e_yygroup[e_yygrp_i].table;
		} break;
case 19:
# line 135 "gis_pars.y"
{
			yyval.nod_val = e_expr_list[yypvt[-1].int_val - 1] ;
		} break;
case 20:
# line 139 "gis_pars.y"
{
			yyval.nod_val = make_node(LEAF_OPR, NUL_EXP, NUL_EXP,      yypvt[-1].str_val) ;
		} break;
case 21:
# line 143 "gis_pars.y"
{
			yyval.nod_val = make_node(LEAF_OPR, NUL_EXP, NUL_EXP,      yypvt[-0].str_val) ;
		} break;
case 22:
# line 149 "gis_pars.y"
{
			if(get_win(yypvt[-0].str_val) == 0)
				yyerror("window error (map name doesn't exist)");
		} break;
case 23:
# line 154 "gis_pars.y"
{
			write_window( get_cur_win()) ;
		} break;
case 24:
# line 160 "gis_pars.y"
{
		} break;
case 25:
# line 163 "gis_pars.y"
{
			mark_group (&e_yygroup[e_yygrp_i-1], yypvt[-0].int_val, yypvt[-0].int_val);
		} break;
case 26:
# line 167 "gis_pars.y"
{
			mark_group (&e_yygroup[e_yygrp_i-1], yypvt[-2].int_val, yypvt[-0].int_val);
		} break;
case 27:
# line 173 "gis_pars.y"
{
			if(get_cats(yypvt[-0].str_val) == 0)
				yyerror("categories error");
		} break;
case 28:
# line 180 "gis_pars.y"
{
		} break;
case 29:
# line 185 "gis_pars.y"
{
		} break;
case 30:
# line 190 "gis_pars.y"
{
			G_gishelp("COMBINE",NULL) ;
		} break;
case 31:
# line 194 "gis_pars.y"
{
			G_gishelp("COMBINE",NULL) ;
		} break;
case 32:
# line 198 "gis_pars.y"
{
			G_gishelp("COMBINE",NULL) ;
		} break;
case 33:
# line 202 "gis_pars.y"
{
			G_gishelp("COMBINE",yypvt[-0].str_val) ;
		} break;
case 34:
# line 206 "gis_pars.y"
{
			G_gishelp("COMBINE","AND") ;
		} break;
case 35:
# line 210 "gis_pars.y"
{
			G_gishelp("COMBINE","OR") ;
		} break;
case 36:
# line 214 "gis_pars.y"
{
			G_gishelp("COMBINE","NOT") ;
		} break;
case 37:
# line 218 "gis_pars.y"
{
			G_gishelp("COMBINE","GRP") ;
		} break;
case 38:
# line 222 "gis_pars.y"
{
			G_gishelp("COMBINE","CATS") ;
		} break;
case 39:
# line 226 "gis_pars.y"
{
			G_gishelp("COMBINE","EXPR") ;
		} break;
case 40:
# line 230 "gis_pars.y"
{
			G_gishelp("COMBINE","RANGE") ;
		} break;
case 41:
# line 234 "gis_pars.y"
{
			G_gishelp("COMBINE","NAM") ;
		} break;
case 42:
# line 238 "gis_pars.y"
{
			G_gishelp("COMBINE","OVR") ;
		} break;
case 43:
# line 242 "gis_pars.y"
{
			G_gishelp("COMBINE","COV") ;
		} break;
case 44:
# line 246 "gis_pars.y"
{
			G_gishelp("COMBINE","WIN") ;
		} break;
case 45:
# line 250 "gis_pars.y"
{
			G_gishelp("COMBINE","BYE") ;
		} break;
case 46:
# line 254 "gis_pars.y"
{
			G_gishelp("COMBINE","ERA") ;
		} break;
case 47:
# line 258 "gis_pars.y"
{
			G_gishelp("COMBINE","HST") ;
		} break;
case 48:
# line 262 "gis_pars.y"
{
			G_gishelp("COMBINE","HLP") ;
		} break;
case 49:
# line 268 "gis_pars.y"
{
		} break;
	}
	goto yystack;		/* reset registers in driver code */
}
