extern char *malloc(), *realloc();

# line 2 "gis_pars.y"
#include "stdio.h"
#include "include.h"

# line 6 "gis_pars.y"
typedef union 
{
        long            long_val;
        int              int_val;
        char            *str_val;
} YYSTYPE;
# define ERAS_TKN 257
# define LST_TKN 258
# define COLR_TKN 259
# define PRT_TKN 260
# define ASG_TKN 261
# define ANAL_TKN 262
# define UNCH_TKN 263
# define CHOS_TKN 264
# define REC_TKN 265
# define SAV_TKN 266
# define MAP_TKN 267
# define CATS_TKN 268
# define LINE_TKN 269
# define EXEC_TKN 270
# define QUIT_TKN 271
# define HELP_TKN 272
# define PAREN_TKN 273
# define ADD_TKN 274
# define MULT_TKN 275
# define NAM_STR 276
# define LONG_NUM 277
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 239 "gis_pars.y"

/* ========================================================================= */
yyerror(message) char *message ;
{
        printf ("What?  Type help for help (%s)\n", message);
}
/* ========================================================================= */
int yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 68
# define YYLAST 129
int yyact[]={

    53,    52,    54,    55,    56,    57,    58,    59,    60,    61,
    62,    51,    63,    64,    65,   108,    66,    67,    68,    15,
    28,    16,    21,    17,    18,    70,    20,    19,    23,    22,
    41,    40,    30,    24,    25,    29,    34,    26,    27,    36,
    35,    31,    32,    39,   113,    79,    38,   126,   124,   122,
    33,   120,   114,    78,   127,   125,   123,   117,   121,   115,
   111,   109,    82,    80,   118,    45,   116,   112,   110,    83,
    81,    43,    44,    75,    73,   129,   128,   119,    42,   107,
    76,    74,   106,   105,   104,   103,   102,   101,   100,    99,
    98,    97,    96,    95,    94,    93,    92,    91,    90,    89,
    88,    87,    86,    85,    84,    77,    72,    69,    50,    49,
    48,    47,    46,    71,    37,    14,    13,    12,    11,    10,
     9,     8,     7,     6,     5,     4,     3,     2,     1 };
int yypact[]={

  -237, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000,  -226,  -148,  -230,  -233,
  -245,  -246,  -198,  -204,  -157,  -158,  -159,  -160,  -161,  -258,
 -1000,  -162,  -251,  -155,  -163,  -188,  -189,  -164,  -224,  -206,
  -207,  -165,  -166, -1000,  -167, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000,  -168,  -169,  -170,  -171,  -172,  -173,  -174,  -175,
  -176,  -177,  -178,  -179,  -180,  -181,  -182,  -183,  -184, -1000,
  -185,  -186, -1000,  -187, -1000,  -190, -1000, -1000,  -262, -1000,
 -1000,  -208, -1000,  -209, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,  -225, -1000,
  -210, -1000,  -212, -1000,  -192, -1000,  -218, -1000,  -220, -1000,
 -1000,  -221, -1000,  -222, -1000,  -193, -1000,  -194, -1000, -1000 };
int yypgo[]={

     0,   128,   127,   126,   125,   124,   123,   122,   121,   120,
   119,   118,   117,   116,   115 };
int yyr1[]={

     0,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     2,     2,     2,     2,     7,
     3,     4,     4,     4,     6,     6,     6,     6,     6,     6,
     5,     5,     5,     5,     5,     5,     8,     8,     9,     9,
    10,    12,    12,    12,    12,    12,    12,    12,    11,    13,
    13,    13,    13,    13,    13,    13,    13,    13,    13,    13,
    13,    13,    13,    13,    13,    13,    13,    14 };
int yyr2[]={

     0,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     7,     9,     9,     7,     7,
     7,    11,     7,    13,     7,     9,    11,    13,    15,    17,
     7,     9,    11,    13,    15,    17,     7,     5,     7,     5,
     5,     5,     5,     5,     9,     9,     7,     7,     4,     5,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     2 };
int yychk[]={

 -1000,    -1,    -2,    -3,    -4,    -5,    -6,    -7,    -8,    -9,
   -10,   -11,   -12,   -13,   -14,   256,   258,   260,   261,   264,
   263,   259,   266,   265,   270,   271,   274,   275,   257,   272,
   269,   267,   268,   276,   262,   266,   265,   262,   276,   276,
   276,   276,   276,   269,   276,   269,   269,   269,   269,   269,
   269,   269,   259,   258,   260,   261,   262,   263,   264,   265,
   266,   267,   268,   270,   271,   272,   274,   275,   276,   269,
   276,   268,   269,   262,   269,   262,   269,   269,   277,   269,
   269,   276,   269,   276,   269,   269,   269,   269,   269,   269,
   269,   269,   269,   269,   269,   269,   269,   269,   269,   269,
   269,   269,   269,   269,   269,   269,   269,   269,   277,   269,
   276,   269,   276,   269,   277,   269,   276,   269,   276,   269,
   269,   276,   269,   276,   269,   276,   269,   276,   269,   269 };
int yydef[]={

     0,    -2,     1,     2,     3,     4,     5,     6,     7,     8,
     9,    10,    11,    12,    13,    14,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    67,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    37,     0,    39,    40,    48,    41,    42,
    43,    49,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    15,
     0,     0,    18,     0,    46,     0,    47,    20,     0,    22,
    30,     0,    24,     0,    19,    36,    38,    50,    51,    52,
    53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
    63,    64,    65,    66,    16,    17,    44,    45,     0,    31,
     0,    25,     0,    21,     0,    32,     0,    26,     0,    23,
    33,     0,    27,     0,    34,     0,    28,     0,    35,    29 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"ERAS_TKN",	257,
	"LST_TKN",	258,
	"COLR_TKN",	259,
	"PRT_TKN",	260,
	"ASG_TKN",	261,
	"ANAL_TKN",	262,
	"UNCH_TKN",	263,
	"CHOS_TKN",	264,
	"REC_TKN",	265,
	"SAV_TKN",	266,
	"MAP_TKN",	267,
	"CATS_TKN",	268,
	"LINE_TKN",	269,
	"EXEC_TKN",	270,
	"QUIT_TKN",	271,
	"HELP_TKN",	272,
	"PAREN_TKN",	273,
	"ADD_TKN",	274,
	"MULT_TKN",	275,
	"NAM_STR",	276,
	"LONG_NUM",	277,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"weight_cmnd : list_expr",
	"weight_cmnd : print_expr",
	"weight_cmnd : assign_expr",
	"weight_cmnd : choose_expr",
	"weight_cmnd : unchoose_expr",
	"weight_cmnd : color_expr",
	"weight_cmnd : save_expr",
	"weight_cmnd : recover_expr",
	"weight_cmnd : execute_expr",
	"weight_cmnd : quit_expr",
	"weight_cmnd : misc_expr",
	"weight_cmnd : help_expr",
	"weight_cmnd : null_expr",
	"weight_cmnd : error",
	"list_expr : LST_TKN MAP_TKN LINE_TKN",
	"list_expr : LST_TKN CATS_TKN NAM_STR LINE_TKN",
	"list_expr : LST_TKN NAM_STR CATS_TKN LINE_TKN",
	"list_expr : LST_TKN ANAL_TKN LINE_TKN",
	"color_expr : COLR_TKN NAM_STR LINE_TKN",
	"print_expr : PRT_TKN ANAL_TKN LINE_TKN",
	"assign_expr : ASG_TKN NAM_STR LONG_NUM LONG_NUM LINE_TKN",
	"assign_expr : ASG_TKN NAM_STR LINE_TKN",
	"assign_expr : ASG_TKN NAM_STR LONG_NUM LONG_NUM LONG_NUM LINE_TKN",
	"unchoose_expr : UNCH_TKN NAM_STR LINE_TKN",
	"unchoose_expr : UNCH_TKN NAM_STR NAM_STR LINE_TKN",
	"unchoose_expr : UNCH_TKN NAM_STR NAM_STR NAM_STR LINE_TKN",
	"unchoose_expr : UNCH_TKN NAM_STR NAM_STR NAM_STR NAM_STR LINE_TKN",
	"unchoose_expr : UNCH_TKN NAM_STR NAM_STR NAM_STR NAM_STR NAM_STR LINE_TKN",
	"unchoose_expr : UNCH_TKN NAM_STR NAM_STR NAM_STR NAM_STR NAM_STR NAM_STR LINE_TKN",
	"choose_expr : CHOS_TKN NAM_STR LINE_TKN",
	"choose_expr : CHOS_TKN NAM_STR NAM_STR LINE_TKN",
	"choose_expr : CHOS_TKN NAM_STR NAM_STR NAM_STR LINE_TKN",
	"choose_expr : CHOS_TKN NAM_STR NAM_STR NAM_STR NAM_STR LINE_TKN",
	"choose_expr : CHOS_TKN NAM_STR NAM_STR NAM_STR NAM_STR NAM_STR LINE_TKN",
	"choose_expr : CHOS_TKN NAM_STR NAM_STR NAM_STR NAM_STR NAM_STR NAM_STR LINE_TKN",
	"save_expr : SAV_TKN NAM_STR LINE_TKN",
	"save_expr : SAV_TKN LINE_TKN",
	"recover_expr : REC_TKN NAM_STR LINE_TKN",
	"recover_expr : REC_TKN LINE_TKN",
	"execute_expr : EXEC_TKN LINE_TKN",
	"misc_expr : ADD_TKN LINE_TKN",
	"misc_expr : MULT_TKN LINE_TKN",
	"misc_expr : ERAS_TKN LINE_TKN",
	"misc_expr : LST_TKN SAV_TKN ANAL_TKN LINE_TKN",
	"misc_expr : LST_TKN REC_TKN ANAL_TKN LINE_TKN",
	"misc_expr : LST_TKN SAV_TKN LINE_TKN",
	"misc_expr : LST_TKN REC_TKN LINE_TKN",
	"quit_expr : QUIT_TKN LINE_TKN",
	"help_expr : HELP_TKN LINE_TKN",
	"help_expr : HELP_TKN COLR_TKN LINE_TKN",
	"help_expr : HELP_TKN LST_TKN LINE_TKN",
	"help_expr : HELP_TKN PRT_TKN LINE_TKN",
	"help_expr : HELP_TKN ASG_TKN LINE_TKN",
	"help_expr : HELP_TKN ANAL_TKN LINE_TKN",
	"help_expr : HELP_TKN UNCH_TKN LINE_TKN",
	"help_expr : HELP_TKN CHOS_TKN LINE_TKN",
	"help_expr : HELP_TKN REC_TKN LINE_TKN",
	"help_expr : HELP_TKN SAV_TKN LINE_TKN",
	"help_expr : HELP_TKN MAP_TKN LINE_TKN",
	"help_expr : HELP_TKN CATS_TKN LINE_TKN",
	"help_expr : HELP_TKN EXEC_TKN LINE_TKN",
	"help_expr : HELP_TKN QUIT_TKN LINE_TKN",
	"help_expr : HELP_TKN HELP_TKN LINE_TKN",
	"help_expr : HELP_TKN ADD_TKN LINE_TKN",
	"help_expr : HELP_TKN MULT_TKN LINE_TKN",
	"help_expr : HELP_TKN NAM_STR LINE_TKN",
	"null_expr : LINE_TKN",
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
# line 39 "gis_pars.y"
{ return(LIST_EXPR) ; } break;
case 2:
# line 41 "gis_pars.y"
{ return(PRNT_EXPR) ; } break;
case 3:
# line 43 "gis_pars.y"
{ return(ASSGN_EXPR) ; } break;
case 4:
# line 45 "gis_pars.y"
{ return(CHOOS_EXPR) ; } break;
case 5:
# line 47 "gis_pars.y"
{ return(UNCHOOS_EXPR) ; } break;
case 6:
# line 49 "gis_pars.y"
{ return(COLR_EXPR) ; } break;
case 7:
# line 51 "gis_pars.y"
{ return(SAVE_EXPR) ; } break;
case 8:
# line 53 "gis_pars.y"
{ return(RECOV_EXPR) ; } break;
case 9:
# line 55 "gis_pars.y"
{ return(EXECUT_EXPR) ; } break;
case 10:
# line 57 "gis_pars.y"
{ return(QUIT_EXPR) ; } break;
case 11:
# line 59 "gis_pars.y"
{ return(MISC_EXPR) ; } break;
case 12:
# line 61 "gis_pars.y"
{ return(HELP_EXPR) ; } break;
case 13:
# line 63 "gis_pars.y"
{ return(NULL_EXPR) ; } break;
case 14:
# line 65 "gis_pars.y"
{ return(ERR_EXPR) ; } break;
case 15:
# line 68 "gis_pars.y"
{ list_maps() ; } break;
case 16:
# line 70 "gis_pars.y"
{ list_cats(yypvt[-1].str_val) ; } break;
case 17:
# line 72 "gis_pars.y"
{ list_cats(yypvt[-2].str_val) ; } break;
case 18:
# line 74 "gis_pars.y"
{ list_analysis(0) ; } break;
case 19:
# line 77 "gis_pars.y"
{ select_colors (yypvt[-1].str_val) ; } break;
case 20:
# line 80 "gis_pars.y"
{ list_analysis(1) ; } break;
case 21:
# line 83 "gis_pars.y"
{ assign_single(yypvt[-3].str_val, yypvt[-2].long_val, yypvt[-1].long_val) ; } break;
case 22:
# line 85 "gis_pars.y"
{ ask_weights(yypvt[-1].str_val) ; } break;
case 23:
# line 87 "gis_pars.y"
{ assign_mult(yypvt[-4].str_val, yypvt[-3].long_val, yypvt[-2].long_val, yypvt[-1].long_val) ; } break;
case 24:
# line 90 "gis_pars.y"
{ unchoose_map(yypvt[-1].str_val) ; } break;
case 25:
# line 92 "gis_pars.y"
{
					unchoose_map(yypvt[-2].str_val) ;
					unchoose_map(yypvt[-1].str_val) ;
				} break;
case 26:
# line 97 "gis_pars.y"
{
					unchoose_map(yypvt[-3].str_val) ;
					unchoose_map(yypvt[-2].str_val) ;
					unchoose_map(yypvt[-1].str_val) ;
				} break;
case 27:
# line 103 "gis_pars.y"
{
					unchoose_map(yypvt[-4].str_val) ;
					unchoose_map(yypvt[-3].str_val) ;
					unchoose_map(yypvt[-2].str_val) ;
					unchoose_map(yypvt[-1].str_val) ;
				} break;
case 28:
# line 110 "gis_pars.y"
{
					unchoose_map(yypvt[-5].str_val) ;
					unchoose_map(yypvt[-4].str_val) ;
					unchoose_map(yypvt[-3].str_val) ;
					unchoose_map(yypvt[-2].str_val) ;
					unchoose_map(yypvt[-1].str_val) ;
				} break;
case 29:
# line 118 "gis_pars.y"
{
					unchoose_map(yypvt[-6].str_val) ;
					unchoose_map(yypvt[-5].str_val) ;
					unchoose_map(yypvt[-4].str_val) ;
					unchoose_map(yypvt[-3].str_val) ;
					unchoose_map(yypvt[-2].str_val) ;
					unchoose_map(yypvt[-1].str_val) ;
				} break;
case 30:
# line 128 "gis_pars.y"
{ choose_map(yypvt[-1].str_val) ; } break;
case 31:
# line 130 "gis_pars.y"
{
					choose_map(yypvt[-2].str_val) ;
					choose_map(yypvt[-1].str_val) ;
				} break;
case 32:
# line 135 "gis_pars.y"
{
					choose_map(yypvt[-3].str_val) ;
					choose_map(yypvt[-2].str_val) ;
					choose_map(yypvt[-1].str_val) ;
				} break;
case 33:
# line 141 "gis_pars.y"
{
					choose_map(yypvt[-4].str_val) ;
					choose_map(yypvt[-3].str_val) ;
					choose_map(yypvt[-2].str_val) ;
					choose_map(yypvt[-1].str_val) ;
				} break;
case 34:
# line 148 "gis_pars.y"
{
					choose_map(yypvt[-5].str_val) ;
					choose_map(yypvt[-4].str_val) ;
					choose_map(yypvt[-3].str_val) ;
					choose_map(yypvt[-2].str_val) ;
					choose_map(yypvt[-1].str_val) ;
				} break;
case 35:
# line 156 "gis_pars.y"
{
					choose_map(yypvt[-6].str_val) ;
					choose_map(yypvt[-5].str_val) ;
					choose_map(yypvt[-4].str_val) ;
					choose_map(yypvt[-3].str_val) ;
					choose_map(yypvt[-2].str_val) ;
					choose_map(yypvt[-1].str_val) ;
				} break;
case 36:
# line 166 "gis_pars.y"
{ save(yypvt[-1].str_val) ; } break;
case 37:
# line 168 "gis_pars.y"
{ save(NUL_STR) ; } break;
case 38:
# line 171 "gis_pars.y"
{ recover(yypvt[-1].str_val) ; } break;
case 39:
# line 173 "gis_pars.y"
{ recover(NUL_STR) ; } break;
case 40:
# line 176 "gis_pars.y"
{ execute() ; } break;
case 41:
# line 179 "gis_pars.y"
{ set_to_add() ; } break;
case 42:
# line 181 "gis_pars.y"
{ set_to_mult() ; } break;
case 43:
# line 183 "gis_pars.y"
{ if (at_console())
					    {
						R_color(D_translate_color("black")) ;
						D_erase_window() ;
						R_flush() ;
					    }
					} break;
case 44:
# line 191 "gis_pars.y"
{ system("ls -l") ; } break;
case 45:
# line 193 "gis_pars.y"
{ system("ls -l") ; } break;
case 46:
# line 195 "gis_pars.y"
{ system("ls -l") ; } break;
case 47:
# line 197 "gis_pars.y"
{ system("ls -l") ; } break;
case 49:
# line 202 "gis_pars.y"
{ G_gishelp("WEIGHT", "general") ; } break;
case 50:
# line 204 "gis_pars.y"
{ G_gishelp("WEIGHT", "COLR") ; } break;
case 51:
# line 206 "gis_pars.y"
{ G_gishelp("WEIGHT", "LST") ; } break;
case 52:
# line 208 "gis_pars.y"
{ G_gishelp("WEIGHT", "PRT") ; } break;
case 53:
# line 210 "gis_pars.y"
{ G_gishelp("WEIGHT", "ASG") ; } break;
case 54:
# line 212 "gis_pars.y"
{ G_gishelp("WEIGHT", "ANAL") ; } break;
case 55:
# line 214 "gis_pars.y"
{ G_gishelp("WEIGHT", "UNCH") ; } break;
case 56:
# line 216 "gis_pars.y"
{ G_gishelp("WEIGHT", "CHOS") ; } break;
case 57:
# line 218 "gis_pars.y"
{ G_gishelp("WEIGHT", "REC") ; } break;
case 58:
# line 220 "gis_pars.y"
{ G_gishelp("WEIGHT", "SAV") ; } break;
case 59:
# line 222 "gis_pars.y"
{ G_gishelp("WEIGHT", "MAP") ; } break;
case 60:
# line 224 "gis_pars.y"
{ G_gishelp("WEIGHT", "CATS") ; } break;
case 61:
# line 226 "gis_pars.y"
{ G_gishelp("WEIGHT", "EXEC") ; } break;
case 62:
# line 228 "gis_pars.y"
{ G_gishelp("WEIGHT", "QUIT") ; } break;
case 63:
# line 230 "gis_pars.y"
{ G_gishelp("WEIGHT", "HELP") ; } break;
case 64:
# line 232 "gis_pars.y"
{ G_gishelp("WEIGHT", "ADD_MULT") ; } break;
case 65:
# line 234 "gis_pars.y"
{ G_gishelp("WEIGHT", "ADD_MULT") ; } break;
case 66:
# line 236 "gis_pars.y"
{ G_gishelp("WEIGHT", yypvt[-1].str_val) ; } break;
	}
	goto yystack;		/* reset registers in driver code */
}
