
# line 13 "binfer.y"

#include "symtab.h"
#include <stdlib.h>
#include <math.h>
#include "local_proto.h"
    struct symtab table;
    struct symtab *cur_sym;
    struct symtab *cur_att;
    struct names namelist;
    struct names problist;
    char probbuf[256];
    char reclassbuf[256];
    int valno = 1;
    int expected_type;
    int value_type;
    extern int verbose;
    extern int probabilitymaps;
    extern int combinedmap;
    extern int colortable;


# line 35 "binfer.y"
typedef union
#ifdef __cplusplus
	YYSTYPE
#endif
 {
    char *y_sym;
} YYSTYPE;
# define Identifier 257
# define String 258
# define Constant 259
# define LAYER 260
# define CONTEXT 261
# define SUBJECTIVE 262
# define INFERRED 263
# define QUESTION 264
# define THRU 265
# define NO_COMBINED_MAP 266
# define NO_PROBABILITY_MAPS 267
# define COMBINED_MAP 268
# define ASPECT 269
# define GREY 270
# define HISTO 271
# define RAINBOW 272
# define RAMP 273
# define RANDOM 274
# define REDYELLOWGREEN 275
# define WAVE 276

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

# line 510 "binfer.y"



#include <stdio.h>
yytabelem yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 89
# define YYLAST 253
yytabelem yyact[]={

   120,   121,   122,   123,   124,   125,   126,   127,    16,    17,
    18,    19,    20,    21,    22,    23,    11,   135,    81,    27,
    14,    14,     8,     9,    10,    15,   130,    14,   144,   141,
   116,    94,    47,    38,   142,   129,   115,    89,   143,    90,
    72,    68,   113,    48,   114,    44,    79,    71,    88,    73,
   130,    86,   140,   100,   145,   147,   137,   149,    57,    45,
    53,    50,    32,    95,    29,    28,    24,    49,    97,    97,
   148,   106,   102,   147,    97,    92,    37,    96,    83,    41,
    75,    37,    43,    70,    65,    47,   136,    56,    36,   119,
    58,   103,    12,    76,    85,    60,    67,    61,    40,    62,
    52,    33,    54,    39,    93,    91,    25,    69,    63,    42,
    34,    74,    99,    84,    77,    82,    80,    78,    90,    31,
    66,    87,    51,    64,    46,    55,    59,    35,    30,    74,
    26,    13,     7,    98,     6,   104,     5,     4,    80,   107,
     3,   108,    74,   109,     2,   117,   110,   111,    87,   101,
   132,   134,    80,   128,   118,   105,     1,   133,   131,   146,
   112,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   138,     0,   139,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   151,   153,   152,   150,     0,     0,     0,
     0,     0,   144,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    38 };
yytabelem yypact[]={

-10000000,-10000000,-10000000,-10000000,  -244,-10000000,-10000000,  -236,-10000000,-10000000,
  -261,     8,-10000000,  -243,     7,     6,-10000000,-10000000,-10000000,-10000000,
-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,  -242,     4,-10000000,-10000000,
  -224,-10000000,-10000000,  -224,  -224,    -5,    21,     3,-10000000,  -224,
    48,     2,    -5,    21,     0,-10000000,    21,-10000000,-10000000,-10000000,
-10000000,    21,-10000000,-10000000,-10000000,    21,-10000000,-10000000,-10000000,    44,
    48,   -82,-10000000,    43,   -83,  -224,-10000000,    40,  -224,   -83,
  -224,-10000000,  -246,    34,-10000000,  -224,    -7,-10000000,-10000000,    31,
-10000000,  -227,    33,  -224,   -38,    28,-10000000,-10000000,-10000000,  -224,
-10000000,    27,  -224,   -83,   -86,-10000000,  -224,-10000000,-10000000,   -83,
  -229,    21,  -224,  -269,-10000000,-10000000,  -224,-10000000,-10000000,-10000000,
-10000000,-10000000,   -43,  -229,  -229,  -248,-10000000,-10000000,-10000000,    -4,
-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,-10000000,  -248,  -229,-10000000,  -229,   -39,  -230,-10000000,-10000000,
  -231,    -8,    11,-10000000,-10000000,-10000000,    -2,  -231,  -231,   -67,
-10000000,    29,-10000000,    29 };
yytabelem yypgo[]={

     0,    45,    36,    47,    42,    44,   160,    38,    34,   159,
   156,   144,   140,   137,   136,   134,   132,    92,   131,   130,
   128,   127,    59,    88,    43,   126,   123,    49,   115,    63,
   113,   112,    35,   110,   109,    82,   108,   107,    46,   105,
   104,   103,   101,    98,    97,    96,    94,    93,    48,    51,
    91,    89,    86 };
yytabelem yyr1[]={

     0,    11,    10,    12,    13,    13,    15,    15,    15,    15,
    15,    15,    15,    15,    15,    15,    14,    14,    14,    20,
    16,    21,    21,    25,    23,    26,    26,    28,    28,    30,
    27,    31,    31,     6,     6,     6,     6,     5,     4,     4,
     2,    33,    18,    34,    34,    36,    35,    37,    39,    39,
    40,    38,    41,    19,    42,    17,    44,    43,    45,    45,
    47,    47,    46,    46,    50,    49,    51,    51,    51,    51,
    51,    51,    51,    51,    51,    52,     9,     9,     8,     8,
     7,     3,     3,     1,    29,    24,    48,    32,    22 };
yytabelem yyr2[]={

     0,     1,     5,     4,     0,     4,     3,     3,     5,     5,
     5,     5,     5,     5,     5,     5,     4,     6,     8,     1,
    11,     4,     6,     1,    11,     0,     8,     4,     6,     1,
     9,     0,     7,     3,     5,     3,     5,     7,     5,     3,
     3,     1,    11,     4,     6,     1,    11,     8,     4,     6,
     1,     7,     1,    13,     1,    11,     1,    17,     0,     6,
     3,     7,     2,     6,     1,    19,     0,     3,     3,     3,
     3,     3,     3,     3,     3,     7,     5,     7,     3,     7,
     3,     1,     9,     2,     3,     3,     3,     3,     3 };
yytabelem yychk[]={

-10000000,   -10,   -11,   -12,   -13,   -14,   -15,   -16,   266,   267,
   268,   260,   -17,   -18,   263,   261,   269,   270,   271,   272,
   273,   274,   275,   276,    58,   -17,   -19,   262,    58,    58,
   -20,   -17,    58,   -42,   -33,   -21,   -23,    -1,   257,   -41,
   -43,    -1,   -34,   -35,    -1,   -22,   -23,    37,   -24,    46,
    58,   -35,   -22,    58,   -22,   -35,   -24,    58,   -24,   -25,
   -24,   -44,   -24,   -36,   -26,    40,   -22,   -45,   123,   -37,
    40,    -3,   123,   -27,    -1,    40,   -47,    -1,    -3,   -38,
    -1,   264,   -28,    44,   -30,   -46,   -49,    -1,   -48,    44,
   125,   -39,    44,   -40,   258,   -29,    44,    41,   -27,   -31,
    91,   -29,    44,   -50,    -1,   -29,    44,   -38,    -3,   -48,
   -27,    -3,    -6,    -4,    -5,    -2,   259,   -24,   -49,   -51,
   269,   270,   271,   272,   273,   274,   275,   276,   -38,   -32,
    93,    -5,    -2,    -4,    -2,   265,   -52,    60,    -2,    -2,
    91,   259,    -8,    -7,   259,    62,    -9,    44,    59,    59,
    -7,    -8,   -32,    -8 };
yytabelem yydef[]={

     1,    -2,     4,     2,     0,     3,     5,     0,     6,     7,
     0,     0,    16,     0,     0,     0,     8,     9,    10,    11,
    12,    13,    14,    15,    19,    17,     0,     0,    54,    41,
     0,    18,    52,     0,     0,     0,     0,     0,    83,     0,
     0,     0,     0,     0,     0,    20,     0,    88,    21,    85,
    23,     0,    55,    56,    42,     0,    43,    45,    22,    25,
     0,    58,    44,     0,    81,     0,    53,     0,     0,    81,
     0,    24,     0,     0,    29,     0,     0,    60,    46,     0,
    50,     0,     0,     0,    31,     0,    62,    64,    59,     0,
    86,     0,     0,    81,     0,    26,     0,    84,    27,    81,
     0,     0,     0,    66,    61,    47,     0,    48,    51,    82,
    28,    30,     0,    33,    35,    39,    40,    57,    63,     0,
    67,    68,    69,    70,    71,    72,    73,    74,    49,    32,
    87,    34,    38,    36,    39,     0,     0,     0,    38,    37,
     0,     0,     0,    78,    80,    75,     0,     0,     0,     0,
    79,    76,    65,    77 };
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
	"Identifier",	257,
	"String",	258,
	"Constant",	259,
	"LAYER",	260,
	"CONTEXT",	261,
	"SUBJECTIVE",	262,
	"INFERRED",	263,
	"QUESTION",	264,
	"THRU",	265,
	"NO_COMBINED_MAP",	266,
	"NO_PROBABILITY_MAPS",	267,
	"COMBINED_MAP",	268,
	"ASPECT",	269,
	"GREY",	270,
	"HISTO",	271,
	"RAINBOW",	272,
	"RAMP",	273,
	"RANDOM",	274,
	"REDYELLOWGREEN",	275,
	"WAVE",	276,
	"(",	40,
	")",	41,
	"[",	91,
	"]",	93,
	"{",	123,
	"}",	125,
	"<",	60,
	">",	62,
	",",	44,
	".",	46,
	"%",	37,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"Program : /* empty */",
	"Program : Script",
	"Script : Output_Options MainScript",
	"Output_Options : /* empty */",
	"Output_Options : Output_Options Output_Option",
	"Output_Option : NO_COMBINED_MAP",
	"Output_Option : NO_PROBABILITY_MAPS",
	"Output_Option : COMBINED_MAP ASPECT",
	"Output_Option : COMBINED_MAP GREY",
	"Output_Option : COMBINED_MAP HISTO",
	"Output_Option : COMBINED_MAP RAINBOW",
	"Output_Option : COMBINED_MAP RAMP",
	"Output_Option : COMBINED_MAP RANDOM",
	"Output_Option : COMBINED_MAP REDYELLOWGREEN",
	"Output_Option : COMBINED_MAP WAVE",
	"MainScript : Layer_Section Inferred_Section",
	"MainScript : Layer_Section Context_Section Inferred_Section",
	"MainScript : Layer_Section Context_Section Subjective_Section Inferred_Section",
	"Layer_Section : LAYER ':'",
	"Layer_Section : LAYER ':' Layer_Att_Declarations end",
	"Layer_Att_Declarations : Layer_Att_Declaration ep",
	"Layer_Att_Declarations : Layer_Att_Declarations Layer_Att_Declaration ep",
	"Layer_Att_Declaration : Name ':'",
	"Layer_Att_Declaration : Name ':' Layer_Value_List Question_Attachment",
	"Layer_Value_List : /* empty */",
	"Layer_Value_List : '(' Layer_Value Layer_Value_Sublist rp",
	"Layer_Value_Sublist : ',' Layer_Value",
	"Layer_Value_Sublist : Layer_Value_Sublist ',' Layer_Value",
	"Layer_Value : Name",
	"Layer_Value : Name Category_Range Question_Attachment",
	"Category_Range : /* empty */",
	"Category_Range : '[' Reclass_Rule rbr",
	"Reclass_Rule : Input_Cat_List",
	"Reclass_Rule : Input_Cat_List Input_Cat_Range",
	"Reclass_Rule : Input_Cat_Range",
	"Reclass_Rule : Input_Cat_Range Input_Cat_List",
	"Input_Cat_Range : Input_Cat THRU Input_Cat",
	"Input_Cat_List : Input_Cat_List Input_Cat",
	"Input_Cat_List : Input_Cat",
	"Input_Cat : Constant",
	"Context_Section : CONTEXT ':'",
	"Context_Section : CONTEXT ':' Context_Att_Declarations end",
	"Context_Att_Declarations : Context_Att_Declaration ep",
	"Context_Att_Declarations : Context_Att_Declarations Context_Att_Declaration ep",
	"Context_Att_Declaration : Name ':'",
	"Context_Att_Declaration : Name ':' Context_Value_List Question_Attachment",
	"Context_Value_List : '(' Context_Value Context_Value_Sublist rp",
	"Context_Value_Sublist : ',' Context_Value",
	"Context_Value_Sublist : Context_Value_Sublist ',' Context_Value",
	"Context_Value : Name",
	"Context_Value : Name Question_Attachment",
	"Subjective_Section : SUBJECTIVE ':'",
	"Subjective_Section : SUBJECTIVE ':' Context_Att_Declaration ep end",
	"Inferred_Section : INFERRED ':'",
	"Inferred_Section : INFERRED ':' Inferred_Att_Declaration end",
	"Inferred_Att_Declaration : Name ':'",
	"Inferred_Att_Declaration : Name ':' Determinant_List '(' Inferred_Value_List rp ep",
	"Determinant_List : /* empty */",
	"Determinant_List : '{' Att_List rb",
	"Att_List : Name",
	"Att_List : Att_List ',' Name",
	"Inferred_Value_List : Inferred_Value",
	"Inferred_Value_List : Inferred_Value_List ',' Inferred_Value",
	"Inferred_Value : Name",
	"Inferred_Value : Name Optional_Color_Table Prior_Probability '[' Probability_List Conditional_Probability_Table ';' rbr",
	"Optional_Color_Table : /* empty */",
	"Optional_Color_Table : ASPECT",
	"Optional_Color_Table : GREY",
	"Optional_Color_Table : HISTO",
	"Optional_Color_Table : RAINBOW",
	"Optional_Color_Table : RAMP",
	"Optional_Color_Table : RANDOM",
	"Optional_Color_Table : REDYELLOWGREEN",
	"Optional_Color_Table : WAVE",
	"Prior_Probability : '<' Constant '>'",
	"Conditional_Probability_Table : ';' Probability_List",
	"Conditional_Probability_Table : Conditional_Probability_Table ';' Probability_List",
	"Probability_List : Probability",
	"Probability_List : Probability_List ',' Probability",
	"Probability : Constant",
	"Question_Attachment : /* empty */",
	"Question_Attachment : '{' QUESTION String rb",
	"Name : Identifier",
	"rp : ')'",
	"ep : '.'",
	"rb : '}'",
	"rbr : ']'",
	"end : '%'",
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
# line 80 "binfer.y"
{ init(); } break;
case 2:
# line 81 "binfer.y"
{ 
                 check_table(&yyparse_return,verbose);
                 if ( verbose ) fprintf(stderr,"\nAll input parsed.\n");
                 return(-1);
             } break;
case 6:
# line 95 "binfer.y"
{ 
                          combinedmap = 0;
                          fprintf(stderr,"NoCombinedMap option set.\n"); 
                      } break;
case 7:
# line 101 "binfer.y"
{ 
                          probabilitymaps = 0;
                          fprintf(stderr,"NoProbabiltyMaps option set.\n"); 
                      } break;
case 8:
# line 107 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = AspectColors;
                              fprintf(stderr,"Combined map colortable set to aspect colors.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      } break;
case 9:
# line 117 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = GreyScale;
                              fprintf(stderr,"Combined map colortable set to grey scale.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      } break;
case 10:
# line 127 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = HistoGreyScale;
                              fprintf(stderr,"Combined map colortable set to histogram stretched grey scale.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      } break;
case 11:
# line 137 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = Rainbow;
                              fprintf(stderr,"Combined map colortable set to rainbow colors.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      } break;
case 12:
# line 147 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = Ramp;
                              fprintf(stderr,"Combined map colortable set to color ramp.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      } break;
case 13:
# line 157 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = Random;
                              fprintf(stderr,"Combined map colortable set to random colors.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      } break;
case 14:
# line 167 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = RYG;
                              fprintf(stderr,"Combined map colortable set to red yellow green.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      } break;
case 15:
# line 177 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = Wave;
                              fprintf(stderr,"Combined map colortable set to color wave.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      } break;
case 19:
# line 194 "binfer.y"
{ 
                       expected_type = AttributeSymbol;
                       value_type = LayerAttribute; 
                   } break;
case 20:
# line 199 "binfer.y"
{ 
                       if ( verbose ) fprintf(stderr,"\nParsed layers section.\n"); 
                   } break;
case 23:
# line 208 "binfer.y"
{ 
                   cur_sym = s_create(yypvt[-1].y_sym,expected_type,value_type);
                   expected_type = ValueSymbol; 
               } break;
case 24:
# line 214 "binfer.y"
{ 
                   expected_type = AttributeSymbol;
                   if ( yypvt[-0].y_sym != (char *)0 )
                       cur_att->question = strsave(yypvt[-0].y_sym); 
                   valno = 1;
               } break;
case 29:
# line 230 "binfer.y"
{ 
               cur_sym = s_create(yypvt[-0].y_sym,expected_type,value_type); 
           } break;
case 30:
# line 234 "binfer.y"
{ 
               if ( yypvt[-0].y_sym != (char *)0 )
                   cur_sym->question = strsave(yypvt[-0].y_sym); 
           } break;
case 32:
# line 243 "binfer.y"
{ 
               cur_sym->element.val->desc.layer->cat_num = (CELL)valno;
            
               sprintf(cur_sym->reclass,"%s = %d %s",strsave(yypvt[-1].y_sym),valno++,
                       cur_sym->name);
           } break;
case 33:
# line 251 "binfer.y"
{ yyval.y_sym = strsave(yypvt[-0].y_sym); } break;
case 34:
# line 252 "binfer.y"
{ sprintf(reclassbuf,"%s %s",yypvt[-1].y_sym,yypvt[-0].y_sym);
                                       yyval.y_sym = strsave(reclassbuf); } break;
case 35:
# line 254 "binfer.y"
{ yyval.y_sym = strsave(yypvt[-0].y_sym); } break;
case 36:
# line 255 "binfer.y"
{ sprintf(reclassbuf,"%s %s",yypvt[-1].y_sym,yypvt[-0].y_sym);
                                       yyval.y_sym = strsave(reclassbuf); } break;
case 37:
# line 260 "binfer.y"
{ sprintf(reclassbuf,"%s thru %s",yypvt[-2].y_sym,yypvt[-0].y_sym);
                                 yyval.y_sym = strsave(reclassbuf); } break;
case 38:
# line 264 "binfer.y"
{ sprintf(reclassbuf,"%s %s",yypvt[-1].y_sym,yypvt[-0].y_sym);
                                 yyval.y_sym = strsave(reclassbuf); } break;
case 39:
# line 266 "binfer.y"
{ yyval.y_sym = strsave(yypvt[-0].y_sym); } break;
case 40:
# line 269 "binfer.y"
{ yyval.y_sym = strsave(yypvt[-0].y_sym);} break;
case 41:
# line 273 "binfer.y"
{ 
                      expected_type = AttributeSymbol;
                      value_type = ContextAttribute; 
                  } break;
case 42:
# line 278 "binfer.y"
{ 
               if ( verbose ) fprintf(stderr,"\nParsed context section.\n"); 
           } break;
case 45:
# line 287 "binfer.y"
{ 
                    cur_att = cur_sym = s_create(yypvt[-1].y_sym,expected_type,value_type);
                    expected_type = ValueSymbol; 
               } break;
case 46:
# line 292 "binfer.y"
{ 
                   expected_type = AttributeSymbol;
                   if ( yypvt[-0].y_sym != (char *)0 )
                       cur_att->question = strsave(yypvt[-0].y_sym); 
               } break;
case 50:
# line 307 "binfer.y"
{ 
               cur_sym = s_create(yypvt[-0].y_sym,expected_type,value_type); 
           } break;
case 51:
# line 311 "binfer.y"
{ 
               if ( yypvt[-0].y_sym != (char *)0 )
                   cur_sym->question = strsave(yypvt[-0].y_sym); 
           } break;
case 52:
# line 318 "binfer.y"
{ 
                         expected_type = AttributeSymbol;
                         value_type = SubjectiveAttribute; 
                     } break;
case 53:
# line 323 "binfer.y"
{ 
                         if ( verbose ) 
                             fprintf(stderr,"\nParsed subjective section.\n"); 
                     } break;
case 54:
# line 329 "binfer.y"
{ 
                        expected_type = AttributeSymbol;
                        value_type = InferredAttribute; 
                    } break;
case 55:
# line 334 "binfer.y"
{ 
                        if ( verbose ) 
                            fprintf(stderr,"\nParsed inferred section.\n"); 
                    } break;
case 56:
# line 340 "binfer.y"
{ 
                   cur_sym = s_create(yypvt[-1].y_sym,expected_type,value_type);
                   expected_type = ValueSymbol; 
               } break;
case 57:
# line 345 "binfer.y"
{ 
                   expected_type = AttributeSymbol; 
               } break;
case 60:
# line 355 "binfer.y"
{ 
               if (!add_name(yypvt[-0].y_sym)) yyerror("Name not stored"); 
           } break;
case 61:
# line 359 "binfer.y"
{ 
               if (!add_name(yypvt[-0].y_sym)) yyerror("Name not stored"); 
           } break;
case 64:
# line 369 "binfer.y"
{ 
               cur_sym = s_create(yypvt[-0].y_sym,expected_type,value_type); 
           } break;
case 65:
# line 374 "binfer.y"
{
                sprintf(probbuf,"%s%s;",yypvt[-3].y_sym,yypvt[-2].y_sym);
                if (!add_prob_list(probbuf)) yyerror("Problist not stored");
            } break;
case 67:
# line 382 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = AspectColors;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           } break;
case 68:
# line 392 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = GreyScale;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           } break;
case 69:
# line 402 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = HistoGreyScale;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           } break;
case 70:
# line 412 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = Rainbow;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           } break;
case 71:
# line 422 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = Ramp;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           } break;
case 72:
# line 432 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = Random;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           } break;
case 73:
# line 442 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = RYG;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           } break;
case 74:
# line 452 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = Wave;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           } break;
case 75:
# line 465 "binfer.y"
{ 
                cur_sym->element.val->desc.infr->prior_prob = atof(yypvt[-1].y_sym); 
            } break;
case 76:
# line 472 "binfer.y"
{ 
                sprintf(probbuf,";%s",yypvt[-0].y_sym); 
                yyval.y_sym = strsave(probbuf);
            } break;
case 77:
# line 477 "binfer.y"
{ 
                sprintf(probbuf,"%s;%s",yypvt[-2].y_sym,yypvt[-0].y_sym);
                yyval.y_sym = strsave(probbuf);
            } break;
case 78:
# line 484 "binfer.y"
{ 
                yyval.y_sym = strsave(yypvt[-0].y_sym); 
            } break;
case 79:
# line 488 "binfer.y"
{ 
                sprintf(probbuf,"%s,%s",yypvt[-2].y_sym,yypvt[-0].y_sym);
                yyval.y_sym = strsave(probbuf);
            } break;
case 80:
# line 494 "binfer.y"
{ yyval.y_sym = strsave(yypvt[-0].y_sym); } break;
case 81:
# line 497 "binfer.y"
{ yyval.y_sym = (char *)0; } break;
case 82:
# line 498 "binfer.y"
{ yyval.y_sym = strsave(yypvt[-1].y_sym); } break;
case 84:
# line 504 "binfer.y"
{ yyerrok; } break;
case 85:
# line 505 "binfer.y"
{ yyerrok; } break;
case 86:
# line 506 "binfer.y"
{ yyerrok; } break;
case 87:
# line 507 "binfer.y"
{ yyerrok; } break;
case 88:
# line 508 "binfer.y"
{ yyerrok; } break;
	}
	goto yystack;		/* reset registers in driver code */
}
