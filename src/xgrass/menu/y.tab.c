extern char *malloc(), *realloc();

# line 5 "xc.grammar.y"

#include "xc.xclip.h"

int dialogLevel = 0;
XCInterfaceData *interface = NULL;
XCInterfaceData *curPtr = NULL;
XCInterfaceData *tempPtr = NULL;
extern XclipGlobalData *zzGlobal;

#define CURFLAG (curPtr->flag)
#define CURPARM (curPtr->parm)

XCInterfaceData *_xc_stack[50];
int _xc_size = 0;
#define PUSHDIALOGPTR(p) ((void)(_xc_stack[_xc_size++] = (p)))
#define POPDIALOGPTR     (_xc_stack[_xc_size] = NULL , _xc_stack[--_xc_size])


# line 26 "xc.grammar.y"
typedef union  {
    char *  cval;
    int ival;
    double dval;
    Boolean bval;
} YYSTYPE;
# define String 257
# define Integer 258
# define Real 259
# define Logical 260
# define Program 261
# define Title 262
# define Description 263
# define Help 264
# define HelpFile 265
# define HelpWidgetRef 266
# define Capture 267
# define ErrorCodes 268
# define CommandString 269
# define Dialog 270
# define Flag 271
# define Parameter 272
# define Key 273
# define Requires 274
# define Precludes 275
# define Optional 276
# define Input 277
# define Multiple 278
# define Type 279
# define TypeEnumerate 280
# define TypeFileName 281
# define TypeDatabaseElement 282
# define TypeCharacter 283
# define TypeInteger 284
# define TypeFloat 285
# define TypeDouble 286
# define TypeLogical 287
# define DatabaseElementRaster 288
# define DatabaseElementAsciiDlg 289
# define DatabaseElementDlg 290
# define DatabaseElementAsciiVector 291
# define DatabaseElementVector 292
# define DatabaseElementSites 293
# define DatabaseElementRegion 294
# define DatabaseElementIcon 295
# define DatabaseElementLabel 296
# define DatabaseElementGroup 297
# define DatabaseElementSubGroup 298
# define StatusOld 299
# define StatusNew 300
# define SelectType 301
# define Default 302
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 1105 "xc.grammar.y"

zzinitParser()
{
interface = NULL;
_xc_size = 0;
curPtr = NULL;
tempPtr = NULL;
}
int yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 30,
	279, 51,
	-2, 46,
-1, 179,
	300, 97,
	-2, 95,
	};
# define YYNPROD 112
# define YYLAST 300
int yyact[]={

    68,    53,    54,   211,   210,   114,    42,   115,    29,    66,
    19,    56,    55,    59,    58,    57,   130,   131,   132,   133,
   134,   135,   136,   137,   138,   139,   140,    17,    15,    16,
   173,   174,    97,    98,    44,    45,    91,    32,    51,    52,
   177,   176,    80,    85,    35,    79,    81,    82,    83,    84,
   145,   146,   147,   148,   149,   150,    24,     4,   215,   214,
   207,   206,   205,   195,   194,   193,   192,   191,   175,   190,
   129,   128,   127,   124,   123,   122,   121,   120,   116,   102,
   101,   100,    95,    94,    93,    92,    89,    88,    65,    60,
    48,    36,    23,    22,    21,     9,    96,   178,   172,    62,
   144,     8,    11,   106,    10,   196,   213,   212,   202,   201,
   200,   199,   198,    20,   179,   169,   168,   167,   166,   165,
   164,   125,   119,   118,   117,   112,   110,   107,   105,   103,
    90,    78,    77,    76,    75,    74,    73,    72,    71,    70,
    64,    63,    61,    46,    40,    37,    28,     6,    50,    31,
   204,   203,    17,    15,    16,    17,    15,    16,   141,   162,
   161,   160,   159,   158,   157,   156,   155,    87,   154,   153,
   152,   113,    99,    86,   111,   109,   104,    41,    33,    49,
    39,    38,    30,    26,   171,   170,   151,   108,    67,    25,
    20,    43,    34,    27,    14,    13,    12,   143,    69,   142,
   126,    47,    18,     7,     5,    53,    54,     3,     2,     1,
     0,     0,     0,     0,     0,    56,    55,    59,    58,    57,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    51,    52,   163,     0,     0,     0,     0,     0,
     0,   180,   181,   182,   183,   184,   185,   186,   187,   188,
   189,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   197,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   208,   209 };
int yypact[]={

 -1000, -1000,  -204, -1000,    89,   -22,  -162, -1000,  -243, -1000,
  -115, -1000, -1000, -1000, -1000,  -163,  -164,  -165,  -206, -1000,
 -1000, -1000, -1000, -1000,    88,  -265,  -226,  -219,  -166,    87,
  -226, -1000,    86,  -273,  -230,    85, -1000,  -167,  -263, -1000,
  -168, -1000,    84,   -24,    83,    82,  -169,  -260, -1000,   -59,
 -1000,    81,    80,    79,    78,    77,    76,    75,    74,    73,
 -1000,  -238,  -243,  -170,  -171, -1000,    72,  -227, -1000, -1000,
  -172,  -173,  -174,  -175,  -239,  -239,  -176,  -177,  -178, -1000,
    71,    70,    69,    69,    68,    67,  -277,  -118, -1000, -1000,
  -179,    66, -1000, -1000, -1000, -1000,    65, -1000, -1000,    64,
 -1000, -1000, -1000,  -180, -1000,  -181, -1000,  -182, -1000, -1000,
  -183, -1000,  -184, -1000,    63, -1000, -1000,  -185,  -186,  -187,
 -1000, -1000, -1000, -1000, -1000,  -272,  -213, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000,  -213, -1000,    62,    61,    60,    59,    58,
    57,  -234,    56,    56,    56,    56,    56,    56,    56,    56,
    56,    56,    56, -1000,  -188,  -190,  -191,  -192,  -193,  -194,
    46,  -234, -1000,    54,    53,    52,    51,    50, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,  -195,  -196,
  -197,  -239,  -239,  -295,  -297, -1000, -1000, -1000,    49,    48,
 -1000, -1000,  -198,  -199, -1000, -1000 };
int yypgo[]={

     0,   209,    96,   208,   207,   204,   203,   202,   201,   200,
   199,   197,   100,   104,   102,   196,   195,   194,   193,   192,
   191,   189,   188,   186,   185,   184,    98,   183,   182,   181,
   179,   149,   178,   177,   176,   103,   175,   174,   173,   171,
   170,    97,   169,   168,   166,   165,   164,   163,   162,   161,
   160,   159,   158,   151,   150,   148 };
int yyr1[]={

     0,     3,     5,     1,     7,     8,     9,     4,    10,    10,
    11,    11,    12,    12,    12,    12,    12,    12,     6,     6,
    13,    13,    14,    14,    14,    18,    17,    19,    19,    20,
    20,    20,    21,    22,    23,    15,    24,    24,    25,    25,
    26,    26,    26,    26,    26,    27,    29,    16,    28,    28,
    31,    32,    31,    33,    33,    33,    33,    33,    33,    33,
    38,    33,    34,    34,    35,    35,    36,    36,    37,    37,
    40,    39,    42,    39,    43,    39,    44,    39,    45,    39,
    46,    39,    47,    39,    48,    39,    49,    39,    50,    39,
    51,    39,    39,    52,    41,    53,    41,    54,    41,    30,
    30,    55,    55,    55,    55,    55,    55,    55,    55,    55,
     2,     2 };
int yyr2[]={

     0,     1,     1,     9,     1,     1,     1,    26,     0,     2,
     4,     2,     7,     7,     7,     7,     7,     7,     0,     6,
     4,     2,     2,     2,     2,     1,    17,     0,     7,     0,
     7,     7,     1,     1,     1,    27,     0,     2,     4,     2,
     7,     7,     7,    11,    11,     1,     1,    15,     4,     2,
     7,     1,     4,     7,    11,     9,     9,     9,     9,     9,
     1,     8,     0,     5,     0,     5,     0,     5,     0,     5,
     1,    10,     1,    10,     1,    10,     1,    10,     1,    10,
     1,    10,     1,    10,     1,    10,     1,    10,     1,    10,
     1,    10,     7,     0,     1,     1,     6,     1,     6,     4,
     2,     7,     7,     7,     7,    11,    11,     7,     7,     7,
     3,     3 };
int yychk[]={

 -1000,    -1,    -3,    -4,   261,    -5,    58,    -6,   123,   257,
   -13,   -14,   -15,   -16,   -17,   271,   272,   270,    -7,   125,
   -14,   257,   257,   257,   262,   -21,   -27,   -18,    58,   273,
   -28,   -31,   263,   -32,   -19,   263,   257,    58,   -29,   -31,
    58,   -33,   279,   -20,   264,   265,    58,    -8,   257,   -30,
   -55,   301,   302,   264,   265,   275,   274,   278,   277,   276,
   257,    58,   123,    58,    58,   257,   269,   -22,    59,   -55,
    58,    58,    58,    58,    58,    58,    58,    58,    58,   283,
   280,   284,   285,   286,   287,   281,   -38,   -13,   257,   257,
    58,   263,   257,   257,   257,   257,    -2,   271,   272,    -2,
   257,   257,   257,    58,   -34,    58,   -35,    58,   -35,   -36,
    58,   -37,    58,   -39,   282,   125,   257,    58,    58,    58,
   257,   257,   257,   257,   257,    58,    -9,   257,   257,   257,
   288,   289,   290,   291,   292,   293,   294,   295,   296,   297,
   298,   -52,   -10,   -11,   -12,   263,   264,   265,   266,   267,
   268,   -23,   -40,   -42,   -43,   -44,   -45,   -46,   -47,   -48,
   -49,   -50,   -51,   -12,    58,    58,    58,    58,    58,    58,
   -24,   -25,   -26,   264,   265,   302,   275,   274,   -41,    58,
   -41,   -41,   -41,   -41,   -41,   -41,   -41,   -41,   -41,   -41,
   257,   257,   257,   257,   257,   257,    59,   -26,    58,    58,
    58,    58,    58,   -53,   -54,   257,   257,   257,    -2,    -2,
   299,   300,    58,    58,   257,   257 };
int yydef[]={

     1,    -2,     0,     2,     0,    18,     0,     3,     0,     4,
     0,    21,    22,    23,    24,     0,     0,     0,     0,    19,
    20,    32,    45,    25,     0,     0,    51,    27,     0,     0,
    -2,    49,     0,     0,    29,     0,     5,     0,     0,    48,
     0,    52,     0,     0,     0,     0,     0,     0,    33,     0,
   100,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    50,    60,     0,     0,     0,    28,     0,     0,    47,    99,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    53,
     0,    62,    64,    64,    66,    68,     0,     0,    30,    31,
     0,     0,   101,   102,   103,   104,     0,   110,   111,     0,
   107,   108,   109,     0,    55,     0,    56,     0,    57,    58,
     0,    59,     0,    61,     0,    26,     6,     0,     0,     0,
    54,    63,    65,    67,    69,    93,     8,    34,   105,   106,
    70,    72,    74,    76,    78,    80,    82,    84,    86,    88,
    90,    92,     7,     9,    11,     0,     0,     0,     0,     0,
     0,    36,    94,    94,    94,    94,    94,    94,    94,    94,
    94,    94,    94,    10,     0,     0,     0,     0,     0,     0,
     0,    37,    39,     0,     0,     0,     0,     0,    71,    -2,
    73,    75,    77,    79,    81,    83,    85,    87,    89,    91,
    12,    13,    14,    15,    16,    17,    35,    38,     0,     0,
     0,     0,     0,     0,     0,    40,    41,    42,     0,     0,
    96,    98,     0,     0,    43,    44 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"String",	257,
	"Integer",	258,
	"Real",	259,
	"Logical",	260,
	"Program",	261,
	"Title",	262,
	"Description",	263,
	"Help",	264,
	"HelpFile",	265,
	"HelpWidgetRef",	266,
	"Capture",	267,
	"ErrorCodes",	268,
	"CommandString",	269,
	"Dialog",	270,
	"Flag",	271,
	"Parameter",	272,
	"Key",	273,
	"Requires",	274,
	"Precludes",	275,
	"Optional",	276,
	"Input",	277,
	"Multiple",	278,
	"Type",	279,
	"TypeEnumerate",	280,
	"TypeFileName",	281,
	"TypeDatabaseElement",	282,
	"TypeCharacter",	283,
	"TypeInteger",	284,
	"TypeFloat",	285,
	"TypeDouble",	286,
	"TypeLogical",	287,
	"DatabaseElementRaster",	288,
	"DatabaseElementAsciiDlg",	289,
	"DatabaseElementDlg",	290,
	"DatabaseElementAsciiVector",	291,
	"DatabaseElementVector",	292,
	"DatabaseElementSites",	293,
	"DatabaseElementRegion",	294,
	"DatabaseElementIcon",	295,
	"DatabaseElementLabel",	296,
	"DatabaseElementGroup",	297,
	"DatabaseElementSubGroup",	298,
	"StatusOld",	299,
	"StatusNew",	300,
	"SelectType",	301,
	"Default",	302,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"Interface : /* empty */",
	"Interface : ProgramHeader",
	"Interface : ProgramHeader MainProgram",
	"ProgramHeader : Program ':' String",
	"ProgramHeader : Program ':' String Title ':' String",
	"ProgramHeader : Program ':' String Title ':' String CommandString ':' String",
	"ProgramHeader : Program ':' String Title ':' String CommandString ':' String OptionalHeaderElementList",
	"OptionalHeaderElementList : /* empty */",
	"OptionalHeaderElementList : HeaderElementList",
	"HeaderElementList : HeaderElementList HeaderElement",
	"HeaderElementList : HeaderElement",
	"HeaderElement : Description ':' String",
	"HeaderElement : Help ':' String",
	"HeaderElement : HelpFile ':' String",
	"HeaderElement : HelpWidgetRef ':' String",
	"HeaderElement : Capture ':' String",
	"HeaderElement : ErrorCodes ':' String",
	"MainProgram : /* empty */",
	"MainProgram : '{' FlagOrParmOrDialogDescriptionList '}'",
	"FlagOrParmOrDialogDescriptionList : FlagOrParmOrDialogDescriptionList FlagOrParmOrDialogDescription",
	"FlagOrParmOrDialogDescriptionList : FlagOrParmOrDialogDescription",
	"FlagOrParmOrDialogDescription : FlagDescription",
	"FlagOrParmOrDialogDescription : ParmDescription",
	"FlagOrParmOrDialogDescription : DialogDescription",
	"DialogDescription : Dialog String",
	"DialogDescription : Dialog String OptionalDescription OptionalDialogElementList '{' FlagOrParmOrDialogDescriptionList '}'",
	"OptionalDescription : /* empty */",
	"OptionalDescription : Description ':' String",
	"OptionalDialogElementList : /* empty */",
	"OptionalDialogElementList : Help ':' String",
	"OptionalDialogElementList : HelpFile ':' String",
	"FlagDescription : Flag String",
	"FlagDescription : Flag String Key ':' String",
	"FlagDescription : Flag String Key ':' String Description ':' String",
	"FlagDescription : Flag String Key ':' String Description ':' String OptFlagDescList ';'",
	"OptFlagDescList : /* empty */",
	"OptFlagDescList : OptionalFlagDescList",
	"OptionalFlagDescList : OptionalFlagDescList OptionalFlagDesc",
	"OptionalFlagDescList : OptionalFlagDesc",
	"OptionalFlagDesc : Help ':' String",
	"OptionalFlagDesc : HelpFile ':' String",
	"OptionalFlagDesc : Default ':' String",
	"OptionalFlagDesc : Precludes ':' FlagOrParmKey ':' String",
	"OptionalFlagDesc : Requires ':' FlagOrParmKey ':' String",
	"ParmDescription : Parameter String",
	"ParmDescription : Parameter String ParmReqList",
	"ParmDescription : Parameter String ParmReqList ParmOptionList ';'",
	"ParmReqList : ParmReqList ParmReq",
	"ParmReqList : ParmReq",
	"ParmReq : Description ':' String",
	"ParmReq : /* empty */",
	"ParmReq : TypeOption",
	"TypeOption : Type ':' TypeCharacter",
	"TypeOption : Type ':' TypeEnumerate ':' String",
	"TypeOption : Type ':' TypeInteger OptIntegerModifier",
	"TypeOption : Type ':' TypeFloat OptDoubleModifier",
	"TypeOption : Type ':' TypeDouble OptDoubleModifier",
	"TypeOption : Type ':' TypeLogical OptLogicalModifier",
	"TypeOption : Type ':' TypeFileName OptFilenameModifier",
	"TypeOption : Type ':'",
	"TypeOption : Type ':' DatabaseElementDesc",
	"OptIntegerModifier : /* empty */",
	"OptIntegerModifier : ':' String",
	"OptDoubleModifier : /* empty */",
	"OptDoubleModifier : ':' String",
	"OptLogicalModifier : /* empty */",
	"OptLogicalModifier : ':' String",
	"OptFilenameModifier : /* empty */",
	"OptFilenameModifier : ':' String",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementRaster",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementRaster OptionalStatus",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementAsciiDlg",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementAsciiDlg OptionalStatus",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementDlg",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementDlg OptionalStatus",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementAsciiVector",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementAsciiVector OptionalStatus",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementVector",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementVector OptionalStatus",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementSites",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementSites OptionalStatus",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementRegion",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementRegion OptionalStatus",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementIcon",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementIcon OptionalStatus",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementLabel",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementLabel OptionalStatus",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementGroup",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementGroup OptionalStatus",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementSubGroup",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementSubGroup OptionalStatus",
	"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementUserDefined",
	"DatabaseElementUserDefined : /* empty */",
	"OptionalStatus : /* empty */",
	"OptionalStatus : ':'",
	"OptionalStatus : ':' StatusOld",
	"OptionalStatus : ':'",
	"OptionalStatus : ':' StatusNew",
	"ParmOptionList : ParmOptionList ParmOption",
	"ParmOptionList : ParmOption",
	"ParmOption : SelectType ':' String",
	"ParmOption : Default ':' String",
	"ParmOption : Help ':' String",
	"ParmOption : HelpFile ':' String",
	"ParmOption : Precludes ':' FlagOrParmKey ':' String",
	"ParmOption : Requires ':' FlagOrParmKey ':' String",
	"ParmOption : Multiple ':' String",
	"ParmOption : Input ':' String",
	"ParmOption : Optional ':' String",
	"FlagOrParmKey : Flag",
	"FlagOrParmKey : Parameter",
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
# line 105 "xc.grammar.y"
{
        bzero((char *)&zzGlobal->_xc_Data, sizeof(_XC_data));
    } break;
case 2:
# line 109 "xc.grammar.y"
{ 
        if ( XCverbose) {
            DoFprintf(dialogLevel,"COMPLETED -> Header Section\n"); 
        }
    } break;
case 3:
# line 115 "xc.grammar.y"
{ 
        zzGlobal->_xc_Data.data = interface;
        /* check precludes and requirements for valid names */
        _XcValidDependencies(zzGlobal->_xc_Data.data,zzGlobal);
        if ( XCverbose) {
            DoFprintf(dialogLevel,"COMPLETED -> Program Section\n"); 
        }
    } break;
case 4:
# line 129 "xc.grammar.y"
{ 
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"BEGIN -> Description of Program: %s\n",yypvt[-0].cval);
        }
        zzGlobal->_xc_Data.prog = _XgStrDup(yypvt[-0].cval);
    } break;
case 5:
# line 136 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tTitle: %s\n",yypvt[-0].cval);
        }
        zzGlobal->_xc_Data.title = _XgStrDup(yypvt[-0].cval);
    } break;
case 6:
# line 143 "xc.grammar.y"
{
        zzGlobal->_xc_Data.argString = _XgStrDup(yypvt[-0].cval);
    } break;
case 12:
# line 169 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tDesc: %s\n",yypvt[-0].cval);
            DoFprintf(dialogLevel,"\tOptional Header Elements:\n");
        }
        zzGlobal->_xc_Data.description = _XgStrDup(yypvt[-0].cval);
    } break;
case 13:
# line 178 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelp: %s\n",yypvt[-0].cval);
        }
        zzGlobal->_xc_Data.help = _XgStrDup(yypvt[-0].cval);
    } break;
case 14:
# line 186 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelpFile: %s\n",yypvt[-0].cval);
        }
        zzGlobal->_xc_Data.helpFile.filename = _XgStrDup(yypvt[-0].cval);
    } break;
case 15:
# line 194 "xc.grammar.y"
{
	char *ptr = _XgStrDup(yypvt[-0].cval);
	XCHelpWidgetData *hptr = zzGlobal->_xc_Data.helpData;

        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelpWidgetRef: %s\n",yypvt[-0].cval);
        }
	if ( hptr ) {

	    while ( hptr->next ) hptr = hptr->next;
	    hptr->next = 
		(XCHelpWidgetData *)_XgMalloc(sizeof(XCHelpWidgetData));
	    hptr = hptr->next;
	    bzero((char *)hptr, sizeof(XCHelpWidgetData));
	} else {
	    hptr = zzGlobal->_xc_Data.helpData = 
		(XCHelpWidgetData *)_XgMalloc(sizeof(XCHelpWidgetData));
	    bzero((char *)hptr, sizeof(XCHelpWidgetData));
	}
	if ( !_XcParseHelpData(hptr,ptr) ) {
          sprintf(errorbuf,"Illegal help data [%s]",ptr);
          yyerror(errorbuf);
	}
	_XgFree(ptr);
    } break;
case 16:
# line 221 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tCapture: %s\n",yypvt[-0].cval);
        }
        zzGlobal->_xc_Data.capture = (ISONSTR(yypvt[-0].cval) || ISTRUESTR(yypvt[-0].cval));
    } break;
case 17:
# line 229 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tErrorcodes: %s\n",yypvt[-0].cval);
        }
        zzGlobal->_xc_Data.nerrcodes = _XcParseErrorCodes(yypvt[-0].cval,&(zzGlobal->_xc_Data.errorcodes));
	if ( zzGlobal->_xc_Data.nerrcodes == -1 ) {
          sprintf(errorbuf,"Illegal errorcode string [%s]", yypvt[-0].cval);
          yyerror(errorbuf);
	}
    } break;
case 25:
# line 276 "xc.grammar.y"
{
        /* 
        * Allocate a new pointer and push it on to the dialog stack.
        */
        if ( interface == NULL ) {
            curPtr = interface = 
                (XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
            bzero((char *)interface, sizeof(XCInterfaceData));
	    PUSHDIALOGPTR(curPtr);
	    curPtr->type = XC_DIALOG;
	    curPtr->name = _XgStrDup(yypvt[-0].cval);
        }  else {
	    tempPtr = 
		(XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
	    bzero((char *)tempPtr, sizeof(XCInterfaceData));
	    /* 
	     * If the current pointer is NOT of type dialog, go left, else
	     * if the right branch of the current pointer is NULL, go right,
	     * else, go left.
	     */
	    if ( curPtr->type != XC_DIALOG ) {
		curPtr = curPtr->left = tempPtr;
	    } else if ( curPtr->right == NULL ) {
		curPtr = curPtr->right = tempPtr;
	    } else {
		curPtr = curPtr->left = tempPtr;
	    }
	    PUSHDIALOGPTR(curPtr);
	    curPtr->type = XC_DIALOG;
	    curPtr->name = _XgStrDup(yypvt[-0].cval);
	}
        dialogLevel++;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"BEGIN -> Dialog Description\n");
            DoFprintf(dialogLevel,"\tDialog Desc: %s\n",yypvt[-0].cval);
        }
    } break;
case 26:
# line 318 "xc.grammar.y"
{
        /*
        * Done with this dialog, pop it off the stack and resume processing.
        */
        curPtr = POPDIALOGPTR;
        dialogLevel--;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"END -> Dialog Description\n");
        }
    } break;
case 28:
# line 335 "xc.grammar.y"
{
	curPtr->desc = _XgStrDup(yypvt[-0].cval);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Desc: %s\n",yypvt[-0].cval);
        }
    } break;
case 30:
# line 348 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelp: %s\n",yypvt[-0].cval);
        }
        curPtr->help = _XgStrDup(yypvt[-0].cval);
    } break;
case 31:
# line 356 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelpFile: %s\n",yypvt[-0].cval);
        }
        curPtr->helpFile.filename = _XgStrDup(yypvt[-0].cval);
    } break;
case 32:
# line 368 "xc.grammar.y"
{ 
	/* 
	* Check for uniqueness of the handle name
	*/
	if ( !_XcUniqueName(interface, yypvt[-0].cval) ) {
	    sprintf(errorbuf,"Non-unique flag or parameter name [%s]",yypvt[-0].cval);
	    yyerror(errorbuf);
	}
        /*
        * Allocate a new pointer and link it in to the list.
        */
        if ( interface == NULL ) {
            curPtr = interface = 
                (XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
            bzero((char *)interface, sizeof(XCInterfaceData));
        } else {
	    tempPtr = 
		(XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
	    bzero((char *)tempPtr, sizeof(XCInterfaceData));
	    if ( curPtr->type != XC_DIALOG ) {
		curPtr = curPtr->left = tempPtr;
	    } else if ( curPtr->right == NULL ) {
		curPtr = curPtr->right = tempPtr;
	    } else {
		curPtr = curPtr->left = tempPtr;
	    }
        }
        curPtr->type = XC_FLAG;
        curPtr->name = _XgStrDup(yypvt[-0].cval);
	CURFLAG = (XCFlagData *)_XgMalloc(sizeof(XCFlagData));
	bzero((char *)CURFLAG,sizeof(XCFlagData));
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"BEGIN -> Flag Description\n");
            DoFprintf(dialogLevel,"\tName: %s\n",yypvt[-0].cval);
        }
    } break;
case 33:
# line 405 "xc.grammar.y"
{ 
	CURFLAG->key = _XgStrDup(yypvt[-0].cval);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tKey: %s\n",yypvt[-0].cval);
        }
    } break;
case 34:
# line 412 "xc.grammar.y"
{
	CURFLAG->desc = _XgStrDup(yypvt[-4].cval);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tDesc: %s\n",yypvt[-0].cval);
            DoFprintf(dialogLevel,"\tOptional Flag Elements:\n");
        }
    } break;
case 35:
# line 421 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"COMPLETED -> Flag\n");
        }
    } break;
case 40:
# line 448 "xc.grammar.y"
{
	CURFLAG->help = _XgStrDup(yypvt[-0].cval);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelp: %s\n",yypvt[-0].cval);
        }
    } break;
case 41:
# line 456 "xc.grammar.y"
{
	CURFLAG->helpFile.filename = _XgStrDup(yypvt[-0].cval);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelpFile: %s\n",yypvt[-0].cval);
        }
    } break;
case 42:
# line 464 "xc.grammar.y"
{
	CURFLAG->def = ISONSTR(yypvt[-0].cval);
	CURFLAG->flagSet = True;
	CURFLAG->answer = curPtr->flag->def;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tDefault: %s\n",yypvt[-0].cval);
        }
    } break;
case 43:
# line 474 "xc.grammar.y"
{
	if (CURFLAG->precludes ) {
	    XCRequireData *rPtr = &CURFLAG->preclude;

	    while ( rPtr->next ) {
		rPtr = rPtr->next;
	    }
	    rPtr->next = (XCRequireData *)XtMalloc(sizeof(XCRequireData));
	    rPtr = rPtr->next;
	    rPtr->isFlag = ISFLAGSTR(yypvt[-2].cval);
	    rPtr->name = _XgStrDup(yypvt[-0].cval);
	    rPtr->next = NULL;
	} else {
	    CURFLAG->precludes = True;
	    CURFLAG->preclude.isFlag = ISFLAGSTR(yypvt[-2].cval);
	    CURFLAG->preclude.name = _XgStrDup(yypvt[-0].cval);
	    CURFLAG->preclude.next = NULL;
	}
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tPreculdes: %s -> %s\n",yypvt[-2].cval,yypvt[-0].cval);
        }
    } break;
case 44:
# line 498 "xc.grammar.y"
{
	if (CURFLAG->requirements ) {
	    XCRequireData *rPtr = &CURFLAG->requires;

	    while ( rPtr->next ) {
		rPtr = rPtr->next;
	    }
	    rPtr->next = (XCRequireData *)XtMalloc(sizeof(XCRequireData));
	    rPtr = rPtr->next;
	    rPtr->isFlag = ISFLAGSTR(yypvt[-2].cval);
	    rPtr->name = _XgStrDup(yypvt[-0].cval);
	    rPtr->next = NULL;
	} else {
	    CURFLAG->requirements = True;
	    CURFLAG->requires.isFlag = ISFLAGSTR(yypvt[-2].cval);
	    CURFLAG->requires.name = _XgStrDup(yypvt[-0].cval);
	    CURFLAG->requires.next = NULL;
	}
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tRequires: %s -> %s\n",yypvt[-2].cval,yypvt[-0].cval);
        }
    } break;
case 45:
# line 526 "xc.grammar.y"
{
	/* 
	* Check for uniqueness of the handle name
	*/
	if ( !_XcUniqueName(interface, yypvt[-0].cval) ) {
	    sprintf(errorbuf,"Non-unique flag or parameter name [%s]",yypvt[-0].cval);
	    yyerror(errorbuf);
	}
        if ( interface == NULL ) {
            curPtr = interface = 
                (XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
            bzero((char *)interface, sizeof(XCInterfaceData));
        } else {
	    tempPtr = 
		(XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
	    bzero((char *)tempPtr, sizeof(XCInterfaceData));
	    if ( curPtr->type != XC_DIALOG ) {
		curPtr = curPtr->left = tempPtr;
	    } else if ( curPtr->right == NULL ) {
		curPtr = curPtr->right = tempPtr;
	    } else {
		curPtr = curPtr->left = tempPtr;
	    }
        }
        curPtr->type = XC_PARM;
        curPtr->name = _XgStrDup(yypvt[-0].cval);
	curPtr->parm = (XCParmData *)_XgMalloc(sizeof(XCParmData));
	bzero((char *)curPtr->parm,sizeof(XCParmData));
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"BEGIN -> Parm Description\n");
            DoFprintf(dialogLevel,"\tKey: %s\n",yypvt[-0].cval);
        }
    } break;
case 46:
# line 560 "xc.grammar.y"
{
        /*
        * If this is a database element and an input parameter and has a 
        * default, check it out for existence.
        */
	if ( CURPARM->type == XC_TYPE_UNKNOWN ) {
          sprintf(errorbuf,"No type given for parameter [%s]",curPtr->name);
          yyerror(errorbuf);
	}
        if ( CURPARM->parmSet && CURPARM->isInput &&
             CURPARM->type == XC_TYPE_DB_ELEMENT )
            VerifyDBElement(CURPARM);
    } break;
case 47:
# line 574 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"COMPLETED -> Parm\n");
        }
    } break;
case 50:
# line 593 "xc.grammar.y"
{
	CURPARM->desc = _XgStrDup(yypvt[-0].cval);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tDesc: %s\n",yypvt[-0].cval);
        }
    } break;
case 51:
# line 601 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tType:");
            fflush(stderr);
        }
    } break;
case 53:
# line 614 "xc.grammar.y"
{
        CURPARM->type = XC_TYPE_CHARACTER;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Character\n");
        }
    } break;
case 54:
# line 622 "xc.grammar.y"
{
        CURPARM->type = XC_TYPE_ENUMERATE;
	CURPARM->hasModifier = True;
        XcModifierParse(&CURPARM->modifier,XC_TYPE_ENUMERATE,yypvt[-0].cval);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Enumerate\n");
            DoFprintf(dialogLevel,"\tValues: %s\n", yypvt[-0].cval);
        }
    } break;
case 55:
# line 633 "xc.grammar.y"
{
        CURPARM->type = XC_TYPE_INTEGER;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Integer\n");
        }
    } break;
case 56:
# line 641 "xc.grammar.y"
{
        CURPARM->type = XC_TYPE_FLOAT;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Float\n");
        }
    } break;
case 57:
# line 649 "xc.grammar.y"
{
        CURPARM->type = XC_TYPE_DOUBLE;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Double\n");
        }
    } break;
case 58:
# line 657 "xc.grammar.y"
{
        CURPARM->type = XC_TYPE_LOGICAL;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Logical\n");
        }
    } break;
case 59:
# line 665 "xc.grammar.y"
{
        CURPARM->type = XC_TYPE_FILENAME;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Filename\n");
        }
    } break;
case 60:
# line 673 "xc.grammar.y"
{
	CURPARM->type = XC_TYPE_DB_ELEMENT;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Database Element: ");
            fflush(stderr);
        }
    } break;
case 63:
# line 689 "xc.grammar.y"
{
        XcModifierParse(&CURPARM->modifier,XC_TYPE_INTEGER,yypvt[-0].cval);
        CURPARM->hasModifier = True;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Modifiers: %s",yypvt[-0].cval);
        }
    } break;
case 65:
# line 704 "xc.grammar.y"
{
        XcModifierParse(&CURPARM->modifier,XC_TYPE_DOUBLE,yypvt[-0].cval);
        CURPARM->hasModifier = True;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Modifiers: %s",yypvt[-0].cval);
        }
    } break;
case 67:
# line 719 "xc.grammar.y"
{
        XcModifierParse(&CURPARM->modifier,XC_TYPE_LOGICAL,yypvt[-0].cval);
        CURPARM->hasModifier = True;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Modifiers: %s",yypvt[-0].cval);
        }
    } break;
case 69:
# line 734 "xc.grammar.y"
{
        XcModifierParse(&CURPARM->modifier,XC_TYPE_FILENAME,yypvt[-0].cval);
        CURPARM->hasModifier = True;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Modifiers: %s",yypvt[-0].cval);
        }
    } break;
case 70:
# line 748 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_RASTER;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Raster [Status = ");
            fflush(stderr);
        }
    } break;
case 72:
# line 759 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_ASCII_DLG;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"ASCII DLG [Status = ");
            fflush(stderr);
        }
    } break;
case 74:
# line 770 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_DLG;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Binary DLG [Status = ");
            fflush(stderr);
        }
    } break;
case 76:
# line 781 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_ASCII_VECTOR;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"ASCII Vector [Status = ");
            fflush(stderr);
        }
    } break;
case 78:
# line 792 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_VECTOR;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Vector [Status = ");
            fflush(stderr);
        }
    } break;
case 80:
# line 803 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_SITES;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Sites [Status = ");
            fflush(stderr);
        }
    } break;
case 82:
# line 814 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_REGION;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Region [Status = ");
            fflush(stderr);
        }
    } break;
case 84:
# line 825 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_ICON;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Icon [Status = ");
            fflush(stderr);
        }
    } break;
case 86:
# line 836 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_LABEL;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Label [Status = ");
            fflush(stderr);
        }
    } break;
case 88:
# line 847 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_IMAGE_GROUP;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Imagery Group [Status = ");
            fflush(stderr);
        }
    } break;
case 90:
# line 858 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_IMAGE_SUBGROUP;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Imagery Sub Group [Status = ");
            fflush(stderr);
        }
    } break;
case 92:
# line 869 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_USER_DEFINED;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"UserDefined\n");
        }
    } break;
case 94:
# line 887 "xc.grammar.y"
{
        CURPARM->value.dbval.status = XC_DB_STATUS_NEW;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Default(New)]\n");
        }
    } break;
case 95:
# line 895 "xc.grammar.y"
{
        CURPARM->value.dbval.status = XC_DB_STATUS_OLD;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Old]\n");
        }
    } break;
case 97:
# line 904 "xc.grammar.y"
{
        CURPARM->value.dbval.status = XC_DB_STATUS_NEW;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"New]\n");
        }
    } break;
case 101:
# line 926 "xc.grammar.y"
{
        CURPARM->selecttype = XC_SELECT_UNDETERMINED;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tSelectType: %s\n",yypvt[-0].cval);
        }
    } break;
case 102:
# line 935 "xc.grammar.y"
{
        if ( CURPARM->type == XC_TYPE_UNKNOWN  &&
             CURPARM->selecttype == XC_SELECT_FALSE ) {
            yyerror("\"default\" defined before \"type\"");
        }

        switch(CURPARM->type) {
        case XC_TYPE_CHARACTER:
            CURPARM->def.cval = _XgStrDup(yypvt[-0].cval);
            CURPARM->value.cval = _XgStrDup(yypvt[-0].cval);
            break;
        case XC_TYPE_ENUMERATE:
            CURPARM->def.cval = _XgStrDup(
		_XcEnumStringToKey(CURPARM->modifier,yypvt[-0].cval));
            CURPARM->value.cval = _XgStrDup(CURPARM->def.cval);
            break;
        case XC_TYPE_INTEGER:
            CURPARM->def.ival = atoi(yypvt[-0].cval);
            CURPARM->value.ival = CURPARM->def.ival;
            break;
        case XC_TYPE_FLOAT:
#ifndef mips
            CURPARM->def.dval = strtod(yypvt[-0].cval,(char **)NULL);
#else
            CURPARM->def.dval = atof(yypvt[-0].cval);
#endif
            CURPARM->value.dval = CURPARM->def.dval;
            break;
        case XC_TYPE_DOUBLE:
#ifndef mips
            CURPARM->def.dval = strtod(yypvt[-0].cval,(char **)NULL);
#else
            CURPARM->def.dval = atof(yypvt[-0].cval);
#endif
            CURPARM->value.dval = CURPARM->def.dval;
            break;
        case XC_TYPE_LOGICAL:
            CURPARM->def.bval = (ISONSTR(yypvt[-0].cval) || ISTRUESTR(yypvt[-0].cval));
            CURPARM->value.bval = CURPARM->def.bval;
            break;
        case XC_TYPE_FILENAME:
            CURPARM->def.cval = _XgStrDup(yypvt[-0].cval);
            if ( CURPARM->isInput && access(CURPARM->def.cval,0) != 0 ) {
		sprintf(errorbuf,"invalid default input file name [%s]",yypvt[-0].cval);
		yyerror(errorbuf);
            }
            CURPARM->value.cval = _XgStrDup(yypvt[-0].cval);
            break;
        case XC_TYPE_DB_ELEMENT:
            CURPARM->def.dbval.desc = _XgStrDup(yypvt[-0].cval);
            CURPARM->value.dbval.desc = _XgStrDup(yypvt[-0].cval);
            break;
        }
        CURPARM->parmSet = True;

        if ( XCverbose  ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tDefault: %s\n",yypvt[-0].cval);
        }
    } break;
case 103:
# line 997 "xc.grammar.y"
{
	CURPARM->help = _XgStrDup(yypvt[-0].cval);
        if ( XCverbose  ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tHelp: %s\n",yypvt[-0].cval);
        }
    } break;
case 104:
# line 1006 "xc.grammar.y"
{
	CURPARM->helpFile.filename = _XgStrDup(yypvt[-0].cval);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tHelpFile: %s\n",yypvt[-0].cval);
        }
    } break;
case 105:
# line 1015 "xc.grammar.y"
{
        if (CURPARM->precludes ) {
            XCRequireData *rPtr = &CURPARM->preclude;

            while ( rPtr->next ) {
                rPtr = rPtr->next;
            }
            rPtr->next = (XCRequireData *)XtMalloc(sizeof(XCRequireData));
            rPtr = rPtr->next;
            rPtr->isFlag = ISFLAGSTR(yypvt[-2].cval);
            rPtr->name = _XgStrDup(yypvt[-0].cval);
            rPtr->next = NULL;
        } else {
            CURPARM->precludes = True;
            CURPARM->preclude.isFlag = ISFLAGSTR(yypvt[-2].cval);
            CURPARM->preclude.name = _XgStrDup(yypvt[-0].cval);
            CURPARM->preclude.next = NULL;
        }

        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tPrecludes: %s -> %s\n",yypvt[-2].cval,yypvt[-0].cval);
        }
    } break;
case 106:
# line 1040 "xc.grammar.y"
{
        if (CURPARM->requirements ) {
            XCRequireData *rPtr = &CURPARM->requires;

            while ( rPtr->next ) {
                rPtr = rPtr->next;
            }
            rPtr->next = (XCRequireData *)XtMalloc(sizeof(XCRequireData));
            rPtr = rPtr->next;
            rPtr->isFlag = ISFLAGSTR(yypvt[-2].cval);
            rPtr->name = _XgStrDup(yypvt[-0].cval);
            rPtr->next = NULL;
        } else {
            CURPARM->requirements = True;
            CURPARM->requires.isFlag = ISFLAGSTR(yypvt[-2].cval);
            CURPARM->requires.name = _XgStrDup(yypvt[-0].cval);
            CURPARM->requires.next = NULL;
        }
        if ( XCverbose ) DoFprintf(dialogLevel,"\t\tRequires: %s -> %s\n",yypvt[-2].cval,yypvt[-0].cval);
    } break;
case 107:
# line 1062 "xc.grammar.y"
{
      if ( CURPARM->type == XC_TYPE_FILENAME ||
           CURPARM->type == XC_TYPE_DB_ELEMENT ||
           CURPARM->type == XC_TYPE_ENUMERATE ) {
          CURPARM->multiple = (ISONSTR(yypvt[-0].cval) || ISTRUESTR(yypvt[-0].cval));
      } else {
          sprintf(errorbuf,"[%s] can not be multi-valued",curPtr->name);
          yyerror(errorbuf);
      }

        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tMultiple: %s\n",yypvt[-0].cval);
        }
    } break;
case 108:
# line 1079 "xc.grammar.y"
{
        CURPARM->isInput = (ISONSTR(yypvt[-0].cval) || ISTRUESTR(yypvt[-0].cval));
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tInput: %s\n",yypvt[-0].cval);
        }
    } break;
case 109:
# line 1088 "xc.grammar.y"
{
        CURPARM->optional  = (ISONSTR(yypvt[-0].cval) || ISTRUESTR(yypvt[-0].cval));
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tOptional: %s\n",yypvt[-0].cval);
        }
    } break;
case 110:
# line 1100 "xc.grammar.y"
{ yyval.cval = "flag"; } break;
case 111:
# line 1102 "xc.grammar.y"
{ yyval.cval = "parameter"; } break;
	}
	goto yystack;		/* reset registers in driver code */
}
