extern char *malloc(), *realloc();

# line 29 "grammar.y"

#include "xgen.h"

/* 
 * flag to die after the parse from an error that is not really bad 
 * enough to merit a stop of the parse, but could cause problems later  
 */

Boolean          fatalError = False;

/* 
 * flag to indicate that we're parsing a pulldonwn object 
 */

Boolean          doingPulldown = False;

/* 
 * pointers to current data types while building the application list.
 */

/* environment pointers */
Environ *        curEnv = (Environ *)0;
Environ *        tailEnv = (Environ *)0;

/* shell pointers */
Shell *          curShell = (Shell *)0;
Shell *          tailShell = (Shell *)0;

/* object pointers */
InterfaceObject *curObject = (InterfaceObject *)0;
InterfaceObject *pulldownObject = (InterfaceObject *)0;
InterfaceObject *tailObject = (InterfaceObject *)0;

/* resource pointers */
Resource *       curRes = (Resource *)0;
Resource *       tailRes = (Resource *)0;

/* the current type of item, shell, object, and resource we have */
int              itemType = ENV;
int              shellType = (1L<<0);
int              objectType = (1L<<0);

int              resourceType = ENVIRONMENT;


# line 77 "grammar.y"
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
# define Environment 261
# define InitialShells 262
# define Menu 263
# define CommandBoard 264
# define Label 265
# define Message 266
# define List 267
# define PushButton 268
# define TextEntry 269
# define Table 270
# define Separator 271
# define Slider 272
# define Toggle 273
# define PullDown 274
# define Font 275
# define FixedFont 276
# define Background 277
# define Foreground 278
# define BackgroundPixmap 279
# define TopShadowColor 280
# define TopShadowPixmap 281
# define BottomShadowColor 282
# define BottomShadowPixmap 283
# define X 284
# define DX 285
# define Y 286
# define DY 287
# define Width 288
# define Height 289
# define MaxWidth 290
# define MaxHeight 291
# define Columns 292
# define Override 293
# define Popup 294
# define Popdown 295
# define Destroy 296
# define Exit 297
# define Help 298
# define Eval 299
# define RunForeground 300
# define RunBackground 301
# define InputFrom 302
# define CaptureOutput 303
# define UpdateFrom 304
# define Pane 305
# define Store 306
# define Highlight 307
# define GetEnv 308
# define Clear 309
# define CommandArg 310
# define TableArg 311
# define Set 312
# define Alignment 313
# define ListElement 314
# define ListSeparator 315
# define ListType 316
# define VisibleItems 317
# define ScrollBar 318
# define ValueString 319
# define LabelPixmap 320
# define MaxLength 321
# define Minimum 322
# define Maximum 323
# define StartValue 324
# define SliderWidth 325
# define SliderHeight 326
# define Orientation 327
# define DecimalPoints 328
# define EntryFont 329
# define Rows 330
# define RowsDisplayed 331
# define ColumnsDisplayed 332
# define ColumnHeadings 333
# define RowHeadings 334
# define RowValue 335
# define TableValue 336
# define RowHeight 337
# define ColumnWidth 338
# define Newline 339
# define TitleString 340
# define ToggleType 341
# define SeparatorType 342
# define Sensitive 343
# define Insensitive 344
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 800 "grammar.y"

int yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 127
# define YYLAST 281
int yyact[]={

    46,   166,   133,   125,   126,   128,   129,   130,   131,   111,
    17,   153,    30,    47,    48,    49,    50,    51,    52,    53,
    54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
    64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
    74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
   117,    84,    85,    86,    87,    88,    89,    90,    91,    92,
    93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
   103,   104,   105,   106,   107,   108,   109,   110,   112,   113,
   114,   115,   116,     6,     7,     8,     9,    10,    11,    12,
    13,    14,   144,   145,   146,   147,   148,   149,   150,   151,
   152,   143,    39,    38,    37,    36,    35,    34,    33,    32,
    31,   141,   140,    41,   139,   118,   167,   162,   121,   120,
    27,    26,    29,   123,    25,    24,   127,    23,    22,    21,
    20,    19,    43,   164,   138,    16,     4,     5,    42,   161,
   125,   126,    18,   157,   158,   136,   122,    40,    15,     3,
     2,    28,    44,   142,   124,    45,     1,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   144,   145,   146,   147,   148,
   149,   150,   151,   152,   143,   144,   145,   146,   147,   148,
   149,   150,   151,   152,   143,     0,   134,   135,   132,     0,
   137,   155,   154,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   156,     0,     0,     0,     0,
     0,     0,   160,     0,     0,   163,     0,   155,   154,   165,
   159 };
int yypact[]={

 -1000, -1000,  -192,  -251,  -192, -1000,    73,    72,    71,    70,
    69,    67,    66,    63,    62,  -251, -1000,  -245, -1000,  -147,
  -148,  -149,  -150,  -151,  -152,  -153,  -154,  -155, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
  -262,    -8,  -262, -1000,    61,    60, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,  -260, -1000,
  -252,  -252,  -123, -1000,  -245, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000,  -262,    94,  -173,   -30,
 -1000, -1000,  -245, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000,  -245,  -262, -1000,
    58,  -262, -1000,    93,  -173,   -40,    57, -1000 };
int yypgo[]={

     0,   156,   122,   126,   155,   154,   153,   152,   150,   149,
   148,   135,   147,   113,   146,   123,   145,   114,   112,   111,
   144,   143,   139,   138,   132,   136,   137 };
int yyr1[]={

     0,     8,     1,    10,    10,    12,    11,    14,    14,    16,
    15,     5,     5,    17,    17,    17,    17,    20,    18,    21,
    22,    19,     6,     6,     6,     6,     6,     6,     6,     6,
     6,    13,    13,    23,    23,     2,    24,    24,     4,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     9,     9,    25,    25,    26,    26,    26,    26,    26,    26,
    26,    26,    26,     3,     3,     3,     3 };
int yyr2[]={

     0,     1,     7,     4,     2,     1,    14,     4,     2,     1,
    14,     3,     3,     4,     4,     2,     2,     1,    10,     1,
     1,    19,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     0,     2,     4,     2,     2,     7,     7,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     0,     2,     4,     2,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     3,     3,     3,     3 };
int yychk[]={

 -1000,    -1,    -8,    -9,   -25,   -26,   275,   276,   277,   278,
   279,   280,   281,   282,   283,   -10,   -11,   261,   -26,    58,
    58,    58,    58,    58,    58,    58,    58,    58,   -11,    -2,
   257,   257,   257,   257,   257,   257,   257,   257,   257,   257,
   -12,   -13,   -23,   -24,    -7,    -4,   262,   275,   276,   277,
   278,   279,   280,   281,   282,   283,   284,   285,   286,   287,
   288,   289,   290,   291,   292,   293,   294,   295,   296,   297,
   298,   299,   300,   301,   302,   303,   304,   305,   306,   307,
   308,   309,   310,   311,   313,   314,   315,   316,   317,   318,
   319,   320,   321,   322,   323,   324,   325,   326,   327,   328,
   329,   330,   331,   332,   333,   334,   335,   336,   337,   338,
   339,   271,   340,   341,   342,   343,   344,   312,   123,   -24,
    58,    58,   -14,   -15,    -5,   263,   264,    -3,   257,   258,
   259,   260,    -3,   125,   -15,    -2,   -16,   -13,    40,   -17,
   -18,   -19,    -6,   274,   265,   266,   267,   268,   269,   270,
   271,   272,   273,    41,   -18,   -19,    -2,   -21,   -20,    -2,
   -13,   -22,    59,   -13,    40,   -17,    41,    59 };
int yydef[]={

     1,    -2,   110,     0,   111,   113,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     2,     4,     0,   112,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     3,     5,
    35,   114,   115,   116,   117,   118,   119,   120,   121,   122,
    31,     0,    32,    34,     0,     0,    39,    40,    41,    42,
    43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
    53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
    63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
    73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
    83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
    93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
   103,   104,   105,   106,   107,   108,   109,    38,     0,    33,
     0,     0,     0,     8,     0,    11,    12,    36,   123,   124,
   125,   126,    37,     6,     7,     9,    31,     0,     0,     0,
    15,    16,     0,    19,    22,    23,    24,    25,    26,    27,
    28,    29,    30,    10,    13,    14,    17,     0,    31,    20,
     0,    31,    18,     0,     0,     0,     0,    21 };
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
	"Environment",	261,
	"InitialShells",	262,
	"Menu",	263,
	"CommandBoard",	264,
	"Label",	265,
	"Message",	266,
	"List",	267,
	"PushButton",	268,
	"TextEntry",	269,
	"Table",	270,
	"Separator",	271,
	"Slider",	272,
	"Toggle",	273,
	"PullDown",	274,
	"Font",	275,
	"FixedFont",	276,
	"Background",	277,
	"Foreground",	278,
	"BackgroundPixmap",	279,
	"TopShadowColor",	280,
	"TopShadowPixmap",	281,
	"BottomShadowColor",	282,
	"BottomShadowPixmap",	283,
	"X",	284,
	"DX",	285,
	"Y",	286,
	"DY",	287,
	"Width",	288,
	"Height",	289,
	"MaxWidth",	290,
	"MaxHeight",	291,
	"Columns",	292,
	"Override",	293,
	"Popup",	294,
	"Popdown",	295,
	"Destroy",	296,
	"Exit",	297,
	"Help",	298,
	"Eval",	299,
	"RunForeground",	300,
	"RunBackground",	301,
	"InputFrom",	302,
	"CaptureOutput",	303,
	"UpdateFrom",	304,
	"Pane",	305,
	"Store",	306,
	"Highlight",	307,
	"GetEnv",	308,
	"Clear",	309,
	"CommandArg",	310,
	"TableArg",	311,
	"Set",	312,
	"Alignment",	313,
	"ListElement",	314,
	"ListSeparator",	315,
	"ListType",	316,
	"VisibleItems",	317,
	"ScrollBar",	318,
	"ValueString",	319,
	"LabelPixmap",	320,
	"MaxLength",	321,
	"Minimum",	322,
	"Maximum",	323,
	"StartValue",	324,
	"SliderWidth",	325,
	"SliderHeight",	326,
	"Orientation",	327,
	"DecimalPoints",	328,
	"EntryFont",	329,
	"Rows",	330,
	"RowsDisplayed",	331,
	"ColumnsDisplayed",	332,
	"ColumnHeadings",	333,
	"RowHeadings",	334,
	"RowValue",	335,
	"TableValue",	336,
	"RowHeight",	337,
	"ColumnWidth",	338,
	"Newline",	339,
	"TitleString",	340,
	"ToggleType",	341,
	"SeparatorType",	342,
	"Sensitive",	343,
	"Insensitive",	344,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"Program : /* empty */",
	"Program : OptGlobals EnvironList",
	"EnvironList : EnvironList Environ",
	"EnvironList : Environ",
	"Environ : Environment Name",
	"Environ : Environment Name OptResourceList '{' ShellList '}'",
	"ShellList : ShellList Shell",
	"ShellList : Shell",
	"Shell : ShellType Name",
	"Shell : ShellType Name OptResourceList '(' ObjectList ')'",
	"ShellType : Menu",
	"ShellType : CommandBoard",
	"ObjectList : ObjectList Object",
	"ObjectList : ObjectList PulldownObject",
	"ObjectList : Object",
	"ObjectList : PulldownObject",
	"Object : ObjectType Name",
	"Object : ObjectType Name OptResourceList ';'",
	"PulldownObject : PullDown",
	"PulldownObject : PullDown Name",
	"PulldownObject : PullDown Name OptResourceList '(' ObjectList ')' ';'",
	"ObjectType : Label",
	"ObjectType : Message",
	"ObjectType : List",
	"ObjectType : PushButton",
	"ObjectType : TextEntry",
	"ObjectType : Table",
	"ObjectType : Separator",
	"ObjectType : Slider",
	"ObjectType : Toggle",
	"OptResourceList : /* empty */",
	"OptResourceList : ResourceList",
	"ResourceList : ResourceList Resource",
	"ResourceList : Resource",
	"Name : String",
	"Resource : ResourceType ':' Value",
	"Resource : SetResourceType ':' Value",
	"SetResourceType : Set",
	"ResourceType : InitialShells",
	"ResourceType : Font",
	"ResourceType : FixedFont",
	"ResourceType : Background",
	"ResourceType : Foreground",
	"ResourceType : BackgroundPixmap",
	"ResourceType : TopShadowColor",
	"ResourceType : TopShadowPixmap",
	"ResourceType : BottomShadowColor",
	"ResourceType : BottomShadowPixmap",
	"ResourceType : X",
	"ResourceType : DX",
	"ResourceType : Y",
	"ResourceType : DY",
	"ResourceType : Width",
	"ResourceType : Height",
	"ResourceType : MaxWidth",
	"ResourceType : MaxHeight",
	"ResourceType : Columns",
	"ResourceType : Override",
	"ResourceType : Popup",
	"ResourceType : Popdown",
	"ResourceType : Destroy",
	"ResourceType : Exit",
	"ResourceType : Help",
	"ResourceType : Eval",
	"ResourceType : RunForeground",
	"ResourceType : RunBackground",
	"ResourceType : InputFrom",
	"ResourceType : CaptureOutput",
	"ResourceType : UpdateFrom",
	"ResourceType : Pane",
	"ResourceType : Store",
	"ResourceType : Highlight",
	"ResourceType : GetEnv",
	"ResourceType : Clear",
	"ResourceType : CommandArg",
	"ResourceType : TableArg",
	"ResourceType : Alignment",
	"ResourceType : ListElement",
	"ResourceType : ListSeparator",
	"ResourceType : ListType",
	"ResourceType : VisibleItems",
	"ResourceType : ScrollBar",
	"ResourceType : ValueString",
	"ResourceType : LabelPixmap",
	"ResourceType : MaxLength",
	"ResourceType : Minimum",
	"ResourceType : Maximum",
	"ResourceType : StartValue",
	"ResourceType : SliderWidth",
	"ResourceType : SliderHeight",
	"ResourceType : Orientation",
	"ResourceType : DecimalPoints",
	"ResourceType : EntryFont",
	"ResourceType : Rows",
	"ResourceType : RowsDisplayed",
	"ResourceType : ColumnsDisplayed",
	"ResourceType : ColumnHeadings",
	"ResourceType : RowHeadings",
	"ResourceType : RowValue",
	"ResourceType : TableValue",
	"ResourceType : RowHeight",
	"ResourceType : ColumnWidth",
	"ResourceType : Newline",
	"ResourceType : Separator",
	"ResourceType : TitleString",
	"ResourceType : ToggleType",
	"ResourceType : SeparatorType",
	"ResourceType : Sensitive",
	"ResourceType : Insensitive",
	"OptGlobals : /* empty */",
	"OptGlobals : Globals",
	"Globals : Globals GlobalElement",
	"Globals : GlobalElement",
	"GlobalElement : Font ':' String",
	"GlobalElement : FixedFont ':' String",
	"GlobalElement : Background ':' String",
	"GlobalElement : Foreground ':' String",
	"GlobalElement : BackgroundPixmap ':' String",
	"GlobalElement : TopShadowColor ':' String",
	"GlobalElement : TopShadowPixmap ':' String",
	"GlobalElement : BottomShadowColor ':' String",
	"GlobalElement : BottomShadowPixmap ':' String",
	"Value : String",
	"Value : Integer",
	"Value : Real",
	"Value : Logical",
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
# line 195 "grammar.y"
{ 
			xgenGD.screen = DefaultScreen(xgenGD.display);
			xgenGD.scrptr = XtScreen(xgenGD.applShell);
			xgenGD.cmap = DefaultColormap(xgenGD.display,xgenGD.screen);
		} break;
case 2:
# line 202 "grammar.y"
{
			return(fatalError);
		} break;
case 5:
# line 215 "grammar.y"
{
			if ( !UniqueEnvName(yypvt[-0].cval) ) {
				char errorbuf[80];

				sprintf(errorbuf,"Non-unique environment name \"%s\"",yypvt[-0].cval);
				yyerror(errorbuf,False);
			}
			curEnv = AllocEnviron(); 
			curEnv->name = yypvt[-0].cval;
			AddEnviron(curEnv);
			tailEnv = curEnv;
			resourceType = ENVIRONMENT;
			itemType = ENV;
		} break;
case 9:
# line 243 "grammar.y"
{
			if ( !UniqueShellName(yypvt[-0].cval) ) {
				char errorbuf[80];

				sprintf(errorbuf,"Non-unique shell name \"%s\"",yypvt[-0].cval);
				yyerror(errorbuf,False);
			}
			curShell = AllocShell(); 
			curShell->type = yypvt[-1].ival;
			curShell->name = yypvt[-0].cval;
			AddShell(curShell,tailEnv);
			tailShell = curShell;
			resourceType = SHELL;
			itemType = SHL;
		} break;
case 11:
# line 263 "grammar.y"
{ yyval.ival = MENU; shellType = MU; } break;
case 12:
# line 265 "grammar.y"
{ yyval.ival = COMMANDBOARD; shellType = CO; } break;
case 17:
# line 278 "grammar.y"
{
			curObject = AllocObject(); 
			curObject->type = yypvt[-1].ival;
			curObject->name = SaveString(yypvt[-0].cval);
			if ( doingPulldown && !(objectType & (PB|SE))) {
				char errorbuf[80];

				sprintf(errorbuf,"invalid object type %s for pulldown menu.",
					ObjectString(objectType));
				XgenFatalError("parser",errorbuf);
			}
			if (!(ShellObjectValid(shellType) & objectType)) {
				char errorbuf[80];

				sprintf(errorbuf,"invalid object type %s for shell type %s.",
					ObjectString(objectType),ShellString(shellType));
				XgenFatalError("parser",errorbuf);
			}
			if ( doingPulldown )
			    AddPulldownObject(curObject,pulldownObject);
			else
			    AddObject(curObject,tailShell);
			tailObject = curObject;
			resourceType = OBJECT;
			itemType = OBJ;
		} break;
case 19:
# line 309 "grammar.y"
{
			objectType = PU;
		} break;
case 20:
# line 313 "grammar.y"
{
			doingPulldown = True;
			curObject = AllocObject(); 
			curObject->type = PULLDOWN;
			curObject->name = SaveString(yypvt[-0].cval);
			if (!(ShellObjectValid(shellType) & objectType)) {
				char errorbuf[80];

				sprintf(errorbuf,"invalid object type %s for shell type %s.",
					ObjectString(objectType),ShellString(shellType));
				XgenFatalError("parser",errorbuf);
			}
			AddObject(curObject,tailShell);
			tailObject = curObject;
			pulldownObject = curObject;
			resourceType = OBJECT;
			itemType = OBJ;
		} break;
case 21:
# line 333 "grammar.y"
{
			  doingPulldown = False;
		} break;
case 22:
# line 340 "grammar.y"
{ yyval.ival = LABEL; objectType = LA; } break;
case 23:
# line 342 "grammar.y"
{ yyval.ival = MESSAGE; objectType = ME; } break;
case 24:
# line 344 "grammar.y"
{ yyval.ival = LIST; objectType = LI; } break;
case 25:
# line 346 "grammar.y"
{ yyval.ival = PUSHBUTTON; objectType = PB; } break;
case 26:
# line 348 "grammar.y"
{ yyval.ival = TEXTENTRY; objectType = TE; } break;
case 27:
# line 350 "grammar.y"
{ yyval.ival = TABLE; objectType = TA; } break;
case 28:
# line 352 "grammar.y"
{ yyval.ival = SEPARATOR; objectType = SE; } break;
case 29:
# line 354 "grammar.y"
{ yyval.ival = SLIDER; objectType = SL; } break;
case 30:
# line 356 "grammar.y"
{ yyval.ival = TOGGLE; objectType = TO; } break;
case 36:
# line 375 "grammar.y"
{
			curRes = AllocResource(); 
			if ( rindex(yypvt[-0].cval,'$')) 
				curRes->variable = True;
			if ( !curRes->variable) {
			    switch(ResourceDataType(yypvt[-2].ival)) {
				    case INTEGER:
					    if (!CheckType(yypvt[-0].cval,Int)) yyerror(errorbuf);
					    break;
				    case REAL:
					    if (!CheckType(yypvt[-0].cval,Real)) yyerror(errorbuf);
					    break;
				    case BOOLEAN:
					    if (!CheckType(yypvt[-0].cval,OnOff)) yyerror(errorbuf);
					    break;
				}
			}
			switch(resourceType) {
				case ENVIRONMENT:
					if ( !(ResourceValid(yypvt[-2].ival) & itemType) ) {
						char errorbuf[80];

						sprintf(errorbuf,"invalid environment resource: %s",
							ResourceString(yypvt[-2].ival));
						XgenFatalError("parser",errorbuf);
					}
					AddResource(curRes,(char *)curEnv,resourceType,yypvt[-0].cval,yypvt[-2].ival);
					break;
				case SHELL:
					if ( !(ResourceValid(yypvt[-2].ival) & shellType) ) {
						char errorbuf[80];

						sprintf(errorbuf,"invalid shell resource: %s",
							ResourceString(yypvt[-2].ival));
						XgenFatalError("parser",errorbuf);
					}
					AddResource(curRes,(char *)curShell,resourceType,yypvt[-0].cval,yypvt[-2].ival);
					break;
				case OBJECT:
					if ( !(ResourceValid(yypvt[-2].ival) & objectType) ) {
						char errorbuf[80];

						sprintf(errorbuf,"invalid object resource: %s",
							ResourceString(yypvt[-2].ival));
						XgenFatalError("parser",errorbuf);
					}
					AddResource(curRes,(char *)curObject,resourceType,yypvt[-0].cval,yypvt[-2].ival);
					break;
			}
			tailRes = curRes;
		} break;
case 37:
# line 427 "grammar.y"
{
			char buf[80];

			/*
			 * go ahead and set the resource to x=y. Will check later 
			 * for modifiers that need to wait til runtime to get a value.
			 */
			sprintf(buf,"%s=%s",yypvt[-2].cval,yypvt[-0].cval);
			curRes = AllocResource(); 
			if ( rindex(buf,'$')) 
				curRes->variable = True;
			switch (resourceType) {
				case ENVIRONMENT:
			        AddResource(curRes,(char *)curEnv,ENVIRONMENT,SaveString(buf),Set);
				    break;
				case SHELL:
			        AddResource(curRes,(char *)curShell,SHELL,SaveString(buf),Set);
				    break;
				case OBJECT:
			        AddResource(curRes,(char *)curObject,OBJECT,SaveString(buf),Set);
				    break;
			}
			tailRes = curRes;
		} break;
case 38:
# line 454 "grammar.y"
{ yyval.cval = yypvt[-0].cval; } break;
case 39:
# line 458 "grammar.y"
{ yyval.ival = InitialShells; } break;
case 40:
# line 460 "grammar.y"
{ yyval.ival =  Font ; } break;
case 41:
# line 462 "grammar.y"
{ yyval.ival =  FixedFont ; } break;
case 42:
# line 464 "grammar.y"
{ yyval.ival =  Background ; } break;
case 43:
# line 466 "grammar.y"
{ yyval.ival =  Foreground ; } break;
case 44:
# line 468 "grammar.y"
{ yyval.ival =  BackgroundPixmap ; } break;
case 45:
# line 470 "grammar.y"
{ yyval.ival =  TopShadowColor ; } break;
case 46:
# line 472 "grammar.y"
{ yyval.ival =  TopShadowPixmap ; } break;
case 47:
# line 474 "grammar.y"
{ yyval.ival =  BottomShadowColor ; } break;
case 48:
# line 476 "grammar.y"
{ yyval.ival =  BottomShadowPixmap ; } break;
case 49:
# line 478 "grammar.y"
{ yyval.ival =  X ; } break;
case 50:
# line 480 "grammar.y"
{ yyval.ival =  DX ; } break;
case 51:
# line 482 "grammar.y"
{ yyval.ival =  Y ; } break;
case 52:
# line 484 "grammar.y"
{ yyval.ival =  DY ; } break;
case 53:
# line 486 "grammar.y"
{ yyval.ival =  Width ; } break;
case 54:
# line 488 "grammar.y"
{ yyval.ival =  Height ; } break;
case 55:
# line 490 "grammar.y"
{ yyval.ival =  MaxWidth ; } break;
case 56:
# line 492 "grammar.y"
{ yyval.ival =  MaxHeight ; } break;
case 57:
# line 494 "grammar.y"
{ yyval.ival =  Columns ; } break;
case 58:
# line 496 "grammar.y"
{ yyval.ival =  Override ; } break;
case 59:
# line 498 "grammar.y"
{ yyval.ival =  Popup ; } break;
case 60:
# line 500 "grammar.y"
{ yyval.ival =  Popdown ; } break;
case 61:
# line 502 "grammar.y"
{ yyval.ival =  Destroy ; } break;
case 62:
# line 504 "grammar.y"
{ yyval.ival =  Exit ; } break;
case 63:
# line 506 "grammar.y"
{ yyval.ival =  Help ; } break;
case 64:
# line 508 "grammar.y"
{ yyval.ival =  Eval ; } break;
case 65:
# line 510 "grammar.y"
{ yyval.ival =  RunForeground ; } break;
case 66:
# line 512 "grammar.y"
{ yyval.ival =  RunBackground ; } break;
case 67:
# line 514 "grammar.y"
{ yyval.ival =  InputFrom ; } break;
case 68:
# line 516 "grammar.y"
{ yyval.ival =  CaptureOutput ; } break;
case 69:
# line 518 "grammar.y"
{ yyval.ival =  UpdateFrom ; } break;
case 70:
# line 520 "grammar.y"
{ yyval.ival =  Pane ; } break;
case 71:
# line 522 "grammar.y"
{ yyval.ival =  Store ; } break;
case 72:
# line 524 "grammar.y"
{ yyval.ival =  Highlight ; } break;
case 73:
# line 526 "grammar.y"
{ yyval.ival =  GetEnv ; } break;
case 74:
# line 528 "grammar.y"
{ yyval.ival =  Clear ; } break;
case 75:
# line 530 "grammar.y"
{ yyval.ival =  CommandArg ; } break;
case 76:
# line 532 "grammar.y"
{ yyval.ival =  TableArg ; } break;
case 77:
# line 534 "grammar.y"
{ yyval.ival =  Alignment ; } break;
case 78:
# line 536 "grammar.y"
{ yyval.ival =  ListElement ; } break;
case 79:
# line 538 "grammar.y"
{ yyval.ival =  ListSeparator ; } break;
case 80:
# line 540 "grammar.y"
{ yyval.ival =  ListType ; } break;
case 81:
# line 542 "grammar.y"
{ yyval.ival =  VisibleItems ; } break;
case 82:
# line 544 "grammar.y"
{ yyval.ival =  ScrollBar ; } break;
case 83:
# line 546 "grammar.y"
{ yyval.ival =  ValueString ; } break;
case 84:
# line 548 "grammar.y"
{ yyval.ival =  LabelPixmap ; } break;
case 85:
# line 550 "grammar.y"
{ yyval.ival =  MaxLength ; } break;
case 86:
# line 552 "grammar.y"
{ yyval.ival =  Minimum ; } break;
case 87:
# line 554 "grammar.y"
{ yyval.ival =  Maximum ; } break;
case 88:
# line 556 "grammar.y"
{ yyval.ival =  StartValue ; } break;
case 89:
# line 558 "grammar.y"
{ yyval.ival =  SliderWidth ; } break;
case 90:
# line 560 "grammar.y"
{ yyval.ival =  SliderHeight ; } break;
case 91:
# line 562 "grammar.y"
{ yyval.ival =  Orientation ; } break;
case 92:
# line 564 "grammar.y"
{ yyval.ival =  DecimalPoints ; } break;
case 93:
# line 566 "grammar.y"
{ yyval.ival =  EntryFont ; } break;
case 94:
# line 568 "grammar.y"
{ yyval.ival =  Rows ; } break;
case 95:
# line 570 "grammar.y"
{ yyval.ival =  RowsDisplayed ; } break;
case 96:
# line 572 "grammar.y"
{ yyval.ival =  ColumnsDisplayed ; } break;
case 97:
# line 574 "grammar.y"
{ yyval.ival =  ColumnHeadings ; } break;
case 98:
# line 576 "grammar.y"
{ yyval.ival =  RowHeadings ; } break;
case 99:
# line 578 "grammar.y"
{ yyval.ival =  RowValue ; } break;
case 100:
# line 580 "grammar.y"
{ yyval.ival =  TableValue ; } break;
case 101:
# line 582 "grammar.y"
{ yyval.ival =  RowHeight ; } break;
case 102:
# line 584 "grammar.y"
{ yyval.ival =  ColumnWidth ; } break;
case 103:
# line 586 "grammar.y"
{ yyval.ival =  Newline ; } break;
case 104:
# line 588 "grammar.y"
{ yyval.ival =  Separator ; } break;
case 105:
# line 590 "grammar.y"
{ yyval.ival =  TitleString ; } break;
case 106:
# line 592 "grammar.y"
{ yyval.ival =  ToggleType ; } break;
case 107:
# line 594 "grammar.y"
{ yyval.ival =  SeparatorType ; } break;
case 108:
# line 596 "grammar.y"
{ yyval.ival =  Sensitive ; } break;
case 109:
# line 598 "grammar.y"
{ yyval.ival =  Insensitive ; } break;
case 114:
# line 611 "grammar.y"
{
			xgenGD.g_font = SaveString(yypvt[-0].cval);
			if ( (xgenGD.g_fs = XLoadQueryFont(xgenGD.display,xgenGD.g_font)) == 0 ) {
				char errorbuf[80];

				sprintf(errorbuf,"font %s not found",xgenGD.g_font);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		} break;
case 115:
# line 622 "grammar.y"
{
			xgenGD.g_ffont = SaveString(yypvt[-0].cval);
			if ((xgenGD.g_ffs = XLoadQueryFont(xgenGD.display,xgenGD.g_ffont)) != 0 ) {
				if (xgenGD.g_ffs->min_bounds.width !=  xgenGD.g_ffs->max_bounds.width) {
					char errorbuf[80];
	
					sprintf(errorbuf,"fixed font %s, not fixed",xgenGD.g_ffont);
					XgenFatalWarning("parser",errorbuf);
					fatalError = True;
				} 
			} else {
					char errorbuf[80];
	
					sprintf(errorbuf,"fixed font %s not found",xgenGD.g_ffont);
					XgenFatalWarning("parser",errorbuf);
					fatalError = True;
			}
		} break;
case 116:
# line 641 "grammar.y"
{
			xgenGD.g_bg = SaveString(yypvt[-0].cval);
			if (XParseColor(xgenGD.display,xgenGD.cmap,xgenGD.g_bg,&xgenGD.g_bgs) != 0){
				if ( XAllocColor(xgenGD.display,xgenGD.cmap,&xgenGD.g_bgs) == 0 ) {
					char errorbuf[80];

					sprintf(errorbuf,"couldn't allocate color %s",xgenGD.g_bg);
					XgenFatalWarning("parser",errorbuf);
					fatalError = True;
				}
			} else {
				char errorbuf[80];

				sprintf(errorbuf,"invalid background color %s",xgenGD.g_bg);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		} break;
case 117:
# line 660 "grammar.y"
{
			xgenGD.g_fg = SaveString(yypvt[-0].cval);
			if (XParseColor(xgenGD.display,xgenGD.cmap,xgenGD.g_fg,&xgenGD.g_fgs) != 0){
				if ( XAllocColor(xgenGD.display,xgenGD.cmap,&xgenGD.g_fgs) == 0 ) {
					char errorbuf[80];

					sprintf(errorbuf,"couldn't allocate color %s",xgenGD.g_fg);
					XgenFatalWarning("parser",errorbuf);
					fatalError = True;
				}
			} else {
				char errorbuf[80];

				sprintf(errorbuf,"invalid foreground color %s",xgenGD.g_fg);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		} break;
case 118:
# line 679 "grammar.y"
{ 
			Pixel tfg = ((xgenGD.g_fgs.pixel) ? 
				(xgenGD.g_fgs.pixel):BlackPixel(xgenGD.display,xgenGD.screen));
			Pixel tbg = ((xgenGD.g_bgs.pixel) ? 
				(xgenGD.g_bgs.pixel):WhitePixel(xgenGD.display,xgenGD.screen));

			xgenGD.g_bgpix = SaveString(yypvt[-0].cval);
			xgenGD.g_bgpm = XmGetPixmap(xgenGD.scrptr, xgenGD.g_bgpix, tfg, tbg);
			if ( xgenGD.g_bgpm == XmUNSPECIFIED_PIXMAP ) {
				char errorbuf[80];

				sprintf(errorbuf,
					"invalid background pixmap %s",xgenGD.g_bgpix);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		} break;
case 119:
# line 697 "grammar.y"
{ 
			xgenGD.g_ts = SaveString(yypvt[-0].cval);
			if (XParseColor(xgenGD.display,xgenGD.cmap,xgenGD.g_ts,&xgenGD.g_tss) != 0){
				if ( XAllocColor(xgenGD.display,xgenGD.cmap,&xgenGD.g_tss) == 0 ) {
					char errorbuf[80];

					sprintf(errorbuf,"couldn't allocate color %s",xgenGD.g_ts);
					XgenFatalWarning("parser",errorbuf);
					fatalError = True;
				}
			} else {
				char errorbuf[80];

				sprintf(errorbuf,
					"invalid top shadow color %s",xgenGD.g_ts);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		} break;
case 120:
# line 717 "grammar.y"
{ 
			Pixel tfg = ((xgenGD.g_fgs.pixel) ? 
				(xgenGD.g_fgs.pixel):BlackPixel(xgenGD.display,xgenGD.screen));
			Pixel tbg = ((xgenGD.g_bgs.pixel) ? 
				(xgenGD.g_bgs.pixel):WhitePixel(xgenGD.display,xgenGD.screen));

			xgenGD.g_tspix = SaveString(yypvt[-0].cval);
			xgenGD.g_tspm = XmGetPixmap(xgenGD.scrptr, xgenGD.g_tspix, tfg, tbg);
			if ( xgenGD.g_tspm == XmUNSPECIFIED_PIXMAP ) {
				char errorbuf[80];

				sprintf(errorbuf,
					"invalid top shadow pixmap %s",xgenGD.g_tspix);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		} break;
case 121:
# line 735 "grammar.y"
{ 
			xgenGD.g_bs = SaveString(yypvt[-0].cval);
			if (XParseColor(xgenGD.display,xgenGD.cmap,xgenGD.g_bs,&xgenGD.g_bss) != 0){
				if ( XAllocColor(xgenGD.display,xgenGD.cmap,&xgenGD.g_bss) == 0 ) {
					char errorbuf[80];

					sprintf(errorbuf,"couldn't allocate color %s",xgenGD.g_bs);
					XgenFatalWarning("parser",errorbuf);
					fatalError = True;
				}
			} else {
				char errorbuf[80];

				sprintf(errorbuf,
					"invalid bottom shadow color %s",xgenGD.g_bs);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		} break;
case 122:
# line 755 "grammar.y"
{ 
			Pixel tfg = ((xgenGD.g_fgs.pixel) ? 
				(xgenGD.g_fgs.pixel):BlackPixel(xgenGD.display,xgenGD.screen));
			Pixel tbg = ((xgenGD.g_bgs.pixel) ? 
				(xgenGD.g_bgs.pixel):WhitePixel(xgenGD.display,xgenGD.screen));

			xgenGD.g_bspix = SaveString(yypvt[-0].cval);
			xgenGD.g_bspm = XmGetPixmap(xgenGD.scrptr, xgenGD.g_bspix, tfg, tbg);
			if ( xgenGD.g_bspm == XmUNSPECIFIED_PIXMAP ) {
				char errorbuf[80];

				sprintf(errorbuf,
					"invalid bottom shadow pixmap %s",xgenGD.g_bspix);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		} break;
case 123:
# line 776 "grammar.y"
{
			yyval.cval = SaveString(yypvt[-0].cval);
		} break;
case 124:
# line 780 "grammar.y"
{
			char buf[80];

			sprintf(buf,"%d",yypvt[-0].ival);
			yyval.cval = SaveString(buf);
		} break;
case 125:
# line 787 "grammar.y"
{
			char buf[80];

			sprintf(buf,"%f",yypvt[-0].dval);
			yyval.cval = SaveString(buf);
		} break;
case 126:
# line 794 "grammar.y"
{
			if ( yypvt[-0].bval == True ) yyval.cval = SaveString("True");
			else yyval.cval = SaveString("False");
		} break;
	}
	goto yystack;		/* reset registers in driver code */
}
