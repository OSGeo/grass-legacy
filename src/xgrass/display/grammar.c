extern char *malloc(), *realloc();

# line 5 "grammar.y"

#define __DEBUG 0
#include "xgdisp.h"
extern double atof();

typedef struct _xgdisp_parser_globs {
    Boolean docSizeSet;
    char *docSize;
    Boolean pageWidthSet;
    double pageWidth;
    Boolean pageHeightSet;
    double pageHeight;
    Boolean unitsSet;
    char *units;
} XgdDisplayParserGlobals;

XgdDisplayParserGlobals parserGlobals;

XgdObject *curObject;
XgdObject templateObject;
XgdObject *geoFrameObject;
Boolean geoFrame = False;
Boolean specificsSet = False;
int gridLinePattern;

struct _vect_template {
    Pixel fg;
    char *name;
    char *mapset;
    int lp;
    int lw;
} vectTempl;

struct _site_template {
    char *name;
    char *mapset;
    int type;
    char *pixmapfile;
    int icontype;
    int size;
    int linewidth;
    Pixel color;
} siteTempl;

#define GEOFRAME_OBJECT_TYPE(x) ((x)->type == XGD_BARSCALE || \
                                 (x)->type == XGD_LEGEND || \
                                 (x)->type == XGD_GRID )

#define TYPE_WITH_SPECIFICS(x) ((x)->type == XGD_POLYLINE || \
                                (x)->type == XGD_POLYGON || \
                                (x)->type == XGD_GEOFRAME || \
                                (x)->type == XGD_LABEL || \
                                (x)->type == XGD_CLOSED_APPROX_SPLINE || \
                                (x)->type == XGD_OPEN_APPROX_SPLINE || \
                                (x)->type == XGD_CLOSED_INTERP_SPLINE || \
                                (x)->type == XGD_OPEN_INTERP_SPLINE )


# line 64 "grammar.y"
typedef union  {
 char *cval;
 int ival;
} YYSTYPE;
# define DOCUMENTSIZE 257
# define PAGEWIDTH 258
# define PAGEHEIGHT 259
# define UNITS 260
# define NIL 261
# define OBJECT 262
# define GEOFRAME 263
# define REGION 264
# define RASTER_MAP 265
# define VECTOR_MAP 266
# define SITE_MAP 267
# define STANDARDSITE 268
# define PIXMAPSITE 269
# define FREEHANDSITE 270
# define GRID 271
# define BARSCALE 272
# define LEGEND 273
# define SQUARE 274
# define RECTANGLE 275
# define CIRCLE 276
# define ELLIPSE 277
# define POLYLINE 278
# define POLYGON 279
# define SPLINE 280
# define OPEN_INTERP_SPLINE 281
# define CLOSED_INTERP_SPLINE 282
# define OPEN_APPROX_SPLINE 283
# define CLOSED_APPROX_SPLINE 284
# define LABEL 285
# define String 286
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 1343 "grammar.y"

int yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 124
# define YYLAST 264
int yyact[]={

    25,   222,   220,   217,   216,   213,   212,   207,    60,   206,
   205,    26,    27,    28,    29,    30,    31,   203,    32,    33,
    34,    35,    36,    62,    63,    61,   199,   197,   196,   195,
    64,    77,   193,   189,   187,   186,   185,   183,   179,   176,
   175,   174,   170,   169,   164,   163,   160,   159,   158,   154,
   153,   143,   142,   141,   138,   137,   129,   128,   127,   126,
   125,   124,   123,   113,   112,   111,   110,   109,   108,   107,
   106,   105,    93,    80,    54,    52,    50,    48,    46,    44,
    42,    40,    38,    22,    15,    14,    13,    12,    83,    84,
    85,    86,    17,    20,    76,    87,    88,    89,   147,   148,
   149,     6,     7,     8,     9,    16,    72,    74,    18,    19,
   173,   223,   218,   211,   209,   204,   178,   172,   146,   131,
   114,    95,    94,    90,    81,    59,    18,   140,    73,    58,
   104,   122,    77,     5,    75,   157,   156,   194,    11,   184,
   171,   155,   202,   192,   182,   168,   152,   136,   121,   103,
   221,   219,   215,   210,   201,   191,   181,   167,   151,   135,
   120,   102,   200,   190,   180,   166,   150,   134,   119,   101,
    92,   139,   133,   118,   100,    78,    79,   162,    82,   145,
   132,   117,    99,   116,    98,   214,   208,   198,   188,   177,
   161,   144,   130,   115,    97,    71,    96,    69,    68,    67,
    66,    70,    65,    57,    56,    55,    53,    51,    49,    47,
    45,    43,    41,    39,    37,    23,    21,     4,    10,     3,
     2,    24,     1,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    91,     0,     0,     0,     0,     0,    19,     0,
     0,     0,     0,   165 };
int yypact[]={

 -1000, -1000,  -156, -1000,  -156, -1000,  -199,  -200,  -201,  -202,
    66, -1000, -1000, -1000, -1000, -1000,    66, -1000,  -169, -1000,
 -1000,  -203, -1000,  -263, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000,  -204, -1000,  -205,
 -1000,  -206, -1000,  -207, -1000,  -208, -1000,  -209, -1000,  -210,
 -1000,  -211, -1000,  -212, -1000,    69,    63, -1000,  -255, -1000,
 -1000, -1000, -1000, -1000, -1000,    68,    92,    92,    92,  -213,
    62,    68, -1000,  -176,    61,    -9, -1000,  -214,    60,    59,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000,    86, -1000, -1000,  -215,  -216,  -217,  -218,
  -219,  -220,  -221,  -222,  -223,    58, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000,    90, -1000,  -224,  -225,  -226,  -227,  -228,
  -229,  -230, -1000, -1000,    57, -1000, -1000, -1000, -1000, -1000,
  -231, -1000,  -232,    67,  -233,  -234,  -235, -1000, -1000,    56,
  -170, -1000, -1000, -1000,  -236,  -237, -1000, -1000, -1000, -1000,
  -238,  -239,  -240, -1000, -1000,  -241,  -242,    66, -1000, -1000,
 -1000,  -243,  -244, -1000,    55,    48,  -245,  -246,  -247, -1000,
    54,  -248, -1000, -1000, -1000, -1000, -1000,  -249, -1000, -1000,
  -250,  -251,  -252, -1000,  -253, -1000, -1000, -1000,  -254, -1000,
  -257,  -258,  -259, -1000,  -260, -1000, -1000, -1000,  -269,    53,
  -276,  -277,  -279, -1000, -1000,    52, -1000,    51,  -280, -1000,
  -281, -1000, -1000, -1000,  -282,  -283,    50, -1000, -1000,  -284,
 -1000,  -285,    49, -1000 };
int yypgo[]={

     0,   222,   221,   220,   219,   218,   105,   217,   133,    92,
   216,   215,   214,   213,   212,   211,   210,   209,   208,   207,
   206,   205,   204,   203,   202,   201,   200,   107,   199,   198,
   197,   196,   195,   106,   194,   193,   192,   191,   190,   189,
   188,   187,   186,   185,   184,   183,   182,   181,   180,   179,
   177,   174,   173,   172,   171,   169,   168,   167,   166,   165,
   164,   163,   162,   161,   160,   159,   158,   157,   156,   155,
   154,   153,   152,   151,   150,   149,   148,   147,   146,   145,
   144,   143,   142,   141,   140,   139,   137,   136,   135,   134,
    94 };
int yyr1[]={

     0,     3,     5,     1,     4,     4,     7,     7,     8,     8,
     8,     8,     6,     6,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,     9,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    22,
    22,    24,    23,    26,    23,    28,    23,    29,    23,    30,
    31,    23,    25,    25,    32,    32,    34,    35,    36,    37,
    38,    39,    40,    41,    42,    43,    33,    44,    45,    33,
    46,    47,    48,    49,    50,    33,    51,    52,    53,    33,
    55,    56,    57,    58,    59,    60,    61,    62,    33,    63,
    64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
    74,    33,    75,    76,    77,    78,    79,    80,    81,    82,
    33,    83,    84,    85,    86,    54,    87,    54,    88,    54,
    27,    89,    89,    90 };
int yyr2[]={

     0,     1,     1,     8,     0,     2,     4,     2,     5,     5,
     5,     5,     4,     2,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,    55,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     0,
     2,     1,    11,     1,    11,     1,    11,     1,    11,     1,
     1,    15,     0,     2,     4,     2,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,    47,     1,     1,    15,
     1,     1,     1,     1,     1,    27,     1,     1,     1,    19,
     1,     1,     1,     1,     1,     1,     1,     1,    39,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,    55,     1,     1,     1,     1,     1,     1,     1,     1,
    39,     1,     1,     1,     1,    23,     1,    11,     1,    10,
     4,     4,     2,    11 };
int yychk[]={

 -1000,    -1,    -3,    -4,    -7,    -8,   257,   258,   259,   260,
    -5,    -8,   286,   286,   286,   286,    -6,    -9,    60,    -9,
   262,   -10,   286,   -11,    -2,   263,   274,   275,   276,   277,
   278,   279,   281,   282,   283,   284,   285,   -12,   286,   -13,
   286,   -14,   286,   -15,   286,   -16,   286,   -17,   286,   -18,
   286,   -19,   286,   -20,   286,   -21,   -22,   -23,    60,    62,
   263,   280,   278,   279,   285,   -24,   -26,   -28,   -29,   -30,
   -25,   -32,   -33,    60,   -27,   -89,   -90,    40,   -27,   -27,
   286,    62,   -33,   264,   265,   266,   267,   271,   272,   273,
    62,   261,   -90,   286,    62,    62,   -31,   -34,   -44,   -46,
   -51,   -55,   -63,   -75,    44,   286,   286,   286,   286,   286,
   286,   286,   286,   286,    62,   -35,   -45,   -47,   -52,   -56,
   -64,   -76,    41,   286,   286,   286,   286,   286,   286,   286,
   -36,    62,   -48,   -53,   -57,   -65,   -77,   286,   286,   -54,
    60,   286,   286,   286,   -37,   -49,    62,   268,   269,   270,
   -58,   -66,   -78,   286,   286,   -83,   -87,   -88,   286,   286,
   286,   -38,   -50,   286,   286,    -6,   -59,   -67,   -79,   286,
   286,   -84,    62,    62,   286,   286,   286,   -39,    62,   286,
   -60,   -68,   -80,   286,   -85,   286,   286,   286,   -40,   286,
   -61,   -69,   -81,   286,   -86,   286,   286,   286,   -41,   286,
   -62,   -70,   -82,   286,    62,   286,   286,   286,   -42,    62,
   -71,    62,   286,   286,   -43,   -72,   286,   286,    62,   -73,
   286,   -74,   286,    62 };
int yydef[]={

     1,    -2,     4,     2,     5,     7,     0,     0,     0,     0,
     0,     6,     8,     9,    10,    11,     3,    13,     0,    12,
    14,     0,    15,     0,    16,    27,    28,    29,    30,    31,
    32,    33,    34,    35,    36,    37,    38,     0,    17,     0,
    18,     0,    19,     0,    20,     0,    21,     0,    22,     0,
    23,     0,    24,     0,    25,    39,     0,    40,     0,    26,
    41,    43,    45,    47,    49,    52,     0,     0,     0,     0,
     0,    53,    55,     0,     0,     0,   122,     0,     0,     0,
    50,    42,    54,    56,    67,    70,    76,    80,    89,   102,
    44,   120,   121,     0,    46,    48,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    57,    68,    71,    77,
    81,    90,   103,     0,    51,     0,     0,     0,     0,     0,
     0,     0,   123,    58,     0,    72,    78,    82,    91,   104,
     0,    69,     0,     0,     0,     0,     0,    59,    73,     0,
     0,    83,    92,   105,     0,     0,    79,   111,   116,   118,
     0,     0,     0,    60,    74,     0,     0,     0,    84,    93,
   106,     0,     0,   112,     0,     0,     0,     0,     0,    61,
     0,     0,   117,   119,    85,    94,   107,     0,    75,   113,
     0,     0,     0,    62,     0,    86,    95,   108,     0,   114,
     0,     0,     0,    63,     0,    87,    96,   109,     0,     0,
     0,     0,     0,    64,   115,     0,    97,     0,     0,    88,
     0,   110,    65,    98,     0,     0,     0,    99,    66,     0,
   100,     0,     0,   101 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"DOCUMENTSIZE",	257,
	"PAGEWIDTH",	258,
	"PAGEHEIGHT",	259,
	"UNITS",	260,
	"NIL",	261,
	"OBJECT",	262,
	"GEOFRAME",	263,
	"REGION",	264,
	"RASTER_MAP",	265,
	"VECTOR_MAP",	266,
	"SITE_MAP",	267,
	"STANDARDSITE",	268,
	"PIXMAPSITE",	269,
	"FREEHANDSITE",	270,
	"GRID",	271,
	"BARSCALE",	272,
	"LEGEND",	273,
	"SQUARE",	274,
	"RECTANGLE",	275,
	"CIRCLE",	276,
	"ELLIPSE",	277,
	"POLYLINE",	278,
	"POLYGON",	279,
	"SPLINE",	280,
	"OPEN_INTERP_SPLINE",	281,
	"CLOSED_INTERP_SPLINE",	282,
	"OPEN_APPROX_SPLINE",	283,
	"CLOSED_APPROX_SPLINE",	284,
	"LABEL",	285,
	"String",	286,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"PageDescription : /* empty */",
	"PageDescription : OptionalGlobals",
	"PageDescription : OptionalGlobals ObjectList",
	"OptionalGlobals : /* empty */",
	"OptionalGlobals : Globals",
	"Globals : Globals GlobalElement",
	"Globals : GlobalElement",
	"GlobalElement : DOCUMENTSIZE String",
	"GlobalElement : PAGEWIDTH String",
	"GlobalElement : PAGEHEIGHT String",
	"GlobalElement : UNITS String",
	"ObjectList : ObjectList ObjectDescription",
	"ObjectList : ObjectDescription",
	"ObjectDescription : '<' OBJECT",
	"ObjectDescription : '<' OBJECT String",
	"ObjectDescription : '<' OBJECT String ObjectType",
	"ObjectDescription : '<' OBJECT String ObjectType String",
	"ObjectDescription : '<' OBJECT String ObjectType String String",
	"ObjectDescription : '<' OBJECT String ObjectType String String String",
	"ObjectDescription : '<' OBJECT String ObjectType String String String String",
	"ObjectDescription : '<' OBJECT String ObjectType String String String String String",
	"ObjectDescription : '<' OBJECT String ObjectType String String String String String String",
	"ObjectDescription : '<' OBJECT String ObjectType String String String String String String String",
	"ObjectDescription : '<' OBJECT String ObjectType String String String String String String String String",
	"ObjectDescription : '<' OBJECT String ObjectType String String String String String String String String String",
	"ObjectDescription : '<' OBJECT String ObjectType String String String String String String String String String OptionalSpecifics '>'",
	"ObjectType : GEOFRAME",
	"ObjectType : SQUARE",
	"ObjectType : RECTANGLE",
	"ObjectType : CIRCLE",
	"ObjectType : ELLIPSE",
	"ObjectType : POLYLINE",
	"ObjectType : POLYGON",
	"ObjectType : OPEN_INTERP_SPLINE",
	"ObjectType : CLOSED_INTERP_SPLINE",
	"ObjectType : OPEN_APPROX_SPLINE",
	"ObjectType : CLOSED_APPROX_SPLINE",
	"ObjectType : LABEL",
	"OptionalSpecifics : /* empty */",
	"OptionalSpecifics : Specifics",
	"Specifics : '<' GEOFRAME",
	"Specifics : '<' GEOFRAME OptionalGeoSpecifics '>'",
	"Specifics : '<' SPLINE",
	"Specifics : '<' SPLINE PointList '>'",
	"Specifics : '<' POLYLINE",
	"Specifics : '<' POLYLINE PointList '>'",
	"Specifics : '<' POLYGON",
	"Specifics : '<' POLYGON PointList '>'",
	"Specifics : '<' LABEL",
	"Specifics : '<' LABEL String",
	"Specifics : '<' LABEL String String '>'",
	"OptionalGeoSpecifics : /* empty */",
	"OptionalGeoSpecifics : GeoSpecificsList",
	"GeoSpecificsList : GeoSpecificsList GeoSpecifics",
	"GeoSpecificsList : GeoSpecifics",
	"GeoSpecifics : '<' REGION",
	"GeoSpecifics : '<' REGION String",
	"GeoSpecifics : '<' REGION String String",
	"GeoSpecifics : '<' REGION String String String",
	"GeoSpecifics : '<' REGION String String String String",
	"GeoSpecifics : '<' REGION String String String String String",
	"GeoSpecifics : '<' REGION String String String String String String",
	"GeoSpecifics : '<' REGION String String String String String String String",
	"GeoSpecifics : '<' REGION String String String String String String String String",
	"GeoSpecifics : '<' REGION String String String String String String String String String",
	"GeoSpecifics : '<' REGION String String String String String String String String String String '>'",
	"GeoSpecifics : '<' RASTER_MAP",
	"GeoSpecifics : '<' RASTER_MAP String",
	"GeoSpecifics : '<' RASTER_MAP String String '>'",
	"GeoSpecifics : '<' VECTOR_MAP",
	"GeoSpecifics : '<' VECTOR_MAP String",
	"GeoSpecifics : '<' VECTOR_MAP String String",
	"GeoSpecifics : '<' VECTOR_MAP String String String",
	"GeoSpecifics : '<' VECTOR_MAP String String String String",
	"GeoSpecifics : '<' VECTOR_MAP String String String String String '>'",
	"GeoSpecifics : '<' SITE_MAP",
	"GeoSpecifics : '<' SITE_MAP String",
	"GeoSpecifics : '<' SITE_MAP String String",
	"GeoSpecifics : '<' SITE_MAP String String SiteSpecifics '>'",
	"GeoSpecifics : '<' GRID",
	"GeoSpecifics : '<' GRID String",
	"GeoSpecifics : '<' GRID String String",
	"GeoSpecifics : '<' GRID String String String",
	"GeoSpecifics : '<' GRID String String String String",
	"GeoSpecifics : '<' GRID String String String String String",
	"GeoSpecifics : '<' GRID String String String String String String",
	"GeoSpecifics : '<' GRID String String String String String String String",
	"GeoSpecifics : '<' GRID String String String String String String String String '>'",
	"GeoSpecifics : '<' BARSCALE",
	"GeoSpecifics : '<' BARSCALE String",
	"GeoSpecifics : '<' BARSCALE String String",
	"GeoSpecifics : '<' BARSCALE String String String",
	"GeoSpecifics : '<' BARSCALE String String String String",
	"GeoSpecifics : '<' BARSCALE String String String String String",
	"GeoSpecifics : '<' BARSCALE String String String String String String",
	"GeoSpecifics : '<' BARSCALE String String String String String String String",
	"GeoSpecifics : '<' BARSCALE String String String String String String String String",
	"GeoSpecifics : '<' BARSCALE String String String String String String String String String",
	"GeoSpecifics : '<' BARSCALE String String String String String String String String String String",
	"GeoSpecifics : '<' BARSCALE String String String String String String String String String String String",
	"GeoSpecifics : '<' BARSCALE String String String String String String String String String String String String '>'",
	"GeoSpecifics : '<' LEGEND",
	"GeoSpecifics : '<' LEGEND String",
	"GeoSpecifics : '<' LEGEND String String",
	"GeoSpecifics : '<' LEGEND String String String",
	"GeoSpecifics : '<' LEGEND String String String String",
	"GeoSpecifics : '<' LEGEND String String String String String",
	"GeoSpecifics : '<' LEGEND String String String String String String",
	"GeoSpecifics : '<' LEGEND String String String String String String String",
	"GeoSpecifics : '<' LEGEND String String String String String String String String '>'",
	"SiteSpecifics : '<' STANDARDSITE",
	"SiteSpecifics : '<' STANDARDSITE String",
	"SiteSpecifics : '<' STANDARDSITE String String",
	"SiteSpecifics : '<' STANDARDSITE String String String",
	"SiteSpecifics : '<' STANDARDSITE String String String String '>'",
	"SiteSpecifics : '<' PIXMAPSITE",
	"SiteSpecifics : '<' PIXMAPSITE String '>'",
	"SiteSpecifics : '<' FREEHANDSITE",
	"SiteSpecifics : '<' FREEHANDSITE ObjectList '>'",
	"PointList : Points NIL",
	"Points : Points Point",
	"Points : Point",
	"Point : '(' String ',' String ')'",
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
# line 108 "grammar.y"
{
          bzero((char *)&parserGlobals, sizeof(XgdDisplayParserGlobals));
      } break;
case 2:
# line 112 "grammar.y"
{
          if ( parserGlobals.unitsSet ) {
              /* XXX */;
          }
          if ( parserGlobals.docSizeSet ) {
              if ( parserGlobals.pageWidthSet ) {
          /* XXX */;
              }
              if ( parserGlobals.pageHeightSet ) {
          /* XXX */;
              }
          }
      } break;
case 8:
# line 140 "grammar.y"
{
          parserGlobals.docSizeSet = True;
          parserGlobals.docSize = malloc(strlen(yypvt[-0].cval) + 1);
          strcpy(parserGlobals.docSize, yypvt[-0].cval);
          if (__DEBUG ) fprintf(stderr, " Documentsize -> %s\n", yypvt[-0].cval);
      } break;
case 9:
# line 147 "grammar.y"
{
          double pageWidth;
          char *copy = malloc(strlen(yypvt[-0].cval) + 1);
          char *ptr = NULL;

          if ( (pageWidth = (double)strtod(yypvt[-0].cval, &ptr, 10)) == 0 ) {
              if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                  yyerror("Illegal type: PAGEWIDTH must be an integer/real");
                  return;
              }
          }
          strcpy(copy, yypvt[-0].cval);
          if ( G_rindex(copy,'.') != NULL ) {
              if (__DEBUG ) fprintf(stderr, " PageWidth -> %lf\n", pageWidth);
          } else {
              if (__DEBUG ) fprintf(stderr, " PageWidth -> %d\n", (int)pageWidth);
          }
          parserGlobals.pageWidthSet = True;
          parserGlobals.pageWidth = pageWidth;
          free(copy);
      } break;
case 10:
# line 169 "grammar.y"
{
          double pageHeight;
          char *copy = malloc(strlen(yypvt[-0].cval) + 1);
          char *ptr = NULL;

          if ( (pageHeight = (double)strtod(yypvt[-0].cval, &ptr, 10)) == 0 ) {
              if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
		  yyerror("Illegal type: PAGEHEIGHT must be an integer/real");
		  return;
              }
          }
          strcpy(copy, yypvt[-0].cval);
          if ( G_rindex(copy,'.') != NULL ) {
              if (__DEBUG ) fprintf(stderr, " PageHeight -> %lf\n", pageHeight);
          } else {
              if (__DEBUG ) fprintf(stderr, " PageHeight -> %d\n", (int)pageHeight);
          }
          parserGlobals.pageHeightSet = True;
          parserGlobals.pageHeight = pageHeight;
          free(copy);
      } break;
case 11:
# line 195 "grammar.y"
{
          char *units;
          char errorbuf[1024];

          if ( !strcmp(yypvt[-0].cval,"mm") || !strcmp(yypvt[-0].cval,"millimeters") ) {
              if (__DEBUG ) fprintf(stderr,"Units -> millimeters\n");
          } else if ( !strcmp(yypvt[-0].cval,"in") || !strcmp(yypvt[-0].cval,"inches") ) {
              if (__DEBUG ) fprintf(stderr,"Units -> inches\n");
          } else if ( !strcmp(yypvt[-0].cval,"pixel") || !strcmp(yypvt[-0].cval,"pixels") ) {
              if (__DEBUG ) fprintf(stderr,"Units -> pixels\n");
          } else {
              yywarning("Illegal value: UNITS must be one of:");
              if (__DEBUG ) fprintf(stderr,"\t\"mm\", \"millimeters\"\n");
              if (__DEBUG ) fprintf(stderr,"\t\"in\", \"inches\"\n");
              if (__DEBUG ) fprintf(stderr,"\t\"pixel\", \"pixels\"\n");
              exit(1);
          }
          parserGlobals.unitsSet = True;
          parserGlobals.units = malloc(strlen(yypvt[-0].cval) + 1);
          strcpy(parserGlobals.units, yypvt[-0].cval);
      } break;
case 14:
# line 226 "grammar.y"
{
              if (__DEBUG ) fprintf(stderr, "PARSING OBJECT DESCRIPTION\n");
              specificsSet = False;
          } break;
case 15:
# line 231 "grammar.y"
{
              if (__DEBUG ) fprintf(stderr,"\toid = %s\n", yypvt[-0].cval);
          } break;
case 16:
# line 235 "grammar.y"
{
              curObject = XgdCreateObject(yypvt[-0].ival);
          } break;
case 17:
# line 239 "grammar.y"
{
              int x;
              char *ptr = NULL;

              if ( (x = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: x position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tx = %d\n", x);
              templateObject.x = x;
          } break;
case 18:
# line 253 "grammar.y"
{
              int y;
              char *ptr;

              if ( (y = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: y position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\ty = %d\n", y);
              templateObject.y = y;
          } break;
case 19:
# line 267 "grammar.y"
{
              int width;
              char *ptr;

              if ( (width = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: width must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\twidth = %d\n", width);
              templateObject.width = width;
          } break;
case 20:
# line 281 "grammar.y"
{
              int height;
              char *ptr;

              if ( (height = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: height must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\theight = %d\n", height);
              templateObject.height = height;
          } break;
case 21:
# line 297 "grammar.y"
{
              int fillpat;
              char *ptr;

              if ( (fillpat = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: fill pattern must be an integer");
                      return;
                  }
              }
              if ( fillpat < 0 || fillpat > XGD_FILL_PATTERN_MAXIMUM ) {
                  fprintf(stderr,
                      "Warning: Fill pattern outside of range [%d-%d].\n",
                      0, XGD_FILL_PATTERN_MAXIMUM);
                  fillpat = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tfillpat = %d\n", fillpat);
              templateObject.fp = fillpat;
          } break;
case 22:
# line 319 "grammar.y"
{
              int linepat;
              char *ptr;

              if ( (linepat = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if ( linepat < 0 || linepat > XGD_LINE_PATTERN_MAXIMUM ) {
                  fprintf(stderr,
                      "Warning: Line pattern outside of range [%d-%d].\n",
                      0, XGD_LINE_PATTERN_MAXIMUM);
                  linepat = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlinepat = %d\n", linepat);
              templateObject.lp = linepat;
          } break;
case 23:
# line 339 "grammar.y"
{
              int linewidth;
              char *ptr;

              if ( (linewidth = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: line width must be an integer");
                      return;
                  }
              }
              if ( linewidth < 0 ) {
                  fprintf(stderr,
                      "Warning: Line width must be non-negative\n");
                  linewidth = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlinewidth = %d\n", linewidth);
              templateObject.lw = linewidth;
          } break;
case 24:
# line 361 "grammar.y"
{
              if (__DEBUG ) fprintf(stderr,"\tfg = %s\n", yypvt[-0].cval);
              templateObject.fg = XgdGetVectColorPixelByName(yypvt[-0].cval);
          } break;
case 25:
# line 370 "grammar.y"
{
              templateObject.bg = XgdGetVectColorPixelByName(yypvt[-0].cval);
              if (__DEBUG ) fprintf(stderr,"\tbg = %s\n", yypvt[-0].cval);

              XgdInitObject(Global.display, XtWindow(Global.drawArea), 
                curObject, templateObject.fp, templateObject.lp,
                templateObject.fg, templateObject.bg, 
                templateObject.lw);
          } break;
case 26:
# line 380 "grammar.y"
{
              XgdObject *obj;
              if ( geoFrame ) {
                  obj = geoFrameObject;
              } else {
                  obj = curObject;
              }

              if ( TYPE_WITH_SPECIFICS(obj) && !specificsSet ) {
                  yyerror("Missing specifics...");
              }
              if ( yyerrflag ) {
                  return;
              }
              if ( geoFrame ) {
		  XgdConfigureObject(XgdGetGCOfObject(obj), 
                    obj, templateObject.x, templateObject.y, 
                    templateObject.width, templateObject.height, True);
                  geoFrame = False;
              } else if ( GEOFRAME_OBJECT_TYPE(obj) ) {
		  XgdConfigureObject(XgdGetGCOfObject(obj), obj, 
		    obj->x, obj->y, obj->width, 
                    obj->height, True);
              } else {
		  XgdConfigureObject(XgdGetGCOfObject(obj), obj, 
		    templateObject.x, templateObject.y, templateObject.width,
		    templateObject.height, True);
              }
	      /*AddObjectToList(&Global.selectedObjects, obj);*/
	      AddObjectToList(&Global.objectList, obj);
	      if ( GEOFRAME_OBJECT_TYPE(obj) ) {
		XgdRedrawGeoframe(obj, TRUE, TRUE, NULL);
	      } else
		XgdDrawObject(XgdGetGCOfObject(obj), obj, True, NULL);
	      /*XgdDrawResizeHandles(obj, Global.xorGC);*/

              if (__DEBUG ) fprintf(stderr, "OBJECT COMPLETE\n");
          } break;
case 27:
# line 422 "grammar.y"
{ 
           yyval.ival = XGD_GEOFRAME; 
       } break;
case 28:
# line 426 "grammar.y"
{ 
           yyval.ival = XGD_SQUARE; 
       } break;
case 29:
# line 430 "grammar.y"
{ 
           yyval.ival = XGD_RECTANGLE; 
       } break;
case 30:
# line 434 "grammar.y"
{ 
           yyval.ival = XGD_CIRCLE; 
       } break;
case 31:
# line 438 "grammar.y"
{ 
           yyval.ival = XGD_ELLIPSE; 
       } break;
case 32:
# line 442 "grammar.y"
{ 
           yyval.ival = XGD_POLYLINE; 
       } break;
case 33:
# line 446 "grammar.y"
{ 
           yyval.ival = XGD_POLYGON; 
       } break;
case 34:
# line 450 "grammar.y"
{ 
           yyval.ival = XGD_OPEN_INTERP_SPLINE; 
       } break;
case 35:
# line 454 "grammar.y"
{ 
           yyval.ival = XGD_CLOSED_INTERP_SPLINE; 
       } break;
case 36:
# line 458 "grammar.y"
{ 
           yyval.ival = XGD_OPEN_APPROX_SPLINE; 
       } break;
case 37:
# line 462 "grammar.y"
{ 
           yyval.ival = XGD_CLOSED_APPROX_SPLINE; 
       } break;
case 38:
# line 466 "grammar.y"
{ 
           yyval.ival = XGD_LABEL; 
       } break;
case 41:
# line 479 "grammar.y"
{
              if (__DEBUG ) fprintf(stderr,"GeoFrame Specifics\n");
	      /*  
	       * set geoFrameObject to point to curObject so we can use 
	       * curobject in subobject specifications...
	       */
	      geoFrameObject = curObject;
          } break;
case 42:
# line 488 "grammar.y"
{
              geoFrame = True;
              specificsSet = True;
          } break;
case 43:
# line 494 "grammar.y"
{
              if (__DEBUG ) fprintf(stderr,"Spline Specifics\n");
          } break;
case 44:
# line 498 "grammar.y"
{
              specificsSet = True;
          } break;
case 45:
# line 503 "grammar.y"
{
              if (__DEBUG ) fprintf(stderr,"Polyline Specifics\n");
          } break;
case 46:
# line 507 "grammar.y"
{
              specificsSet = True;
          } break;
case 47:
# line 512 "grammar.y"
{
              if (__DEBUG ) fprintf(stderr,"Polygon Specifics\n");
          } break;
case 48:
# line 516 "grammar.y"
{
              specificsSet = True;
          } break;
case 49:
# line 521 "grammar.y"
{
              if (__DEBUG ) fprintf(stderr,"Label Specifics\n");
          } break;
case 50:
# line 525 "grammar.y"
{
              ParseString(yypvt[-0].cval, &curObject->Obj.Label.numlines, 
                              &curObject->Obj.Label.lblstr);
          } break;
case 51:
# line 530 "grammar.y"
{
              if ((curObject->Obj.Label.font = 
                   XLoadQueryFont(Global.display, yypvt[-1].cval)) == NULL ) {
                  char buf[1024];
    
                  sprintf (buf, 
                     "Sorry, the selected font:\n[%s]\nIs not available.\n",
                     yypvt[-1].cval);
                  yyerror(buf);
                  return;
              }
              XgdSetLabelFont(curObject, curObject->Obj.Label.font, False);
              XgdSetLabelFontName(curObject, yypvt[-1].cval);
              specificsSet = True;
          } break;
case 56:
# line 561 "grammar.y"
{
              ;
          } break;
case 57:
# line 568 "grammar.y"
{
              int projection;
              char *ptr;

              if ( (projection = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if ( !(projection == PROJECTION_XY ||
                    projection == PROJECTION_UTM ||
                    projection == PROJECTION_SP ||
                    projection == PROJECTION_LL ||
                    projection == PROJECTION_OTHER) ) {
                    yywarning("Illegal projection in region specification.");
                    projection = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tprojection = %d\n", projection);
              curObject->Obj.GeoFrame.region.proj = projection;
          } break;
case 58:
# line 590 "grammar.y"
{
              int zone;
              char *ptr;

              if ( (zone = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: zone must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tzone = %d\n", zone);
              curObject->Obj.GeoFrame.region.zone = zone;
          } break;
case 59:
# line 604 "grammar.y"
{
	      double ew_res;
	      char *ptr = NULL;

	      if ( (ew_res = (double)strtod(yypvt[-0].cval, &ptr, 10)) == 0 ) {
		  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
		  yyerror("Illegal type: east-west res. must be an int/real");
		  return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\tew_res = %lf\n", ew_res);
              curObject->Obj.GeoFrame.region.ew_res = ew_res;
          } break;
case 60:
# line 618 "grammar.y"
{
	      double ns_res;
	      char *ptr = NULL;

	      if ( (ns_res = (double)strtod(yypvt[-0].cval, &ptr, 10)) == 0 ) {
		  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
		  yyerror("Illegal type: north-south res. must be an int/real");
		  return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\tns_res = %lf\n", ns_res);
              curObject->Obj.GeoFrame.region.ns_res = ns_res;
          } break;
case 61:
# line 632 "grammar.y"
{
	      double north;
	      char *ptr = NULL;

	      if ( (north = (double)strtod(yypvt[-0].cval, &ptr, 10)) == 0 ) {
		  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
		      yyerror("Illegal type: north edge must be an int/real");
		      return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\tnorth = %lf\n", north);
              curObject->Obj.GeoFrame.region.north = north;
          } break;
case 62:
# line 646 "grammar.y"
{
	      double south;
	      char *ptr = NULL;

	      if ( (south = (double)strtod(yypvt[-0].cval, &ptr, 10)) == 0 ) {
		  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
		      yyerror("Illegal type: south edge must be an int/real");
		      return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\tsouth = %lf\n", south);
              curObject->Obj.GeoFrame.region.south = south;
          } break;
case 63:
# line 660 "grammar.y"
{
	      double east;
	      char *ptr = NULL;

	      if ( (east = (double)strtod(yypvt[-0].cval, &ptr, 10)) == 0 ) {
		  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
		      yyerror("Illegal type: east edge must be an int/real");
		      return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\teast = %lf\n", east);
              curObject->Obj.GeoFrame.region.east = east;
          } break;
case 64:
# line 674 "grammar.y"
{
	      double west;
	      char *ptr = NULL;

	      if ( (west = (double)strtod(yypvt[-0].cval, &ptr, 10)) == 0 ) {
		  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
		      yyerror("Illegal type: west edge must be an int/real");
		      return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\twest = %lf\n", west);
              curObject->Obj.GeoFrame.region.west = west;
          } break;
case 65:
# line 688 "grammar.y"
{
              int rows;
              char *ptr;

              if ( (rows = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: rows must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\trows = %d\n", rows);
              curObject->Obj.GeoFrame.region.rows = rows;
          } break;
case 66:
# line 702 "grammar.y"
{
              int cols;
              char *ptr;

              if ( (cols = (int)strtol(yypvt[-1].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-1].cval, ptr) ) {
                      yyerror("Illegal type: cols must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tcols = %d\n", cols);
              curObject->Obj.GeoFrame.region.cols = cols;
          } break;
case 67:
# line 717 "grammar.y"
{
              ;
          } break;
case 68:
# line 721 "grammar.y"
{
              curObject->Obj.GeoFrame.rmapset = XtNewString(yypvt[-0].cval);
          } break;
case 69:
# line 725 "grammar.y"
{
              curObject->Obj.GeoFrame.rname = XtNewString(yypvt[-1].cval);
          } break;
case 70:
# line 730 "grammar.y"
{
              ;
          } break;
case 71:
# line 734 "grammar.y"
{
              vectTempl.mapset = XtNewString(yypvt[-0].cval);
          } break;
case 72:
# line 738 "grammar.y"
{
              vectTempl.name = XtNewString(yypvt[-0].cval);
          } break;
case 73:
# line 742 "grammar.y"
{
              vectTempl.fg = XgdGetVectColorPixelByName(yypvt[-0].cval);
          } break;
case 74:
# line 746 "grammar.y"
{
              int linepat;
              char *ptr;

              if ( (linepat = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if ( linepat < 0 || linepat > XGD_LINE_PATTERN_MAXIMUM ) {
                  fprintf(stderr,
                      "Warning: Line pattern outside of range [%d-%d].\n",
                      0, XGD_LINE_PATTERN_MAXIMUM);
                  linepat = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlinepat = %d\n", linepat);
              vectTempl.lp = linepat;
          } break;
case 75:
# line 766 "grammar.y"
{
              int linewidth;
              char *ptr;

              if ( (linewidth = (int)strtol(yypvt[-1].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-1].cval, ptr) ) {
                      yyerror("Illegal type: line width must be an integer");
                      return;
                  }
              }
              if ( linewidth < 0 ) {
                  fprintf(stderr,
                      "Warning: Line width must be non-negative\n");
                  linewidth = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlinewidth = %d\n", linewidth);
              vectTempl.lw = linewidth;

              XgdInitVector(curObject, vectTempl.name, vectTempl.mapset,
                  vectTempl.fg, vectTempl.lp, vectTempl.lw);
          } break;
case 76:
# line 789 "grammar.y"
{
              ;
          } break;
case 77:
# line 793 "grammar.y"
{
              siteTempl.name = XtNewString(yypvt[-0].cval);
fprintf(stderr,"DOING A SITE MAP\nname = %s\n",  siteTempl.name);
          } break;
case 78:
# line 798 "grammar.y"
{
              siteTempl.mapset = XtNewString(yypvt[-0].cval);
fprintf(stderr,"mapset = %s\n",  siteTempl.mapset);
          } break;
case 79:
# line 803 "grammar.y"
{
              ;
          } break;
case 80:
# line 808 "grammar.y"
{
            geoFrameObject->Obj.GeoFrame.gridOn = True;
	    if ( geoFrameObject->Obj.GeoFrame.grid.gc == NULL ) {
		XGCValues gcv;

		gcv.background = geoFrameObject->bg;
		geoFrameObject->Obj.GeoFrame.grid.gc =
		    XCreateGC(Global.display, XtWindow(Global.drawArea), 0, 0);
	    }
          } break;
case 81:
# line 819 "grammar.y"
{
              int labelon;
              char *ptr;

              if ( (labelon = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if ( !(labelon == 1 ||
                    labelon == 0) ) {
                    yywarning("Illegal label on/off flag in grid specification.");
                    labelon = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlabelon = %d\n", labelon);
              XgdSetGridLabelOn(geoFrameObject, labelon);
          } break;
case 82:
# line 838 "grammar.y"
{
              double gap;
              char *ptr = NULL;

              if ( (gap = (double)strtod(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
		      yyerror("Illegal type: grid gap must be an int/real");
		      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tgap = %lf\n", gap);
              XgdSetGridGap(geoFrameObject, gap);
          } break;
case 83:
# line 852 "grammar.y"
{
              int spacing;
              char *ptr;

              if ( (spacing = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tspacing = %d\n", spacing);
              XgdSetGridSpacing(geoFrameObject, spacing);
          } break;
case 84:
# line 866 "grammar.y"
{
              Pixel color  = XgdGetVectColorPixelByName(yypvt[-0].cval);
              if (__DEBUG ) fprintf(stderr,"\tcolor = %s\n", yypvt[-0].cval);
	      XgdSetGridColor(geoFrameObject, color);
          } break;
case 85:
# line 872 "grammar.y"
{
              int linepat;
              char *ptr;

              if ( (linepat = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if ( linepat < 0 || linepat > XGD_LINE_PATTERN_MAXIMUM ) {
                  fprintf(stderr,
                      "Warning: Line pattern outside of range [%d-%d].\n",
                      0, XGD_LINE_PATTERN_MAXIMUM);
                  linepat = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlinepat = %d\n", linepat);
              gridLinePattern = linepat;
          } break;
case 86:
# line 892 "grammar.y"
{
              int linewidth;
              char *ptr;

              if ( (linewidth = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: line width must be an integer");
                      return;
                  }
              }
              if ( linewidth < 0 ) {
                  fprintf(stderr,
                      "Warning: Line width must be non-negative\n");
                  linewidth = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlinewidth = %d\n", linewidth);
              XgdSetGridLinePattern(geoFrameObject, linewidth, gridLinePattern);
          } break;
case 87:
# line 911 "grammar.y"
{
              XFontStruct *fs;
              int xoff = 0, yoff = 0;

              if ((fs = XLoadQueryFont(Global.display, yypvt[-0].cval)) == NULL ) {
                  char buf[1024];

                  sprintf (buf,
                     "Sorry, the selected font:\t\n[%s]\t\nIs not available.",
                     yypvt[-0].cval);
                  yyerror(buf);
                  return;
              }
              XgdSetGridFont(geoFrameObject, fs->fid, False);
              XgdSetGridFontName(geoFrameObject, yypvt[-0].cval);
              if ( geoFrameObject->Obj.GeoFrame.grid.labelon )
                  XgdCalculateGridOffset(geoFrameObject, &xoff, &yoff);
              XgdSetGridOffset(geoFrameObject, xoff, yoff);
          } break;
case 88:
# line 931 "grammar.y"
{
              Pixel color  = XgdGetVectColorPixelByName(yypvt[-1].cval);
              if (__DEBUG ) fprintf(stderr,"\ttextcolor = %s\n", yypvt[-1].cval);
	      XgdSetGridTextColor(geoFrameObject, color);
          } break;
case 89:
# line 938 "grammar.y"
{
	      /* create a new object */
	      curObject = XgdCreateObject(XGD_BARSCALE);
              XgdInitObject(Global.display, XtWindow(Global.drawArea), 
                curObject, Global.fillPattern, Global.linePattern,
                Global.foreground, Global.background, 
                Global.lineWidth);
          } break;
case 90:
# line 947 "grammar.y"
{
              int x;
              char *ptr = NULL;

              if ( (x = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: x position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tx = %d\n", x);
	      curObject->x = x;
          } break;
case 91:
# line 961 "grammar.y"
{
              int y;
              char *ptr = NULL;

              if ( (y = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: y position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\ty = %d\n", y);
	      curObject->y = y;
          } break;
case 92:
# line 975 "grammar.y"
{
              int width;
              char *ptr = NULL;

              if ( (width = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: width must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\twidth = %d\n", width);
              curObject->width = width;
          } break;
case 93:
# line 989 "grammar.y"
{
              int height;
              char *ptr = NULL;

              if ( (height = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: height must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\theight = %d\n", height);
              curObject->height = height;
          } break;
case 94:
# line 1003 "grammar.y"
{
              int style;
              char *ptr;

              if ( (style = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if ( !(style == DASHED ||
                    style == TICKED) ) {
                    yywarning("Illegal style in barscale specification.");
                    style = DASHED;
              }
              if (__DEBUG ) fprintf(stderr,"\tstyle = %d\n", style);
              XgdSetBarscaleStyle(curObject, style);
          } break;
case 95:
# line 1023 "grammar.y"
{
	      double length;
	      char *ptr = NULL;

	      if ( (length = (double)strtod(yypvt[-0].cval, &ptr, 10)) == 0 ) {
		  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
		  yyerror("Illegal type: barscale length must be an int/real");
		  return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\tlength = %lf\n", length);
	      XgdSetBarscaleLength(curObject, length);
          } break;
case 96:
# line 1037 "grammar.y"
{
              int intervals;
              char *ptr = NULL;

              if ( (intervals = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: intervals must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tintervals = %d\n", intervals);
	      XgdSetBarscaleInterval(curObject, intervals);
          } break;
case 97:
# line 1051 "grammar.y"
{
              int units;
              char *ptr;

              if ( (units = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if ( !(units == XGD_KILOMETERS ||
                    units == XGD_METERS ||
                    units == XGD_MILES ||
                    units == XGD_FEET) ) {
                    yywarning("Illegal units in barscale specification.");
                    units = XGD_KILOMETERS;
              }
              if (__DEBUG ) fprintf(stderr,"\tunits = %d\n", units);
	      XgdSetBarscaleUnit(curObject, units);
          } break;
case 98:
# line 1072 "grammar.y"
{ 
              int linewidth;
              char *ptr;

              if ( (linewidth = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: line width must be an integer");
                      return;
                  }
              }
              if ( linewidth < 0 ) {
                  fprintf(stderr,
                      "Warning: Line width must be non-negative\n");
                  linewidth = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlinewidth = %d\n", linewidth);
	      XgdSetBarscaleLineWidth(curObject, linewidth);
          } break;
case 99:
# line 1091 "grammar.y"
{
              Pixel color  = XgdGetVectColorPixelByName(yypvt[-0].cval);
              if (__DEBUG ) fprintf(stderr,"\tcolor = %s\n", yypvt[-0].cval);
	      XgdSetBarscaleColor(curObject, color);
          } break;
case 100:
# line 1097 "grammar.y"
{
	      XFontStruct *fs;

              if ((fs = XLoadQueryFont(Global.display, yypvt[-0].cval)) == NULL ) {
                  char buf[1024];

                  sprintf (buf,
                     "Sorry, the selected font:\t\n[%s]\t\nIs not available.",
                     yypvt[-0].cval);
                  yyerror(buf);
                  return;
              }
              XgdSetBarscaleFont(curObject, fs->fid, False);
              XgdSetBarscaleFontName(curObject, yypvt[-0].cval);
          } break;
case 101:
# line 1113 "grammar.y"
{
              Pixel textcolor = XgdGetVectColorPixelByName(yypvt[-1].cval);
              if (__DEBUG ) fprintf(stderr,"\ttextcolor = %s\n", yypvt[-1].cval);
	      XgdSetBarscaleTextColor(curObject, textcolor);
	      /* set the window width */
              if ( geoFrameObject->Obj.GeoFrame.gridOn &&
                   geoFrameObject->Obj.GeoFrame.grid.labelon ) {
                  XgdSetBarscaleWinWidth(curObject, geoFrameObject->width - 
                      geoFrameObject->Obj.GeoFrame.grid.xoff);
              } else {
                  XgdSetBarscaleWinWidth(curObject, geoFrameObject->width);
              }
	      /* add the barscale into the geoFrameObject... */
              if (geoFrameObject->Obj.GeoFrame.numbarscales != 0) {
		int i;
		int oldcount = geoFrameObject->Obj.GeoFrame.numbarscales;
		XgdObject **newbs =
		    (XgdObject **)XtCalloc(oldcount + 1, sizeof(XgdObject *));

		for (i = 0; i < geoFrameObject->Obj.GeoFrame.numbarscales; i++ )
		    newbs[i] = geoFrameObject->Obj.GeoFrame.barscales[i];
		newbs[i] = curObject;
		XtFree(geoFrameObject->Obj.GeoFrame.barscales);
		geoFrameObject->Obj.GeoFrame.barscales = newbs;
		geoFrameObject->Obj.GeoFrame.numbarscales++;
	       } else {
		geoFrameObject->Obj.GeoFrame.barscales = (XgdObject **)
		    XtCalloc(1, sizeof(XgdObject*));
		geoFrameObject->Obj.GeoFrame.barscales[0] = curObject;
		geoFrameObject->Obj.GeoFrame.numbarscales++;
	       }

	      /*AddObjectToList(&Global.selectedObjects, curObject);*/
	      AddObjectToList(&Global.objectList, curObject);
              /*XgdDrawBarscale(curObject, True);*/
	      XgdConfigureObject(XgdGetGCOfObject(curObject), curObject, 
		curObject->x, curObject->y, curObject->width, 
		curObject->height, True);
	      /*XgdDrawResizeHandles(curObject, Global.xorGC);*/
              specificsSet = True;
          } break;
case 102:
# line 1156 "grammar.y"
{
	      /* create a new object */
	      curObject = XgdCreateObject(XGD_LEGEND);
              XgdInitObject(Global.display, XtWindow(Global.drawArea), 
                curObject, Global.fillPattern, Global.linePattern,
                Global.foreground, Global.background, 
                Global.lineWidth);
          } break;
case 103:
# line 1165 "grammar.y"
{
              int x;
              char *ptr = NULL;

              if ( (x = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: x position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tx = %d\n", x);
	      curObject->x = x;
          } break;
case 104:
# line 1179 "grammar.y"
{
              int y;
              char *ptr = NULL;

              if ( (y = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: y position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\ty = %d\n", y);
	      curObject->y = y;
          } break;
case 105:
# line 1193 "grammar.y"
{
              ;
          } break;
case 106:
# line 1197 "grammar.y"
{
              ;
          } break;
case 107:
# line 1201 "grammar.y"
{
              ;
          } break;
case 108:
# line 1205 "grammar.y"
{
              ;
          } break;
case 109:
# line 1209 "grammar.y"
{
              ;
          } break;
case 110:
# line 1213 "grammar.y"
{
              ;
          } break;
case 111:
# line 1220 "grammar.y"
{
              siteTempl.type = XGD_SITE_STANDARD;
          } break;
case 112:
# line 1224 "grammar.y"
{
              int icontype;
              char *ptr;

              if ( (icontype = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: icontype must be an integer");
                      return;
                  }
              }
              if ( icontype != CROSS && icontype != DIAMOND && 
                   icontype != RECT && icontype != PLUS ) {
                  fprintf(stderr,
                      "Warning: Invalid icon type\n");
                  icontype = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\ticontype = %d\n", icontype);
              siteTempl.icontype = icontype;
          } break;
case 113:
# line 1244 "grammar.y"
{
              int size;
              char *ptr;

              if ( (size = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: size must be an integer");
                      return;
                  }
              }
              if ( size < 0 ) {
                  fprintf(stderr,
                      "Warning: Size must be non-negative\n");
                  size = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tsize = %d\n", size);
              siteTempl.size = size;
          } break;
case 114:
# line 1263 "grammar.y"
{
              int linewidth;
              char *ptr;

              if ( (linewidth = (int)strtol(yypvt[-0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yypvt[-0].cval, ptr) ) {
                      yyerror("Illegal type: line width must be an integer");
                      return;
                  }
              }
              if ( linewidth < 0 ) {
                  fprintf(stderr,
                      "Warning: Line width must be non-negative\n");
                  linewidth = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlinewidth = %d\n", linewidth);
              siteTempl.linewidth = linewidth;
          } break;
case 115:
# line 1282 "grammar.y"
{
              siteTempl.color  = XgdGetVectColorPixelByName(yypvt[-1].cval);
              if (__DEBUG ) fprintf(stderr,"\tcolor = %s\n", yypvt[-1].cval);
              XgdInitSite(geoFrameObject, siteTempl.name, siteTempl.mapset, 
                  siteTempl.type);
              XgdUpdateSiteStd(geoFrameObject->Obj.GeoFrame.sites.Tail,
                  siteTempl.icontype, siteTempl.size, siteTempl.linewidth,
                  siteTempl.color);
          } break;
case 116:
# line 1293 "grammar.y"
{
              siteTempl.type = XGD_SITE_PIXMAP;
          } break;
case 117:
# line 1297 "grammar.y"
{
              siteTempl.pixmapfile = XtNewString(yypvt[-1].cval);

              XgdInitSite(geoFrameObject, siteTempl.name, siteTempl.mapset, 
                  siteTempl.type);
              XgdDrawSite(geoFrameObject, XtWindow(Global.drawArea),
			  siteTempl.name, siteTempl.mapset,
			  siteTempl.pixmapfile);
          } break;
case 118:
# line 1308 "grammar.y"
{
              ;
          } break;
case 123:
# line 1324 "grammar.y"
{
          int x, y;
          char *ptr = NULL;

          if ( (x = (int)strtol(yypvt[-3].cval, &ptr, 10)) == 0 ) {
              if ( ptr != NULL && !strcmp(yypvt[-3].cval, ptr) ) {
              yyerror("Illegal type: x value in point must be an integer");
                      return;
              }
          }
          if ( (y = (int)strtol(yypvt[-1].cval, &ptr, 10)) == 0 ) {
              if ( ptr != NULL && !strcmp(yypvt[-1].cval, ptr) ) {
              yyerror("Illegal type: x value in point must be an integer");
                      return;
              }
          }
          XgdAddPointToPointList(curObject, x, y);
      } break;
	}
	goto yystack;		/* reset registers in driver code */
}
