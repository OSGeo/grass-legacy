#ifndef lint
/*static char yysccsid[] = "from: @(#)yaccpar	1.9 (Berkeley) 02/21/93";*/
static char yyrcsid[] = "$Id: skeleton.c,v 1.4 1993/12/21 18:45:32 jtc Exp $";
#endif
#define YYBYACC 1
#define YYMAJOR 1
#define YYMINOR 9
#define yyclearin (yychar=(-1))
#define yyerrok (yyerrflag=0)
#define YYRECOVERING (yyerrflag!=0)
#define YYPREFIX "yy"
#line 5 "grammar.y"

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

#line 64 "grammar.y"
typedef union {
 char *cval;
 int ival;
} YYSTYPE;
#line 76 "y.tab.c"
#define DOCUMENTSIZE 257
#define PAGEWIDTH 258
#define PAGEHEIGHT 259
#define UNITS 260
#define NIL 261
#define OBJECT 262
#define GEOFRAME 263
#define REGION 264
#define RASTER_MAP 265
#define VECTOR_MAP 266
#define SITE_MAP 267
#define STANDARDSITE 268
#define PIXMAPSITE 269
#define FREEHANDSITE 270
#define GRID 271
#define BARSCALE 272
#define LEGEND 273
#define SQUARE 274
#define RECTANGLE 275
#define CIRCLE 276
#define ELLIPSE 277
#define POLYLINE 278
#define POLYGON 279
#define SPLINE 280
#define OPEN_INTERP_SPLINE 281
#define CLOSED_INTERP_SPLINE 282
#define OPEN_APPROX_SPLINE 283
#define CLOSED_APPROX_SPLINE 284
#define LABEL 285
#define String 286
#define YYERRCODE 256
short yylhs[] = {                                        -1,
    3,    5,    0,    2,    2,    6,    6,    7,    7,    7,
    7,    4,    4,    9,   10,   11,   12,   13,   14,   15,
   16,   17,   18,   19,   21,    8,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,   20,   20,
   24,   22,   26,   22,   27,   22,   28,   22,   29,   30,
   22,   23,   23,   31,   31,   33,   34,   35,   36,   37,
   38,   39,   40,   41,   42,   32,   43,   44,   32,   45,
   46,   47,   48,   49,   32,   50,   51,   53,   32,   54,
   55,   56,   57,   58,   59,   60,   61,   32,   62,   63,
   64,   65,   66,   67,   68,   69,   70,   71,   72,   73,
   32,   74,   75,   76,   77,   78,   79,   80,   81,   32,
   82,   83,   84,   85,   52,   86,   52,   87,   52,   25,
   88,   88,   89,
};
short yylen[] = {                                         2,
    0,    0,    4,    0,    1,    2,    1,    2,    2,    2,
    2,    2,    1,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   27,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    0,    1,
    0,    5,    0,    5,    0,    5,    0,    5,    0,    0,
    7,    0,    1,    2,    1,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   23,    0,    0,    7,    0,
    0,    0,    0,    0,   13,    0,    0,    0,    9,    0,
    0,    0,    0,    0,    0,    0,    0,   19,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   27,    0,    0,    0,    0,    0,    0,    0,    0,   19,
    0,    0,    0,    0,   11,    0,    5,    0,    5,    2,
    2,    1,    5,
};
short yydefred[] = {                                      1,
    0,    0,    0,    0,    0,    0,    2,    0,    7,    8,
    9,   10,   11,    0,    6,    0,    0,   13,   14,   12,
    0,   15,    0,   27,   28,   29,   30,   31,   32,   33,
   34,   35,   36,   37,   38,   16,    0,   17,    0,   18,
    0,   19,    0,   20,    0,   21,    0,   22,    0,   23,
    0,   24,    0,   25,    0,    0,    0,   40,   41,   45,
   47,   43,   49,   26,    0,    0,    0,    0,    0,    0,
    0,    0,   55,    0,    0,    0,  122,    0,    0,   50,
   56,   67,   70,   76,   80,   89,  102,   42,   54,    0,
   46,  120,  121,   48,   44,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   57,   68,   71,   77,   81,
   90,  103,    0,   51,    0,    0,    0,    0,    0,    0,
    0,  123,   58,    0,   72,   78,   82,   91,  104,    0,
   69,    0,    0,    0,    0,    0,   59,   73,    0,    0,
   83,   92,  105,    0,    0,  111,  116,  118,   79,    0,
    0,    0,   60,   74,    0,    0,    0,   84,   93,  106,
    0,    0,  112,    0,    0,    0,    0,    0,   61,    0,
    0,  117,  119,   85,   94,  107,    0,   75,  113,    0,
    0,    0,   62,    0,   86,   95,  108,    0,  114,    0,
    0,    0,   63,    0,   87,   96,  109,    0,    0,    0,
    0,    0,   64,  115,    0,   97,    0,    0,   88,    0,
  110,   65,   98,    0,    0,    0,   99,   66,    0,  100,
    0,    0,  101,
};
short yydgoto[] = {                                       1,
   36,    7,    2,   17,   14,    8,    9,   18,   21,   23,
   37,   39,   41,   43,   45,   47,   49,   51,   53,   57,
   55,   58,   71,   65,   75,   68,   66,   67,   69,   96,
   72,   73,   97,  115,  130,  144,  161,  177,  188,  198,
  208,  214,   98,  116,   99,  117,  132,  145,  162,  100,
  118,  140,  133,  101,  119,  134,  150,  166,  180,  190,
  200,  102,  120,  135,  151,  167,  181,  191,  201,  210,
  215,  219,  221,  103,  121,  136,  152,  168,  182,  192,
  202,  155,  171,  184,  194,  156,  157,   76,   77,
};
short yysindex[] = {                                      0,
    0, -254, -278, -267, -252, -245,    0, -254,    0,    0,
    0,    0,    0,  -27,    0, -220,  -27,    0,    0,    0,
 -243,    0, -261,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0, -242,    0, -241,    0,
 -240,    0, -239,    0, -238,    0, -237,    0, -235,    0,
 -234,    0, -233,    0,  -10, -253,   -8,    0,    0,    0,
    0,    0,    0,    0,   -5,   16,   16,   16, -229, -236,
   -4,   -5,    0, -227,   -2,  -40,    0,   -1,    1,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   18,
    0,    0,    0,    0,    0, -222, -221, -219, -218, -217,
 -216, -215, -214, -213,    4,    0,    0,    0,    0,    0,
    0,    0,   33,    0, -211, -210, -209, -208, -207, -206,
 -205,    0,    0,   20,    0,    0,    0,    0,    0, -203,
    0, -202,   25, -200, -199, -198,    0,    0, -230,   27,
    0,    0,    0, -196, -195,    0,    0,    0,    0, -194,
 -193, -192,    0,    0, -191, -190,  -27,    0,    0,    0,
 -189, -188,    0,   37,  -53, -186, -185, -184,    0,   41,
 -182,    0,    0,    0,    0,    0, -181,    0,    0, -180,
 -179, -178,    0, -177,    0,    0,    0, -176,    0, -175,
 -174, -173,    0, -172,    0,    0,    0, -171,   54, -169,
 -168, -167,    0,    0,   58,    0,   59, -164,    0, -163,
    0,    0,    0, -162, -161,   64,    0,    0, -159,    0,
 -158,   67,    0,
};
short yyrindex[] = {                                      0,
    0,   70,    0,    0,    0,    0,    0,   71,    0,    0,
    0,    0,    0,    0,    0,    0,  132,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   72,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   73,    0,    0,    0,    0,    0,
    0,   74,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,
};
short yygindex[] = {                                      0,
    0,    0,    0,  -24,    0,    0,  129,  -16,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  -56,    0,    0,    0,    0,    0,
    0,   66,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   63,
};
#define YYTABLESIZE 221
short yytable[] = {                                      74,
   20,   24,    3,    4,    5,    6,   16,   10,  173,   59,
   78,   79,   25,   26,   27,   28,   29,   30,   11,   31,
   32,   33,   34,   35,   60,   61,   62,   81,   82,   83,
   84,   63,   16,   12,   85,   86,   87,  146,  147,  148,
   13,   19,   22,   38,   40,   42,   44,   46,   48,   56,
   50,   52,   54,   64,   70,   74,   80,   88,   90,   91,
   94,  104,   95,  105,  106,  114,  107,  108,  109,  110,
  111,  112,  113,  122,  123,  124,  125,  126,  127,  128,
  129,  131,  137,  138,  139,  141,  142,  143,  149,  153,
  154,  158,  159,  160,  163,  164,  169,  170,  172,  174,
  175,  176,  178,  179,  183,  185,  186,  187,  189,  193,
  195,  196,  197,  199,  203,  204,  205,  206,  207,  209,
  211,  212,  213,  216,  217,  218,  220,  222,  223,    4,
    5,    3,  165,   39,   52,   53,   15,   89,   93,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   20,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   92,
};
short yycheck[] = {                                      40,
   17,  263,  257,  258,  259,  260,   60,  286,   62,  263,
   67,   68,  274,  275,  276,  277,  278,  279,  286,  281,
  282,  283,  284,  285,  278,  279,  280,  264,  265,  266,
  267,  285,   60,  286,  271,  272,  273,  268,  269,  270,
  286,  262,  286,  286,  286,  286,  286,  286,  286,   60,
  286,  286,  286,   62,   60,   40,  286,   62,  286,   62,
   62,   44,   62,  286,  286,   62,  286,  286,  286,  286,
  286,  286,  286,   41,  286,  286,  286,  286,  286,  286,
  286,   62,  286,  286,   60,  286,  286,  286,   62,  286,
  286,  286,  286,  286,  286,  286,  286,  286,   62,  286,
  286,  286,   62,  286,  286,  286,  286,  286,  286,  286,
  286,  286,  286,  286,  286,   62,  286,  286,  286,   62,
   62,  286,  286,  286,  286,   62,  286,  286,   62,   60,
   60,    0,  157,   62,   62,   62,    8,   72,   76,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  165,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  261,
};
#define YYFINAL 1
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 286
#if YYDEBUG
char *yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,"'('","')'",0,0,"','",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'<'",0,"'>'",0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
"DOCUMENTSIZE","PAGEWIDTH","PAGEHEIGHT","UNITS","NIL","OBJECT","GEOFRAME",
"REGION","RASTER_MAP","VECTOR_MAP","SITE_MAP","STANDARDSITE","PIXMAPSITE",
"FREEHANDSITE","GRID","BARSCALE","LEGEND","SQUARE","RECTANGLE","CIRCLE",
"ELLIPSE","POLYLINE","POLYGON","SPLINE","OPEN_INTERP_SPLINE",
"CLOSED_INTERP_SPLINE","OPEN_APPROX_SPLINE","CLOSED_APPROX_SPLINE","LABEL",
"String",
};
char *yyrule[] = {
"$accept : PageDescription",
"$$1 :",
"$$2 :",
"PageDescription : $$1 OptionalGlobals $$2 ObjectList",
"OptionalGlobals :",
"OptionalGlobals : Globals",
"Globals : Globals GlobalElement",
"Globals : GlobalElement",
"GlobalElement : DOCUMENTSIZE String",
"GlobalElement : PAGEWIDTH String",
"GlobalElement : PAGEHEIGHT String",
"GlobalElement : UNITS String",
"ObjectList : ObjectList ObjectDescription",
"ObjectList : ObjectDescription",
"$$3 :",
"$$4 :",
"$$5 :",
"$$6 :",
"$$7 :",
"$$8 :",
"$$9 :",
"$$10 :",
"$$11 :",
"$$12 :",
"$$13 :",
"$$14 :",
"ObjectDescription : '<' OBJECT $$3 String $$4 ObjectType $$5 String $$6 String $$7 String $$8 String $$9 String $$10 String $$11 String $$12 String $$13 String $$14 OptionalSpecifics '>'",
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
"OptionalSpecifics :",
"OptionalSpecifics : Specifics",
"$$15 :",
"Specifics : '<' GEOFRAME $$15 OptionalGeoSpecifics '>'",
"$$16 :",
"Specifics : '<' SPLINE $$16 PointList '>'",
"$$17 :",
"Specifics : '<' POLYLINE $$17 PointList '>'",
"$$18 :",
"Specifics : '<' POLYGON $$18 PointList '>'",
"$$19 :",
"$$20 :",
"Specifics : '<' LABEL $$19 String $$20 String '>'",
"OptionalGeoSpecifics :",
"OptionalGeoSpecifics : GeoSpecificsList",
"GeoSpecificsList : GeoSpecificsList GeoSpecifics",
"GeoSpecificsList : GeoSpecifics",
"$$21 :",
"$$22 :",
"$$23 :",
"$$24 :",
"$$25 :",
"$$26 :",
"$$27 :",
"$$28 :",
"$$29 :",
"$$30 :",
"GeoSpecifics : '<' REGION $$21 String $$22 String $$23 String $$24 String $$25 String $$26 String $$27 String $$28 String $$29 String $$30 String '>'",
"$$31 :",
"$$32 :",
"GeoSpecifics : '<' RASTER_MAP $$31 String $$32 String '>'",
"$$33 :",
"$$34 :",
"$$35 :",
"$$36 :",
"$$37 :",
"GeoSpecifics : '<' VECTOR_MAP $$33 String $$34 String $$35 String $$36 String $$37 String '>'",
"$$38 :",
"$$39 :",
"$$40 :",
"GeoSpecifics : '<' SITE_MAP $$38 String $$39 String $$40 SiteSpecifics '>'",
"$$41 :",
"$$42 :",
"$$43 :",
"$$44 :",
"$$45 :",
"$$46 :",
"$$47 :",
"$$48 :",
"GeoSpecifics : '<' GRID $$41 String $$42 String $$43 String $$44 String $$45 String $$46 String $$47 String $$48 String '>'",
"$$49 :",
"$$50 :",
"$$51 :",
"$$52 :",
"$$53 :",
"$$54 :",
"$$55 :",
"$$56 :",
"$$57 :",
"$$58 :",
"$$59 :",
"$$60 :",
"GeoSpecifics : '<' BARSCALE $$49 String $$50 String $$51 String $$52 String $$53 String $$54 String $$55 String $$56 String $$57 String $$58 String $$59 String $$60 String '>'",
"$$61 :",
"$$62 :",
"$$63 :",
"$$64 :",
"$$65 :",
"$$66 :",
"$$67 :",
"$$68 :",
"GeoSpecifics : '<' LEGEND $$61 String $$62 String $$63 String $$64 String $$65 String $$66 String $$67 String $$68 String '>'",
"$$69 :",
"$$70 :",
"$$71 :",
"$$72 :",
"SiteSpecifics : '<' STANDARDSITE $$69 String $$70 String $$71 String $$72 String '>'",
"$$73 :",
"SiteSpecifics : '<' PIXMAPSITE $$73 String '>'",
"$$74 :",
"SiteSpecifics : '<' FREEHANDSITE $$74 ObjectList '>'",
"PointList : Points NIL",
"Points : Points Point",
"Points : Point",
"Point : '(' String ',' String ')'",
};
#endif
#ifdef YYSTACKSIZE
#undef YYMAXDEPTH
#define YYMAXDEPTH YYSTACKSIZE
#else
#ifdef YYMAXDEPTH
#define YYSTACKSIZE YYMAXDEPTH
#else
#define YYSTACKSIZE 500
#define YYMAXDEPTH 500
#endif
#endif
int yydebug;
int yynerrs;
int yyerrflag;
int yychar;
short *yyssp;
YYSTYPE *yyvsp;
YYSTYPE yyval;
YYSTYPE yylval;
short yyss[YYSTACKSIZE];
YYSTYPE yyvs[YYSTACKSIZE];
#define yystacksize YYSTACKSIZE
#define YYABORT goto yyabort
#define YYREJECT goto yyabort
#define YYACCEPT goto yyaccept
#define YYERROR goto yyerrlab
int
#if defined(__STDC__)
yyparse(void)
#else
yyparse()
#endif
{
    register int yym, yyn, yystate;
#if YYDEBUG
    register char *yys;
    extern char *getenv();

    if (yys = getenv("YYDEBUG"))
    {
        yyn = *yys;
        if (yyn >= '0' && yyn <= '9')
            yydebug = yyn - '0';
    }
#endif

    yynerrs = 0;
    yyerrflag = 0;
    yychar = (-1);

    yyssp = yyss;
    yyvsp = yyvs;
    *yyssp = yystate = 0;

yyloop:
    if ((yyn = yydefred[yystate]) != 0) goto yyreduce;
    if (yychar < 0)
    {
        if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, reading %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
    }
    if ((yyn = yysindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: state %d, shifting to state %d\n",
                    YYPREFIX, yystate, yytable[yyn]);
#endif
        if (yyssp >= yyss + yystacksize - 1)
        {
            goto yyoverflow;
        }
        *++yyssp = yystate = yytable[yyn];
        *++yyvsp = yylval;
        yychar = (-1);
        if (yyerrflag > 0)  --yyerrflag;
        goto yyloop;
    }
    if ((yyn = yyrindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
        yyn = yytable[yyn];
        goto yyreduce;
    }
    if (yyerrflag) goto yyinrecovery;
    yyerror("syntax error");
#ifdef lint
    goto yyerrlab;
#endif
yyerrlab:
    ++yynerrs;
yyinrecovery:
    if (yyerrflag < 3)
    {
        yyerrflag = 3;
        for (;;)
        {
            if ((yyn = yysindex[*yyssp]) && (yyn += YYERRCODE) >= 0 &&
                    yyn <= YYTABLESIZE && yycheck[yyn] == YYERRCODE)
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: state %d, error recovery shifting\
 to state %d\n", YYPREFIX, *yyssp, yytable[yyn]);
#endif
                if (yyssp >= yyss + yystacksize - 1)
                {
                    goto yyoverflow;
                }
                *++yyssp = yystate = yytable[yyn];
                *++yyvsp = yylval;
                goto yyloop;
            }
            else
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: error recovery discarding state %d\n",
                            YYPREFIX, *yyssp);
#endif
                if (yyssp <= yyss) goto yyabort;
                --yyssp;
                --yyvsp;
            }
        }
    }
    else
    {
        if (yychar == 0) goto yyabort;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, error recovery discards token %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
        yychar = (-1);
        goto yyloop;
    }
yyreduce:
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: state %d, reducing by rule %d (%s)\n",
                YYPREFIX, yystate, yyn, yyrule[yyn]);
#endif
    yym = yylen[yyn];
    yyval = yyvsp[1-yym];
    switch (yyn)
    {
case 1:
#line 108 "grammar.y"
{
          bzero((char *)&parserGlobals, sizeof(XgdDisplayParserGlobals));
      }
break;
case 2:
#line 112 "grammar.y"
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
      }
break;
case 8:
#line 140 "grammar.y"
{
          parserGlobals.docSizeSet = True;
          parserGlobals.docSize = malloc(strlen(yyvsp[0].cval) + 1);
          strcpy(parserGlobals.docSize, yyvsp[0].cval);
          if (__DEBUG ) fprintf(stderr, " Documentsize -> %s\n", yyvsp[0].cval);
      }
break;
case 9:
#line 147 "grammar.y"
{
          double pageWidth;
          char *copy = malloc(strlen(yyvsp[0].cval) + 1);
          char *ptr = NULL;

          if ( (pageWidth = strtod(yyvsp[0].cval, &ptr)) == 0 ) {
              if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
                  yyerror("Illegal type: PAGEWIDTH must be an integer/real");
                  return;
              }
          }
          strcpy(copy, yyvsp[0].cval);
          if ( G_rindex(copy,'.') != NULL ) {
              if (__DEBUG ) fprintf(stderr, " PageWidth -> %lf\n", pageWidth);
          } else {
              if (__DEBUG ) fprintf(stderr, " PageWidth -> %d\n", (int)pageWidth);
          }
          parserGlobals.pageWidthSet = True;
          parserGlobals.pageWidth = pageWidth;
          free(copy);
      }
break;
case 10:
#line 169 "grammar.y"
{
          double pageHeight;
          char *copy = malloc(strlen(yyvsp[0].cval) + 1);
          char *ptr = NULL;

          if ( (pageHeight = strtod(yyvsp[0].cval, &ptr)) == 0 ) {
              if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
		  yyerror("Illegal type: PAGEHEIGHT must be an integer/real");
		  return;
              }
          }
          strcpy(copy, yyvsp[0].cval);
          if ( G_rindex(copy,'.') != NULL ) {
              if (__DEBUG ) fprintf(stderr, " PageHeight -> %lf\n", pageHeight);
          } else {
              if (__DEBUG ) fprintf(stderr, " PageHeight -> %d\n", (int)pageHeight);
          }
          parserGlobals.pageHeightSet = True;
          parserGlobals.pageHeight = pageHeight;
          free(copy);
      }
break;
case 11:
#line 195 "grammar.y"
{
          char *units;
          char errorbuf[1024];

          if ( !strcmp(yyvsp[0].cval,"mm") || !strcmp(yyvsp[0].cval,"millimeters") ) {
              if (__DEBUG ) fprintf(stderr,"Units -> millimeters\n");
          } else if ( !strcmp(yyvsp[0].cval,"in") || !strcmp(yyvsp[0].cval,"inches") ) {
              if (__DEBUG ) fprintf(stderr,"Units -> inches\n");
          } else if ( !strcmp(yyvsp[0].cval,"pixel") || !strcmp(yyvsp[0].cval,"pixels") ) {
              if (__DEBUG ) fprintf(stderr,"Units -> pixels\n");
          } else {
              yywarning("Illegal value: UNITS must be one of:");
              if (__DEBUG ) fprintf(stderr,"\t\"mm\", \"millimeters\"\n");
              if (__DEBUG ) fprintf(stderr,"\t\"in\", \"inches\"\n");
              if (__DEBUG ) fprintf(stderr,"\t\"pixel\", \"pixels\"\n");
              exit(1);
          }
          parserGlobals.unitsSet = True;
          parserGlobals.units = malloc(strlen(yyvsp[0].cval) + 1);
          strcpy(parserGlobals.units, yyvsp[0].cval);
      }
break;
case 14:
#line 226 "grammar.y"
{
              if (__DEBUG ) fprintf(stderr, "PARSING OBJECT DESCRIPTION\n");
              specificsSet = False;
          }
break;
case 15:
#line 231 "grammar.y"
{
              if (__DEBUG ) fprintf(stderr,"\toid = %s\n", yyvsp[0].cval);
          }
break;
case 16:
#line 235 "grammar.y"
{
              curObject = XgdCreateObject(yyvsp[0].ival);
          }
break;
case 17:
#line 239 "grammar.y"
{
              int x;
              char *ptr = NULL;

              if ( (x = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
                      yyerror("Illegal type: x position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tx = %d\n", x);
              templateObject.x = x;
          }
break;
case 18:
#line 253 "grammar.y"
{
              int y;
              char *ptr;

              if ( (y = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
                      yyerror("Illegal type: y position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\ty = %d\n", y);
              templateObject.y = y;
          }
break;
case 19:
#line 267 "grammar.y"
{
              int width;
              char *ptr;

              if ( (width = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
                      yyerror("Illegal type: width must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\twidth = %d\n", width);
              templateObject.width = width;
          }
break;
case 20:
#line 281 "grammar.y"
{
              int height;
              char *ptr;

              if ( (height = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
                      yyerror("Illegal type: height must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\theight = %d\n", height);
              templateObject.height = height;
          }
break;
case 21:
#line 297 "grammar.y"
{
              int fillpat;
              char *ptr;

              if ( (fillpat = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
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
          }
break;
case 22:
#line 319 "grammar.y"
{
              int linepat;
              char *ptr;

              if ( (linepat = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
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
          }
break;
case 23:
#line 339 "grammar.y"
{
              int linewidth;
              char *ptr;

              if ( (linewidth = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
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
          }
break;
case 24:
#line 361 "grammar.y"
{
              if (__DEBUG ) fprintf(stderr,"\tfg = %s\n", yyvsp[0].cval);
              templateObject.fg = XgdGetVectColorPixelByName(yyvsp[0].cval);
          }
break;
case 25:
#line 370 "grammar.y"
{
              templateObject.bg = XgdGetVectColorPixelByName(yyvsp[0].cval);
              if (__DEBUG ) fprintf(stderr,"\tbg = %s\n", yyvsp[0].cval);

              XgdInitObject(Global.display, XtWindow(Global.drawArea), 
                curObject, templateObject.fp, templateObject.lp,
                templateObject.fg, templateObject.bg, 
                templateObject.lw);
          }
break;
case 26:
#line 380 "grammar.y"
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
          }
break;
case 27:
#line 422 "grammar.y"
{ 
           yyval.ival = XGD_GEOFRAME; 
       }
break;
case 28:
#line 426 "grammar.y"
{ 
           yyval.ival = XGD_SQUARE; 
       }
break;
case 29:
#line 430 "grammar.y"
{ 
           yyval.ival = XGD_RECTANGLE; 
       }
break;
case 30:
#line 434 "grammar.y"
{ 
           yyval.ival = XGD_CIRCLE; 
       }
break;
case 31:
#line 438 "grammar.y"
{ 
           yyval.ival = XGD_ELLIPSE; 
       }
break;
case 32:
#line 442 "grammar.y"
{ 
           yyval.ival = XGD_POLYLINE; 
       }
break;
case 33:
#line 446 "grammar.y"
{ 
           yyval.ival = XGD_POLYGON; 
       }
break;
case 34:
#line 450 "grammar.y"
{ 
           yyval.ival = XGD_OPEN_INTERP_SPLINE; 
       }
break;
case 35:
#line 454 "grammar.y"
{ 
           yyval.ival = XGD_CLOSED_INTERP_SPLINE; 
       }
break;
case 36:
#line 458 "grammar.y"
{ 
           yyval.ival = XGD_OPEN_APPROX_SPLINE; 
       }
break;
case 37:
#line 462 "grammar.y"
{ 
           yyval.ival = XGD_CLOSED_APPROX_SPLINE; 
       }
break;
case 38:
#line 466 "grammar.y"
{ 
           yyval.ival = XGD_LABEL; 
       }
break;
case 41:
#line 479 "grammar.y"
{
              if (__DEBUG ) fprintf(stderr,"GeoFrame Specifics\n");
	      /*  
	       * set geoFrameObject to point to curObject so we can use 
	       * curobject in subobject specifications...
	       */
	      geoFrameObject = curObject;
          }
break;
case 42:
#line 488 "grammar.y"
{
              geoFrame = True;
              specificsSet = True;
          }
break;
case 43:
#line 494 "grammar.y"
{
              if (__DEBUG ) fprintf(stderr,"Spline Specifics\n");
          }
break;
case 44:
#line 498 "grammar.y"
{
              specificsSet = True;
          }
break;
case 45:
#line 503 "grammar.y"
{
              if (__DEBUG ) fprintf(stderr,"Polyline Specifics\n");
          }
break;
case 46:
#line 507 "grammar.y"
{
              specificsSet = True;
          }
break;
case 47:
#line 512 "grammar.y"
{
              if (__DEBUG ) fprintf(stderr,"Polygon Specifics\n");
          }
break;
case 48:
#line 516 "grammar.y"
{
              specificsSet = True;
          }
break;
case 49:
#line 521 "grammar.y"
{
              if (__DEBUG ) fprintf(stderr,"Label Specifics\n");
          }
break;
case 50:
#line 525 "grammar.y"
{
              ParseString(yyvsp[0].cval, &curObject->Obj.Label.numlines, 
                              &curObject->Obj.Label.lblstr);
          }
break;
case 51:
#line 530 "grammar.y"
{
              if ((curObject->Obj.Label.font = 
                   XLoadQueryFont(Global.display, yyvsp[-1].cval)) == NULL ) {
                  char buf[1024];
    
                  sprintf (buf, 
                     "Sorry, the selected font:\n[%s]\nIs not available.\n",
                     yyvsp[-1].cval);
                  yyerror(buf);
                  return;
              }
              XgdSetLabelFont(curObject, curObject->Obj.Label.font, False);
              XgdSetLabelFontName(curObject, yyvsp[-1].cval);
              specificsSet = True;
          }
break;
case 56:
#line 561 "grammar.y"
{
              ;
          }
break;
case 57:
#line 568 "grammar.y"
{
              int projection;
              char *ptr;

              if ( (projection = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
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
          }
break;
case 58:
#line 590 "grammar.y"
{
              int zone;
              char *ptr;

              if ( (zone = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
                      yyerror("Illegal type: zone must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tzone = %d\n", zone);
              curObject->Obj.GeoFrame.region.zone = zone;
          }
break;
case 59:
#line 604 "grammar.y"
{
	      double ew_res;
	      char *ptr = NULL;

	      if ( (ew_res = strtod(yyvsp[0].cval, &ptr)) == 0 ) {
		  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
		  yyerror("Illegal type: east-west res. must be an int/real");
		  return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\tew_res = %lf\n", ew_res);
              curObject->Obj.GeoFrame.region.ew_res = ew_res;
          }
break;
case 60:
#line 618 "grammar.y"
{
	      double ns_res;
	      char *ptr = NULL;

	      if ( (ns_res = strtod(yyvsp[0].cval, &ptr)) == 0 ) {
		  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
		  yyerror("Illegal type: north-south res. must be an int/real");
		  return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\tns_res = %lf\n", ns_res);
              curObject->Obj.GeoFrame.region.ns_res = ns_res;
          }
break;
case 61:
#line 632 "grammar.y"
{
	      double north;
	      char *ptr = NULL;

	      if ( (north = strtod(yyvsp[0].cval, &ptr)) == 0 ) {
		  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
		      yyerror("Illegal type: north edge must be an int/real");
		      return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\tnorth = %lf\n", north);
              curObject->Obj.GeoFrame.region.north = north;
          }
break;
case 62:
#line 646 "grammar.y"
{
	      double south;
	      char *ptr = NULL;

	      if ( (south = strtod(yyvsp[0].cval, &ptr)) == 0 ) {
		  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
		      yyerror("Illegal type: south edge must be an int/real");
		      return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\tsouth = %lf\n", south);
              curObject->Obj.GeoFrame.region.south = south;
          }
break;
case 63:
#line 660 "grammar.y"
{
	      double east;
	      char *ptr = NULL;

	      if ( (east = strtod(yyvsp[0].cval, &ptr)) == 0 ) {
		  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
		      yyerror("Illegal type: east edge must be an int/real");
		      return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\teast = %lf\n", east);
              curObject->Obj.GeoFrame.region.east = east;
          }
break;
case 64:
#line 674 "grammar.y"
{
	      double west;
	      char *ptr = NULL;

	      if ( (west = strtod(yyvsp[0].cval, &ptr)) == 0 ) {
		  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
		      yyerror("Illegal type: west edge must be an int/real");
		      return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\twest = %lf\n", west);
              curObject->Obj.GeoFrame.region.west = west;
          }
break;
case 65:
#line 688 "grammar.y"
{
              int rows;
              char *ptr;

              if ( (rows = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
                      yyerror("Illegal type: rows must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\trows = %d\n", rows);
              curObject->Obj.GeoFrame.region.rows = rows;
          }
break;
case 66:
#line 702 "grammar.y"
{
              int cols;
              char *ptr;

              if ( (cols = (int)strtol(yyvsp[-1].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[-1].cval, ptr) ) {
                      yyerror("Illegal type: cols must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tcols = %d\n", cols);
              curObject->Obj.GeoFrame.region.cols = cols;
          }
break;
case 67:
#line 717 "grammar.y"
{
              ;
          }
break;
case 68:
#line 721 "grammar.y"
{
              curObject->Obj.GeoFrame.rmapset = XtNewString(yyvsp[0].cval);
          }
break;
case 69:
#line 725 "grammar.y"
{
              curObject->Obj.GeoFrame.rname = XtNewString(yyvsp[-1].cval);
          }
break;
case 70:
#line 730 "grammar.y"
{
              ;
          }
break;
case 71:
#line 734 "grammar.y"
{
              vectTempl.mapset = XtNewString(yyvsp[0].cval);
          }
break;
case 72:
#line 738 "grammar.y"
{
              vectTempl.name = XtNewString(yyvsp[0].cval);
          }
break;
case 73:
#line 742 "grammar.y"
{
              vectTempl.fg = XgdGetVectColorPixelByName(yyvsp[0].cval);
          }
break;
case 74:
#line 746 "grammar.y"
{
              int linepat;
              char *ptr;

              if ( (linepat = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
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
          }
break;
case 75:
#line 766 "grammar.y"
{
              int linewidth;
              char *ptr;

              if ( (linewidth = (int)strtol(yyvsp[-1].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[-1].cval, ptr) ) {
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
          }
break;
case 76:
#line 789 "grammar.y"
{
              ;
          }
break;
case 77:
#line 793 "grammar.y"
{
              siteTempl.name = XtNewString(yyvsp[0].cval);
fprintf(stderr,"DOING A SITE MAP\nname = %s\n",  siteTempl.name);
          }
break;
case 78:
#line 798 "grammar.y"
{
              siteTempl.mapset = XtNewString(yyvsp[0].cval);
fprintf(stderr,"mapset = %s\n",  siteTempl.mapset);
          }
break;
case 79:
#line 803 "grammar.y"
{
              ;
          }
break;
case 80:
#line 808 "grammar.y"
{
            geoFrameObject->Obj.GeoFrame.gridOn = True;
	    if ( geoFrameObject->Obj.GeoFrame.grid.gc == NULL ) {
		XGCValues gcv;

		gcv.background = geoFrameObject->bg;
		geoFrameObject->Obj.GeoFrame.grid.gc =
		    XCreateGC(Global.display, XtWindow(Global.drawArea), 0, 0);
	    }
          }
break;
case 81:
#line 819 "grammar.y"
{
              int labelon;
              char *ptr;

              if ( (labelon = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
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
          }
break;
case 82:
#line 838 "grammar.y"
{
              double gap;
              char *ptr = NULL;

              if ( (gap = strtod(yyvsp[0].cval, &ptr)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
		      yyerror("Illegal type: grid gap must be an int/real");
		      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tgap = %lf\n", gap);
              XgdSetGridGap(geoFrameObject, gap);
          }
break;
case 83:
#line 852 "grammar.y"
{
              int spacing;
              char *ptr;

              if ( (spacing = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tspacing = %d\n", spacing);
              XgdSetGridSpacing(geoFrameObject, spacing);
          }
break;
case 84:
#line 866 "grammar.y"
{
              Pixel color  = XgdGetVectColorPixelByName(yyvsp[0].cval);
              if (__DEBUG ) fprintf(stderr,"\tcolor = %s\n", yyvsp[0].cval);
	      XgdSetGridColor(geoFrameObject, color);
          }
break;
case 85:
#line 872 "grammar.y"
{
              int linepat;
              char *ptr;

              if ( (linepat = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
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
          }
break;
case 86:
#line 892 "grammar.y"
{
              int linewidth;
              char *ptr;

              if ( (linewidth = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
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
          }
break;
case 87:
#line 911 "grammar.y"
{
              XFontStruct *fs;
              int xoff = 0, yoff = 0;

              if ((fs = XLoadQueryFont(Global.display, yyvsp[0].cval)) == NULL ) {
                  char buf[1024];

                  sprintf (buf,
                     "Sorry, the selected font:\t\n[%s]\t\nIs not available.",
                     yyvsp[0].cval);
                  yyerror(buf);
                  return;
              }
              XgdSetGridFont(geoFrameObject, fs->fid, False);
              XgdSetGridFontName(geoFrameObject, yyvsp[0].cval);
              if ( geoFrameObject->Obj.GeoFrame.grid.labelon )
                  XgdCalculateGridOffset(geoFrameObject, &xoff, &yoff);
              XgdSetGridOffset(geoFrameObject, xoff, yoff);
          }
break;
case 88:
#line 931 "grammar.y"
{
              Pixel color  = XgdGetVectColorPixelByName(yyvsp[-1].cval);
              if (__DEBUG ) fprintf(stderr,"\ttextcolor = %s\n", yyvsp[-1].cval);
	      XgdSetGridTextColor(geoFrameObject, color);
          }
break;
case 89:
#line 938 "grammar.y"
{
	      /* create a new object */
	      curObject = XgdCreateObject(XGD_BARSCALE);
              XgdInitObject(Global.display, XtWindow(Global.drawArea), 
                curObject, Global.fillPattern, Global.linePattern,
                Global.foreground, Global.background, 
                Global.lineWidth);
          }
break;
case 90:
#line 947 "grammar.y"
{
              int x;
              char *ptr = NULL;

              if ( (x = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
                      yyerror("Illegal type: x position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tx = %d\n", x);
	      curObject->x = x;
          }
break;
case 91:
#line 961 "grammar.y"
{
              int y;
              char *ptr = NULL;

              if ( (y = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
                      yyerror("Illegal type: y position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\ty = %d\n", y);
	      curObject->y = y;
          }
break;
case 92:
#line 975 "grammar.y"
{
              int width;
              char *ptr = NULL;

              if ( (width = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
                      yyerror("Illegal type: width must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\twidth = %d\n", width);
              curObject->width = width;
          }
break;
case 93:
#line 989 "grammar.y"
{
              int height;
              char *ptr = NULL;

              if ( (height = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
                      yyerror("Illegal type: height must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\theight = %d\n", height);
              curObject->height = height;
          }
break;
case 94:
#line 1003 "grammar.y"
{
              int style;
              char *ptr;

              if ( (style = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
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
          }
break;
case 95:
#line 1023 "grammar.y"
{
	      double length;
	      char *ptr = NULL;

	      if ( (length = strtod(yyvsp[0].cval, &ptr)) == 0 ) {
		  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
		  yyerror("Illegal type: barscale length must be an int/real");
		  return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\tlength = %lf\n", length);
	      XgdSetBarscaleLength(curObject, length);
          }
break;
case 96:
#line 1037 "grammar.y"
{
              int intervals;
              char *ptr = NULL;

              if ( (intervals = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
                      yyerror("Illegal type: intervals must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tintervals = %d\n", intervals);
	      XgdSetBarscaleInterval(curObject, intervals);
          }
break;
case 97:
#line 1051 "grammar.y"
{
              int units;
              char *ptr;

              if ( (units = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
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
          }
break;
case 98:
#line 1072 "grammar.y"
{ 
              int linewidth;
              char *ptr;

              if ( (linewidth = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
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
          }
break;
case 99:
#line 1091 "grammar.y"
{
              Pixel color  = XgdGetVectColorPixelByName(yyvsp[0].cval);
              if (__DEBUG ) fprintf(stderr,"\tcolor = %s\n", yyvsp[0].cval);
	      XgdSetBarscaleColor(curObject, color);
          }
break;
case 100:
#line 1097 "grammar.y"
{
	      XFontStruct *fs;

              if ((fs = XLoadQueryFont(Global.display, yyvsp[0].cval)) == NULL ) {
                  char buf[1024];

                  sprintf (buf,
                     "Sorry, the selected font:\t\n[%s]\t\nIs not available.",
                     yyvsp[0].cval);
                  yyerror(buf);
                  return;
              }
              XgdSetBarscaleFont(curObject, fs->fid, False);
              XgdSetBarscaleFontName(curObject, yyvsp[0].cval);
          }
break;
case 101:
#line 1113 "grammar.y"
{
              Pixel textcolor = XgdGetVectColorPixelByName(yyvsp[-1].cval);
              if (__DEBUG ) fprintf(stderr,"\ttextcolor = %s\n", yyvsp[-1].cval);
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
          }
break;
case 102:
#line 1156 "grammar.y"
{
	      /* create a new object */
	      curObject = XgdCreateObject(XGD_LEGEND);
              XgdInitObject(Global.display, XtWindow(Global.drawArea), 
                curObject, Global.fillPattern, Global.linePattern,
                Global.foreground, Global.background, 
                Global.lineWidth);
          }
break;
case 103:
#line 1165 "grammar.y"
{
              int x;
              char *ptr = NULL;

              if ( (x = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
                      yyerror("Illegal type: x position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tx = %d\n", x);
	      curObject->x = x;
          }
break;
case 104:
#line 1179 "grammar.y"
{
              int y;
              char *ptr = NULL;

              if ( (y = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
                      yyerror("Illegal type: y position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\ty = %d\n", y);
	      curObject->y = y;
          }
break;
case 105:
#line 1193 "grammar.y"
{
              ;
          }
break;
case 106:
#line 1197 "grammar.y"
{
              ;
          }
break;
case 107:
#line 1201 "grammar.y"
{
              ;
          }
break;
case 108:
#line 1205 "grammar.y"
{
              ;
          }
break;
case 109:
#line 1209 "grammar.y"
{
              ;
          }
break;
case 110:
#line 1213 "grammar.y"
{
              ;
          }
break;
case 111:
#line 1220 "grammar.y"
{
              siteTempl.type = XGD_SITE_STANDARD;
          }
break;
case 112:
#line 1224 "grammar.y"
{
              int icontype;
              char *ptr;

              if ( (icontype = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
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
          }
break;
case 113:
#line 1244 "grammar.y"
{
              int size;
              char *ptr;

              if ( (size = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
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
          }
break;
case 114:
#line 1263 "grammar.y"
{
              int linewidth;
              char *ptr;

              if ( (linewidth = (int)strtol(yyvsp[0].cval, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp(yyvsp[0].cval, ptr) ) {
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
          }
break;
case 115:
#line 1282 "grammar.y"
{
              siteTempl.color  = XgdGetVectColorPixelByName(yyvsp[-1].cval);
              if (__DEBUG ) fprintf(stderr,"\tcolor = %s\n", yyvsp[-1].cval);
              XgdInitSite(geoFrameObject, siteTempl.name, siteTempl.mapset, 
                  siteTempl.type);
              XgdUpdateSiteStd(geoFrameObject->Obj.GeoFrame.sites.Tail,
                  siteTempl.icontype, siteTempl.size, siteTempl.linewidth,
                  siteTempl.color);
          }
break;
case 116:
#line 1293 "grammar.y"
{
              siteTempl.type = XGD_SITE_PIXMAP;
          }
break;
case 117:
#line 1297 "grammar.y"
{
              siteTempl.pixmapfile = XtNewString(yyvsp[-1].cval);

              XgdInitSite(geoFrameObject, siteTempl.name, siteTempl.mapset, 
                  siteTempl.type);
              XgdDrawSite(geoFrameObject, XtWindow(Global.drawArea),
			  siteTempl.name, siteTempl.mapset,
			  siteTempl.pixmapfile);
          }
break;
case 118:
#line 1308 "grammar.y"
{
              ;
          }
break;
case 123:
#line 1324 "grammar.y"
{
          int x, y;
          char *ptr = NULL;

          if ( (x = (int)strtol(yyvsp[-3].cval, &ptr, 10)) == 0 ) {
              if ( ptr != NULL && !strcmp(yyvsp[-3].cval, ptr) ) {
              yyerror("Illegal type: x value in point must be an integer");
                      return;
              }
          }
          if ( (y = (int)strtol(yyvsp[-1].cval, &ptr, 10)) == 0 ) {
              if ( ptr != NULL && !strcmp(yyvsp[-1].cval, ptr) ) {
              yyerror("Illegal type: x value in point must be an integer");
                      return;
              }
          }
          XgdAddPointToPointList(curObject, x, y);
      }
break;
#line 1947 "y.tab.c"
    }
    yyssp -= yym;
    yystate = *yyssp;
    yyvsp -= yym;
    yym = yylhs[yyn];
    if (yystate == 0 && yym == 0)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: after reduction, shifting from state 0 to\
 state %d\n", YYPREFIX, YYFINAL);
#endif
        yystate = YYFINAL;
        *++yyssp = YYFINAL;
        *++yyvsp = yyval;
        if (yychar < 0)
        {
            if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
            if (yydebug)
            {
                yys = 0;
                if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
                if (!yys) yys = "illegal-symbol";
                printf("%sdebug: state %d, reading %d (%s)\n",
                        YYPREFIX, YYFINAL, yychar, yys);
            }
#endif
        }
        if (yychar == 0) goto yyaccept;
        goto yyloop;
    }
    if ((yyn = yygindex[yym]) && (yyn += yystate) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yystate)
        yystate = yytable[yyn];
    else
        yystate = yydgoto[yym];
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: after reduction, shifting from state %d \
to state %d\n", YYPREFIX, *yyssp, yystate);
#endif
    if (yyssp >= yyss + yystacksize - 1)
    {
        goto yyoverflow;
    }
    *++yyssp = yystate;
    *++yyvsp = yyval;
    goto yyloop;
yyoverflow:
    yyerror("yacc stack overflow");
yyabort:
    return (1);
yyaccept:
    return (0);
}
