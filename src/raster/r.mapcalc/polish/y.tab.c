#ifndef lint
/*static char yysccsid[] = "from: @(#)yaccpar	1.9 (Berkeley) 02/21/93";*/
static char yyrcsid[] = "$Id$";
#endif
#define YYBYACC 1
#define YYMAJOR 1
#define YYMINOR 9
#define yyclearin (yychar=(-1))
#define yyerrok (yyerrflag=0)
#define YYRECOVERING (yyerrflag!=0)
#define YYPREFIX "yy"
#line 11 "pol.y"
#include <stdlib.h>
#include <string.h>
#include "local_proto.h"

static int nstored = 0;
static char **storage = 0;
#line 20 "y.tab.c"
#define NAME 257
#define STRING 258
#define INTEGER 259
#define FLOAT 260
#define FUNCTION 261
#define GT 262
#define GE 263
#define EQ 264
#define LT 265
#define LE 266
#define AND 267
#define OR 268
#define COLOR_GRAY 269
#define COLOR_RED 270
#define COLOR_BLUE 271
#define COLOR_GREEN 272
#define NE 273
#define UMINUS 274
#define YYERRCODE 256
short yylhs[] = {                                        -1,
    0,    0,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    2,    2,    4,    4,    4,    4,    4,    5,    5,
    3,    3,
};
short yylen[] = {                                         2,
    4,    2,    3,    3,    3,    3,    3,    3,    3,    3,
    3,    3,    3,    3,    3,    3,    3,    2,    3,    1,
    1,    4,    6,    1,    2,    5,    7,    3,    4,    1,
    1,    1,    3,    1,    1,    1,    1,    1,    1,    3,
    1,    2,
};
short yydefred[] = {                                      0,
    0,    0,    0,    2,    0,    0,   21,   30,   31,    0,
   35,   36,   38,   37,    0,    0,   34,    0,    0,    0,
    0,    0,    0,   18,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    1,
    0,    0,    0,    0,   33,   28,    0,    0,    3,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   41,    0,    0,    0,   29,    0,   42,
   22,    0,    0,    0,    0,   26,    0,   23,    0,   27,
};
short yydgoto[] = {                                       3,
   18,   19,   66,   20,   48,
};
short yysindex[] = {                                   -168,
   -8,  -52,    0,    0,  453,  -58,    0,    0,    0,  -19,
    0,    0,    0,    0,  453,  453,    0,  116,  -80, -234,
  453, -228,  255,    0,  434,  453,  453,  453,  453,  453,
  453,  453,  453,  453,  453,  453,  453,  453,  453,    0,
  -44,  -24,  -49,  -29,    0,    0,  -29,  -22,    0,  -17,
  -17,  -17,  -17,  -17,  294,  294,  -17,  -32,  -32,  -46,
  -46,  -46,  -46,    0, -209,  -40,  -44,    0,  453,    0,
    0,  -44,  -27,  -29,  -39,    0,  -44,    0,  -13,    0,
};
short yyrindex[] = {                                      0,
    0,    0,    0,    0,    0,  -10,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   14,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    2,   26,   -3,    0,    0,   34,    0,    0,  140,
  152,  164,  176,  188,   50,   54,  201,   42,  128,   62,
   80,   92,  104,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   35,    0,    0,    0,    0,    0,    0,
};
short yygindex[] = {                                      0,
  514,   72,   88,    0,    0,
};
#define YYTABLESIZE 725
short yytable[] = {                                      20,
   65,    4,   21,   72,   38,   22,   19,   38,    5,   36,
   41,   32,   36,   34,   37,   35,   77,   37,   68,   38,
   23,   69,   42,   24,   36,   34,   20,   35,   45,   37,
   20,   20,   20,   20,   20,   25,   20,   19,   32,   22,
   19,   67,   32,   32,   32,   32,   32,   39,   32,   70,
   24,   16,   71,   78,   24,   24,   24,   24,   24,    4,
   24,   39,   25,    5,   39,   76,   25,   25,   25,   25,
   25,   13,   25,    0,   39,   40,   39,   39,   40,   80,
   32,    0,   16,   20,   16,   16,   16,    1,    2,   14,
    4,   43,   32,    4,    5,   32,    0,    5,   13,    0,
    0,   15,   13,   13,   13,   13,   13,   24,   13,    0,
    0,    0,    0,   12,    0,    0,   14,    0,    0,   25,
   14,   14,   14,   14,   14,   40,   14,    0,   15,    0,
    0,    0,   15,   15,   15,   15,   15,   17,   15,    0,
   12,    0,    0,    0,   12,   12,   12,   12,   12,    6,
   12,    0,   38,    0,   73,    0,    0,   36,   34,   75,
   35,    7,   37,    0,   79,    0,    0,    0,   17,    0,
   17,   17,   17,   10,    0,    0,    0,    0,    0,    0,
    6,    0,    0,    6,    0,    8,    0,    0,    0,    0,
    0,    0,    7,    0,    0,    7,    0,    9,    0,    0,
    0,    0,    0,    0,   10,    0,    0,   10,    0,   39,
   11,    0,    0,    0,   64,    0,    8,    0,    0,    8,
    0,    0,    0,    0,    0,    0,    0,    0,    9,    0,
    0,    9,   26,   27,   28,   29,   30,   31,   32,    0,
    0,   11,    0,   33,   11,    0,    0,    0,    0,    0,
    0,   20,   20,   20,   20,   20,   20,   20,    0,    0,
    0,    0,   20,   32,   32,   32,   32,   32,   32,   32,
    0,    0,    0,    0,   32,   24,   24,   24,   24,   24,
   24,   24,    0,    0,    0,    0,   24,   25,   25,   25,
   25,   25,   25,   25,   16,   46,    0,    0,   25,   15,
    0,    0,    0,   16,   16,   16,   16,   16,   16,   16,
    0,    0,    0,    0,   16,    0,    4,    4,   17,    0,
    5,    5,    0,   13,   13,   13,   13,   13,   13,   13,
   38,    0,    0,    0,   13,   36,   34,    0,   35,    0,
   37,   14,   14,   14,   14,   14,   14,   14,    0,    0,
    0,    0,   14,   15,   15,   15,   15,   15,   15,   15,
    0,    0,    0,    0,   15,   12,   12,   12,   12,   12,
   12,   12,    0,    0,    0,    0,   12,   26,   27,   28,
   29,   30,   31,   32,    0,    0,    0,   39,   33,   17,
   17,   17,   17,   17,   17,   17,    0,    0,    0,    0,
   17,    6,    6,    6,    6,    6,    6,    6,    0,    0,
    0,    0,    6,    7,    7,    7,    7,    7,    7,    7,
    0,    0,    0,    0,    7,   10,   10,   10,   10,   10,
   10,   10,    0,    0,    0,    0,   10,    8,    8,    8,
    8,    8,    8,    8,    0,    0,    0,    0,    8,    9,
    9,    9,    9,    9,    9,    9,    0,    0,    0,    0,
    9,    0,   11,   11,   11,   11,   11,   11,   11,    0,
   38,    0,    0,   11,   49,   36,   34,    0,   35,    0,
   37,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   16,    0,    0,    0,    0,   15,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    6,    7,    8,    9,   10,   17,    0,    0,    0,
    0,    0,    0,   11,   12,   13,   14,   39,   24,   25,
    0,    0,    0,    0,   44,    0,   47,    0,    0,   50,
   51,   52,   53,   54,   55,   56,   57,   58,   59,   60,
   61,   62,   63,    0,    0,   26,   27,   28,   29,   30,
    0,    0,    0,    0,    0,    0,   33,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   74,    0,    0,    0,    0,    0,    0,    0,
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
    0,    0,    0,    0,    0,   26,   27,   28,   29,   30,
   31,   32,    0,    0,    0,    0,   33,    0,    0,    6,
    7,    8,    9,   10,    0,    0,    0,    0,    0,    0,
    0,   11,   12,   13,   14,
};
short yycheck[] = {                                      10,
   45,   10,   61,   44,   37,   64,   10,   37,   61,   42,
   91,   10,   42,   43,   47,   45,   44,   47,   41,   37,
   40,   44,  257,   10,   42,   43,   37,   45,  257,   47,
   41,   42,   43,   44,   45,   10,   47,   41,   37,   64,
   44,   91,   41,   42,   43,   44,   45,   94,   47,  259,
   37,   10,   93,   93,   41,   42,   43,   44,   45,   10,
   47,   94,   37,   10,   94,   93,   41,   42,   43,   44,
   45,   10,   47,   -1,   41,   41,   94,   44,   44,   93,
   91,   -1,   41,   94,   43,   44,   45,  256,  257,   10,
   41,   20,   91,   44,   41,   94,   -1,   44,   37,   -1,
   -1,   10,   41,   42,   43,   44,   45,   94,   47,   -1,
   -1,   -1,   -1,   10,   -1,   -1,   37,   -1,   -1,   94,
   41,   42,   43,   44,   45,   10,   47,   -1,   37,   -1,
   -1,   -1,   41,   42,   43,   44,   45,   10,   47,   -1,
   37,   -1,   -1,   -1,   41,   42,   43,   44,   45,   10,
   47,   -1,   37,   -1,   67,   -1,   -1,   42,   43,   72,
   45,   10,   47,   -1,   77,   -1,   -1,   -1,   41,   -1,
   43,   44,   45,   10,   -1,   -1,   -1,   -1,   -1,   -1,
   41,   -1,   -1,   44,   -1,   10,   -1,   -1,   -1,   -1,
   -1,   -1,   41,   -1,   -1,   44,   -1,   10,   -1,   -1,
   -1,   -1,   -1,   -1,   41,   -1,   -1,   44,   -1,   94,
   10,   -1,   -1,   -1,  259,   -1,   41,   -1,   -1,   44,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   41,   -1,
   -1,   44,  262,  263,  264,  265,  266,  267,  268,   -1,
   -1,   41,   -1,  273,   44,   -1,   -1,   -1,   -1,   -1,
   -1,  262,  263,  264,  265,  266,  267,  268,   -1,   -1,
   -1,   -1,  273,  262,  263,  264,  265,  266,  267,  268,
   -1,   -1,   -1,   -1,  273,  262,  263,  264,  265,  266,
  267,  268,   -1,   -1,   -1,   -1,  273,  262,  263,  264,
  265,  266,  267,  268,   40,   41,   -1,   -1,  273,   45,
   -1,   -1,   -1,  262,  263,  264,  265,  266,  267,  268,
   -1,   -1,   -1,   -1,  273,   -1,  267,  268,   64,   -1,
  267,  268,   -1,  262,  263,  264,  265,  266,  267,  268,
   37,   -1,   -1,   -1,  273,   42,   43,   -1,   45,   -1,
   47,  262,  263,  264,  265,  266,  267,  268,   -1,   -1,
   -1,   -1,  273,  262,  263,  264,  265,  266,  267,  268,
   -1,   -1,   -1,   -1,  273,  262,  263,  264,  265,  266,
  267,  268,   -1,   -1,   -1,   -1,  273,  262,  263,  264,
  265,  266,  267,  268,   -1,   -1,   -1,   94,  273,  262,
  263,  264,  265,  266,  267,  268,   -1,   -1,   -1,   -1,
  273,  262,  263,  264,  265,  266,  267,  268,   -1,   -1,
   -1,   -1,  273,  262,  263,  264,  265,  266,  267,  268,
   -1,   -1,   -1,   -1,  273,  262,  263,  264,  265,  266,
  267,  268,   -1,   -1,   -1,   -1,  273,  262,  263,  264,
  265,  266,  267,  268,   -1,   -1,   -1,   -1,  273,  262,
  263,  264,  265,  266,  267,  268,   -1,   -1,   -1,   -1,
  273,   -1,  262,  263,  264,  265,  266,  267,  268,   -1,
   37,   -1,   -1,  273,   41,   42,   43,   -1,   45,   -1,
   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   40,   -1,   -1,   -1,   -1,   45,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  257,  258,  259,  260,  261,   64,   -1,   -1,   -1,
   -1,   -1,   -1,  269,  270,  271,  272,   94,   15,   16,
   -1,   -1,   -1,   -1,   21,   -1,   23,   -1,   -1,   26,
   27,   28,   29,   30,   31,   32,   33,   34,   35,   36,
   37,   38,   39,   -1,   -1,  262,  263,  264,  265,  266,
   -1,   -1,   -1,   -1,   -1,   -1,  273,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   69,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  262,  263,  264,  265,  266,
  267,  268,   -1,   -1,   -1,   -1,  273,   -1,   -1,  257,
  258,  259,  260,  261,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  269,  270,  271,  272,
};
#define YYFINAL 3
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 274
#if YYDEBUG
char *yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,"'\\n'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,"'%'",0,0,"'('","')'","'*'","'+'","','","'-'",0,"'/'",0,0,0,0,0,0,0,
0,0,0,0,0,0,"'='",0,0,"'@'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
"'['",0,"']'","'^'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,"NAME","STRING","INTEGER","FLOAT","FUNCTION","GT","GE",
"EQ","LT","LE","AND","OR","COLOR_GRAY","COLOR_RED","COLOR_BLUE","COLOR_GREEN",
"NE","UMINUS",
};
char *yyrule[] = {
"$accept : stmt",
"stmt : NAME '=' exp '\\n'",
"stmt : error '\\n'",
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
"exp : map",
"exp : mapmod map",
"exp : mapmod map '[' index ']'",
"exp : mapmod map '[' index ',' index ']'",
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
#endif
#ifndef YYSTYPE
typedef int YYSTYPE;
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
#line 82 "pol.y"
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
	mapname (n,'M',0,0);
    return 0;
}

int mapname (int n,char code, int row,int col)
{
    printf ("M%c %d %d %s\n", code, row, col, storage[n]);
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


#line 478 "y.tab.c"
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
#line 22 "pol.y"
{ assign(yyvsp[-3]); return 1; }
break;
case 2:
#line 23 "pol.y"
{ return 0; }
break;
case 4:
#line 27 "pol.y"
{ logical ("&"); }
break;
case 5:
#line 28 "pol.y"
{ logical ("|"); }
break;
case 6:
#line 29 "pol.y"
{ compare (">"); }
break;
case 7:
#line 30 "pol.y"
{ compare (">="); }
break;
case 8:
#line 31 "pol.y"
{ compare ("<"); }
break;
case 9:
#line 32 "pol.y"
{ compare ("<="); }
break;
case 10:
#line 33 "pol.y"
{ compare ("="); }
break;
case 11:
#line 34 "pol.y"
{ compare ("!"); }
break;
case 12:
#line 35 "pol.y"
{ binary_opcode ("^"); }
break;
case 13:
#line 36 "pol.y"
{ binary_opcode ("*"); }
break;
case 14:
#line 37 "pol.y"
{ binary_opcode ("/"); }
break;
case 15:
#line 38 "pol.y"
{ binary_opcode ("%"); }
break;
case 16:
#line 39 "pol.y"
{ binary_opcode ("+"); }
break;
case 17:
#line 40 "pol.y"
{ binary_opcode ("-"); }
break;
case 18:
#line 41 "pol.y"
{ unary_opcode ("-"); }
break;
case 19:
#line 42 "pol.y"
{ define_variable(yyvsp[-2]); }
break;
case 20:
#line 43 "pol.y"
{ name (yyvsp[0]); }
break;
case 21:
#line 44 "pol.y"
{ mapname (yyvsp[0],'M',0,0); }
break;
case 22:
#line 45 "pol.y"
{ mapname (yyvsp[-3],'M',yyvsp[-1],0); }
break;
case 23:
#line 47 "pol.y"
{ mapname (yyvsp[-5],'M',yyvsp[-3],yyvsp[-1]); }
break;
case 24:
#line 48 "pol.y"
{ mapname (yyvsp[0],'M',0,0); }
break;
case 25:
#line 49 "pol.y"
{ mapname (yyvsp[0],yyvsp[-1],0,0); }
break;
case 26:
#line 50 "pol.y"
{ mapname (yyvsp[-3],yyvsp[-4],yyvsp[-1],0); }
break;
case 27:
#line 52 "pol.y"
{ mapname (yyvsp[-5],yyvsp[-6],yyvsp[-3],yyvsp[-1]); }
break;
case 28:
#line 53 "pol.y"
{ function (yyvsp[-2]); }
break;
case 29:
#line 54 "pol.y"
{ function (yyvsp[-3]); }
break;
case 30:
#line 55 "pol.y"
{ integer (yyvsp[0]); }
break;
case 31:
#line 56 "pol.y"
{ floating_point (yyvsp[0]); }
break;
case 33:
#line 61 "pol.y"
{ char buf[1024];
			    sprintf (buf, "%s@%s", storage[yyvsp[-2]], storage[yyvsp[0]]);
			    yyval = store(buf);
			  }
break;
case 34:
#line 67 "pol.y"
{ yyval = '@'; }
break;
case 35:
#line 68 "pol.y"
{ yyval = '#'; }
break;
case 36:
#line 69 "pol.y"
{ yyval = 'r'; }
break;
case 37:
#line 70 "pol.y"
{ yyval = 'g'; }
break;
case 38:
#line 71 "pol.y"
{ yyval = 'b'; }
break;
case 39:
#line 74 "pol.y"
{ another_arg(); }
break;
case 40:
#line 75 "pol.y"
{ another_arg(); }
break;
case 41:
#line 78 "pol.y"
{ yyval = yyvsp[0]; }
break;
case 42:
#line 79 "pol.y"
{ yyval = -yyvsp[0]; }
break;
#line 782 "y.tab.c"
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
