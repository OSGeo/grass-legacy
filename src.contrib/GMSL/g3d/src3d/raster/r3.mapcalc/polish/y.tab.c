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
#line 3 "pol.y"
static int nstored = 0;
static char **storage = 0;
#line 16 "y.tab.c"
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
    1,    1,    2,    2,    4,    4,    4,    4,    4,    5,
    5,    3,    3,
};
short yylen[] = {                                         2,
    4,    2,    3,    3,    3,    3,    3,    3,    3,    3,
    3,    3,    3,    3,    3,    3,    2,    3,    1,    1,
    4,    6,    8,    1,    2,    5,    7,    9,    3,    4,
    1,    1,    1,    3,    1,    1,    1,    1,    1,    1,
    3,    1,    2,
};
short yydefred[] = {                                      0,
    0,    0,    0,    2,    0,    0,   20,   31,   32,    0,
   36,   37,   39,   38,    0,    0,   35,    0,    0,    0,
    0,    0,    0,   17,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    1,    0,
    0,    0,    0,   34,   29,    0,    0,    3,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   12,   13,
   14,   42,    0,    0,    0,   30,    0,   43,   21,    0,
    0,    0,    0,   26,    0,   22,    0,    0,    0,   27,
    0,   23,    0,   28,
};
short yydgoto[] = {                                       3,
   18,   19,   64,   20,   47,
};
short yysindex[] = {                                   -227,
   -2,  -47,    0,    0,  382,   25,    0,    0,    0,    8,
    0,    0,    0,    0,  382,  382,    0,   40,  -53, -217,
  382, -215,  160,    0,  -36,  382,  382,  382,  382,  382,
  382,  382,  382,  382,  382,  382,  382,  382,    0,  -43,
   -4,  -37,  -24,    0,    0,  -24,  -16,    0,   71,   71,
   71,   71,   71,  150,  150,   71,   37,   37,    0,    0,
    0,    0, -197,  -41,  -43,    0,  382,    0,    0,  -43,
  -40,  -24,  -28,    0,  -43,    0,  -43,  -27,  -15,    0,
  -43,    0,  -13,    0,
};
short yyrindex[] = {                                      0,
    0,    0,    0,    0,    0,  -10,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   14,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    2,   26,   31,    0,    0,   50,    0,    0,   78,   90,
  102,  114,  126,   82,   94,  138,   54,   66,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   61,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,
};
short yygindex[] = {                                      0,
  421,   70,  -55,    0,    0,
};
#define YYTABLESIZE 654
short yytable[] = {                                      19,
   38,   63,   70,   75,   48,   36,   34,    4,   35,   71,
   37,   33,   38,    5,   73,   77,   81,   36,   34,   78,
   35,   79,   37,   24,   66,   83,   19,   67,    1,    2,
   19,   19,   19,   19,   19,   25,   19,   40,   33,   41,
   18,   44,   33,   33,   33,   33,   33,   23,   33,   39,
   24,   69,   74,   65,   24,   24,   24,   24,   24,   22,
   24,   68,   25,   15,   76,   80,   25,   25,   25,   25,
   25,   18,   25,   38,   18,   16,   38,   82,   36,   84,
   33,   36,   34,   37,   35,   21,   37,    6,   22,   42,
   40,    4,   33,   40,   15,    0,   15,   15,   15,    7,
    0,   41,    0,    5,   41,    0,   16,   38,   16,   16,
   16,   10,   36,   34,    0,   35,    0,   37,    6,    0,
    0,    6,    4,    8,    0,    4,    0,    0,    0,    0,
    7,    0,    0,    7,    5,    9,    0,    5,    0,    0,
    0,    0,   10,    0,    0,   10,    0,   11,    0,    0,
    0,    0,    0,    0,    8,    0,    0,    8,    0,    0,
    0,    0,    0,    0,    0,    0,    9,    0,    0,    9,
    0,    0,    0,    0,    0,    0,    0,    0,   11,    0,
    0,   11,    0,    0,    0,    0,   38,    0,    0,    0,
    0,   36,   34,    0,   35,    0,   37,    0,    0,   16,
   45,    0,    0,    0,   15,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   62,    0,    0,    0,    0,
    0,    0,    0,   17,    0,   26,   27,   28,   29,   30,
   31,   32,    0,    0,    0,    0,   33,   26,   27,   28,
   29,   30,   31,   32,    0,    0,    0,    0,   33,    0,
    0,   19,   19,   19,   19,   19,   19,   19,    0,    0,
    0,    0,   19,   33,   33,   33,   33,   33,   33,   33,
    0,    0,    0,    0,   33,   24,   24,   24,   24,   24,
   24,   24,    0,    0,    0,    0,   24,   25,   25,   25,
   25,   25,   25,   25,    0,    0,    0,    0,   25,    0,
    0,   26,   27,   28,   29,   30,   31,   32,    0,    0,
    0,    0,   33,    0,    0,   15,   15,   15,   15,   15,
   15,   15,    0,    0,    0,    0,   15,   16,   16,   16,
   16,   16,   16,   16,    0,    0,    0,    0,   16,    6,
    6,    6,    6,    6,    6,    6,    0,    0,    4,    4,
    6,    7,    7,    7,    7,    7,    7,    7,    0,    0,
    5,    5,    7,   10,   10,   10,   10,   10,   10,   10,
    0,    0,    0,    0,   10,    8,    8,    8,    8,    8,
    8,    8,    0,    0,    0,    0,    8,    9,    9,    9,
    9,    9,    9,    9,    0,    0,    0,    0,    9,   11,
   11,   11,   11,   11,   11,   11,    0,    0,    0,    0,
   11,   26,   27,   28,   29,   30,    6,    7,    8,    9,
   10,   16,   33,    0,    0,    0,   15,    0,   11,   12,
   13,   14,    0,    0,    0,   24,   25,    0,    0,    0,
    0,   43,    0,   46,    0,   17,   49,   50,   51,   52,
   53,   54,   55,   56,   57,   58,   59,   60,   61,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,   72,    0,    0,
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
    0,    0,    0,    0,    0,    0,    0,    0,    6,    7,
    8,    9,   10,    0,    0,    0,    0,    0,    0,    0,
   11,   12,   13,   14,
};
short yycheck[] = {                                      10,
   37,   45,   44,   44,   41,   42,   43,   10,   45,   65,
   47,   10,   37,   61,   70,   44,   44,   42,   43,   75,
   45,   77,   47,   10,   41,   81,   37,   44,  256,  257,
   41,   42,   43,   44,   45,   10,   47,   91,   37,  257,
   10,  257,   41,   42,   43,   44,   45,   40,   47,   10,
   37,   93,   93,   91,   41,   42,   43,   44,   45,   64,
   47,  259,   37,   10,   93,   93,   41,   42,   43,   44,
   45,   41,   47,   37,   44,   10,   37,   93,   42,   93,
   91,   42,   43,   47,   45,   61,   47,   10,   64,   20,
   41,   10,   91,   44,   41,   -1,   43,   44,   45,   10,
   -1,   41,   -1,   10,   44,   -1,   41,   37,   43,   44,
   45,   10,   42,   43,   -1,   45,   -1,   47,   41,   -1,
   -1,   44,   41,   10,   -1,   44,   -1,   -1,   -1,   -1,
   41,   -1,   -1,   44,   41,   10,   -1,   44,   -1,   -1,
   -1,   -1,   41,   -1,   -1,   44,   -1,   10,   -1,   -1,
   -1,   -1,   -1,   -1,   41,   -1,   -1,   44,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   41,   -1,   -1,   44,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   41,   -1,
   -1,   44,   -1,   -1,   -1,   -1,   37,   -1,   -1,   -1,
   -1,   42,   43,   -1,   45,   -1,   47,   -1,   -1,   40,
   41,   -1,   -1,   -1,   45,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  259,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   64,   -1,  262,  263,  264,  265,  266,
  267,  268,   -1,   -1,   -1,   -1,  273,  262,  263,  264,
  265,  266,  267,  268,   -1,   -1,   -1,   -1,  273,   -1,
   -1,  262,  263,  264,  265,  266,  267,  268,   -1,   -1,
   -1,   -1,  273,  262,  263,  264,  265,  266,  267,  268,
   -1,   -1,   -1,   -1,  273,  262,  263,  264,  265,  266,
  267,  268,   -1,   -1,   -1,   -1,  273,  262,  263,  264,
  265,  266,  267,  268,   -1,   -1,   -1,   -1,  273,   -1,
   -1,  262,  263,  264,  265,  266,  267,  268,   -1,   -1,
   -1,   -1,  273,   -1,   -1,  262,  263,  264,  265,  266,
  267,  268,   -1,   -1,   -1,   -1,  273,  262,  263,  264,
  265,  266,  267,  268,   -1,   -1,   -1,   -1,  273,  262,
  263,  264,  265,  266,  267,  268,   -1,   -1,  267,  268,
  273,  262,  263,  264,  265,  266,  267,  268,   -1,   -1,
  267,  268,  273,  262,  263,  264,  265,  266,  267,  268,
   -1,   -1,   -1,   -1,  273,  262,  263,  264,  265,  266,
  267,  268,   -1,   -1,   -1,   -1,  273,  262,  263,  264,
  265,  266,  267,  268,   -1,   -1,   -1,   -1,  273,  262,
  263,  264,  265,  266,  267,  268,   -1,   -1,   -1,   -1,
  273,  262,  263,  264,  265,  266,  257,  258,  259,  260,
  261,   40,  273,   -1,   -1,   -1,   45,   -1,  269,  270,
  271,  272,   -1,   -1,   -1,   15,   16,   -1,   -1,   -1,
   -1,   21,   -1,   23,   -1,   64,   26,   27,   28,   29,
   30,   31,   32,   33,   34,   35,   36,   37,   38,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   67,   -1,   -1,
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
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  257,  258,
  259,  260,  261,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  269,  270,  271,  272,
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
"'['",0,"']'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,"NAME","STRING","INTEGER","FLOAT","FUNCTION","GT","GE","EQ",
"LT","LE","AND","OR","COLOR_GRAY","COLOR_RED","COLOR_BLUE","COLOR_GREEN","NE",
"UMINUS",
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
#line 81 "pol.y"

yywrap()
{
    return 1;
}
yyerror(s) char *s;
{
    printf ("??\n");
}

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
	mapname (n,'M',0,0,0);
}

mapname (n, code, row, col, depth)
    char code;
{
    printf ("M%c %d %d %d %s\n", code, row, col, depth, storage[n]);
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
#line 450 "y.tab.c"
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
#line 17 "pol.y"
{ assign(yyvsp[-3]); return 1; }
break;
case 2:
#line 18 "pol.y"
{ return 0; }
break;
case 4:
#line 22 "pol.y"
{ logical ("&"); }
break;
case 5:
#line 23 "pol.y"
{ logical ("|"); }
break;
case 6:
#line 24 "pol.y"
{ compare (">"); }
break;
case 7:
#line 25 "pol.y"
{ compare (">="); }
break;
case 8:
#line 26 "pol.y"
{ compare ("<"); }
break;
case 9:
#line 27 "pol.y"
{ compare ("<="); }
break;
case 10:
#line 28 "pol.y"
{ compare ("="); }
break;
case 11:
#line 29 "pol.y"
{ compare ("!"); }
break;
case 12:
#line 30 "pol.y"
{ binary_opcode ("*"); }
break;
case 13:
#line 31 "pol.y"
{ binary_opcode ("/"); }
break;
case 14:
#line 32 "pol.y"
{ binary_opcode ("%"); }
break;
case 15:
#line 33 "pol.y"
{ binary_opcode ("+"); }
break;
case 16:
#line 34 "pol.y"
{ binary_opcode ("-"); }
break;
case 17:
#line 35 "pol.y"
{ unary_opcode ("-"); }
break;
case 18:
#line 36 "pol.y"
{ define_variable(yyvsp[-2]); }
break;
case 19:
#line 37 "pol.y"
{ name (yyvsp[0]); }
break;
case 20:
#line 38 "pol.y"
{ mapname (yyvsp[0],'M',0,0,0); }
break;
case 21:
#line 39 "pol.y"
{ mapname (yyvsp[-3],'M',yyvsp[-1],0,0); }
break;
case 22:
#line 41 "pol.y"
{ mapname (yyvsp[-5],'M',yyvsp[-3],yyvsp[-1],0); }
break;
case 23:
#line 43 "pol.y"
{ mapname (yyvsp[-7],'M',yyvsp[-5],yyvsp[-3],yyvsp[-1]); }
break;
case 24:
#line 44 "pol.y"
{ mapname (yyvsp[0],'M',0,0,0); }
break;
case 25:
#line 45 "pol.y"
{ mapname (yyvsp[0],yyvsp[-1],0,0,0); }
break;
case 26:
#line 46 "pol.y"
{ mapname (yyvsp[-3],yyvsp[-4],yyvsp[-1],0,0); }
break;
case 27:
#line 48 "pol.y"
{ mapname (yyvsp[-5],yyvsp[-6],yyvsp[-3],yyvsp[-1],0); }
break;
case 28:
#line 50 "pol.y"
{ mapname (yyvsp[-7],yyvsp[-8],yyvsp[-5],yyvsp[-3],yyvsp[-1]); }
break;
case 29:
#line 52 "pol.y"
{ function (yyvsp[-2]); }
break;
case 30:
#line 53 "pol.y"
{ function (yyvsp[-3]); }
break;
case 31:
#line 54 "pol.y"
{ integer (yyvsp[0]); }
break;
case 32:
#line 55 "pol.y"
{ floating_point (yyvsp[0]); }
break;
case 34:
#line 60 "pol.y"
{ char buf[1024];
			    sprintf (buf, "%s@%s", storage[yyvsp[-2]], storage[yyvsp[0]]);
			    yyval = store(buf);
			  }
break;
case 35:
#line 66 "pol.y"
{ yyval = '@'; }
break;
case 36:
#line 67 "pol.y"
{ yyval = '#'; }
break;
case 37:
#line 68 "pol.y"
{ yyval = 'r'; }
break;
case 38:
#line 69 "pol.y"
{ yyval = 'g'; }
break;
case 39:
#line 70 "pol.y"
{ yyval = 'b'; }
break;
case 40:
#line 73 "pol.y"
{ another_arg(); }
break;
case 41:
#line 74 "pol.y"
{ another_arg(); }
break;
case 42:
#line 77 "pol.y"
{ yyval = yyvsp[0]; }
break;
case 43:
#line 78 "pol.y"
{ yyval = -yyvsp[0]; }
break;
#line 758 "y.tab.c"
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
