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
#line 2 "gis_pars.y"
#include "stdio.h"
#include "display.h"
#include "D.h"
#include <stdlib.h>
#include "raster.h"
#include "include.h"
#line 10 "gis_pars.y"
typedef union
{
        long            long_val;
        int              int_val;
        char            *str_val;
} YYSTYPE;
#line 27 "y.tab.c"
#define ERAS_TKN 257
#define LST_TKN 258
#define COLR_TKN 259
#define PRT_TKN 260
#define ASG_TKN 261
#define ANAL_TKN 262
#define UNCH_TKN 263
#define CHOS_TKN 264
#define REC_TKN 265
#define SAV_TKN 266
#define MAP_TKN 267
#define CATS_TKN 268
#define LINE_TKN 269
#define EXEC_TKN 270
#define QUIT_TKN 271
#define HELP_TKN 272
#define PAREN_TKN 273
#define ADD_TKN 274
#define MULT_TKN 275
#define NAM_STR 276
#define LONG_NUM 277
#define YYERRCODE 256
short yylhs[] = {                                        -1,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    1,    1,    1,    1,    6,    2,
    3,    3,    3,    5,    5,    5,    5,    5,    5,    4,
    4,    4,    4,    4,    4,    7,    7,    8,    8,    9,
   11,   11,   11,   11,   11,   11,   11,   10,   12,   12,
   12,   12,   12,   12,   12,   12,   12,   12,   12,   12,
   12,   12,   12,   12,   12,   12,   13,
};
short yylen[] = {                                         2,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    3,    4,    4,    3,    3,    3,
    5,    3,    6,    3,    4,    5,    6,    7,    8,    3,
    4,    5,    6,    7,    8,    3,    2,    3,    2,    2,
    2,    2,    2,    4,    4,    3,    3,    2,    2,    3,
    3,    3,    3,    3,    3,    3,    3,    3,    3,    3,
    3,    3,    3,    3,    3,    3,    1,
};
short yydefred[] = {                                      0,
   14,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   67,    0,    0,    0,    0,    0,    0,    1,    2,    3,
    4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
   43,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   39,    0,   37,    0,   40,   48,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   49,
    0,    0,    0,    0,    0,    0,   41,   42,   18,    0,
   47,    0,   46,   15,    0,    0,   19,   20,   22,    0,
   24,    0,   30,    0,   38,   36,   51,   50,   52,   53,
   54,   55,   56,   57,   58,   59,   60,   61,   62,   63,
   64,   65,   66,   45,   44,   16,   17,    0,   25,    0,
   31,    0,   21,    0,   26,    0,   32,    0,   23,   27,
    0,   33,    0,   28,    0,   34,    0,   29,   35,
};
short yydgoto[] = {                                      17,
   18,   19,   20,   21,   22,   23,   24,   25,   26,   27,
   28,   29,   30,
};
short yysindex[] = {                                   -256,
    0, -263, -223, -265, -250, -259, -241, -230, -222, -219,
    0, -194, -191, -238, -190, -189,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0, -187, -211, -210, -186, -192, -183, -182, -181, -229,
 -214, -213,    0, -180,    0, -179,    0,    0, -178, -177,
 -176, -175, -174, -173, -172, -171, -170, -169, -168,    0,
 -167, -166, -165, -164, -163, -162,    0,    0,    0, -161,
    0, -160,    0,    0, -159, -158,    0,    0,    0, -157,
    0, -209,    0, -208,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0, -228,    0, -205,
    0, -204,    0, -156,    0, -203,    0, -200,    0,    0,
 -199,    0, -195,    0, -155,    0, -154,    0,    0,
};
short yyrindex[] = {                                      0,
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
    0,    0,    0,    0,    0,    0,    0,    0,    0,
};
short yygindex[] = {                                      0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,
};
#define YYTABLESIZE 120
short yytable[] = {                                       1,
    2,    3,    4,    5,    6,   31,    7,    8,    9,   10,
   38,   39,   11,   12,   13,   14,   40,   15,   16,   49,
   50,   51,   52,   53,   54,   55,   56,   57,   58,   59,
   60,   61,   62,   63,   41,   64,   65,   66,   32,   79,
  113,   33,   34,   35,   36,   42,   43,   80,  114,   45,
   70,   72,   37,   44,   81,   83,   46,   71,   73,  109,
  111,   82,   84,  115,  117,  120,  110,  112,  122,  124,
  116,  118,  121,  126,   47,  123,  125,   48,   67,   68,
  127,   69,   74,   75,   76,    0,   77,   78,   85,   86,
   87,   88,   89,   90,   91,   92,   93,   94,   95,   96,
   97,   98,   99,  100,  101,  102,  103,  104,  105,  106,
  107,    0,  119,  128,  129,    0,    0,    0,    0,  108,
};
short yycheck[] = {                                     256,
  257,  258,  259,  260,  261,  269,  263,  264,  265,  266,
  276,  262,  269,  270,  271,  272,  276,  274,  275,  258,
  259,  260,  261,  262,  263,  264,  265,  266,  267,  268,
  269,  270,  271,  272,  276,  274,  275,  276,  262,  269,
  269,  265,  266,  267,  268,  276,  269,  277,  277,  269,
  262,  262,  276,  276,  269,  269,  276,  269,  269,  269,
  269,  276,  276,  269,  269,  269,  276,  276,  269,  269,
  276,  276,  276,  269,  269,  276,  276,  269,  269,  269,
  276,  269,  269,  276,  268,   -1,  269,  269,  269,  269,
  269,  269,  269,  269,  269,  269,  269,  269,  269,  269,
  269,  269,  269,  269,  269,  269,  269,  269,  269,  269,
  269,   -1,  269,  269,  269,   -1,   -1,   -1,   -1,  277,
};
#define YYFINAL 17
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 277
#if YYDEBUG
char *yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"ERAS_TKN","LST_TKN","COLR_TKN",
"PRT_TKN","ASG_TKN","ANAL_TKN","UNCH_TKN","CHOS_TKN","REC_TKN","SAV_TKN",
"MAP_TKN","CATS_TKN","LINE_TKN","EXEC_TKN","QUIT_TKN","HELP_TKN","PAREN_TKN",
"ADD_TKN","MULT_TKN","NAM_STR","LONG_NUM",
};
char *yyrule[] = {
"$accept : weight_cmnd",
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
#line 246 "gis_pars.y"
/* ========================================================================= */
int yyerror(char *message )
{
        printf ("What?  Type help for help (%s)\n", message);

	return 0;
}
/* ========================================================================= */
#line 270 "y.tab.c"
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
#line 43 "gis_pars.y"
{ return(LIST_EXPR) ; }
break;
case 2:
#line 45 "gis_pars.y"
{ return(PRNT_EXPR) ; }
break;
case 3:
#line 47 "gis_pars.y"
{ return(ASSGN_EXPR) ; }
break;
case 4:
#line 49 "gis_pars.y"
{ return(CHOOS_EXPR) ; }
break;
case 5:
#line 51 "gis_pars.y"
{ return(UNCHOOS_EXPR) ; }
break;
case 6:
#line 53 "gis_pars.y"
{ return(COLR_EXPR) ; }
break;
case 7:
#line 55 "gis_pars.y"
{ return(SAVE_EXPR) ; }
break;
case 8:
#line 57 "gis_pars.y"
{ return(RECOV_EXPR) ; }
break;
case 9:
#line 59 "gis_pars.y"
{ return(EXECUT_EXPR) ; }
break;
case 10:
#line 61 "gis_pars.y"
{ return(QUIT_EXPR) ; }
break;
case 11:
#line 63 "gis_pars.y"
{ return(MISC_EXPR) ; }
break;
case 12:
#line 65 "gis_pars.y"
{ return(HELP_EXPR) ; }
break;
case 13:
#line 67 "gis_pars.y"
{ return(NULL_EXPR) ; }
break;
case 14:
#line 69 "gis_pars.y"
{ return(ERR_EXPR) ; }
break;
case 15:
#line 72 "gis_pars.y"
{ list_maps() ; }
break;
case 16:
#line 74 "gis_pars.y"
{ list_cats(yyvsp[-1].str_val) ; }
break;
case 17:
#line 76 "gis_pars.y"
{ list_cats(yyvsp[-2].str_val) ; }
break;
case 18:
#line 78 "gis_pars.y"
{ list_analysis(0) ; }
break;
case 19:
#line 81 "gis_pars.y"
{ select_colors (yyvsp[-1].str_val) ; }
break;
case 20:
#line 84 "gis_pars.y"
{ list_analysis(1) ; }
break;
case 21:
#line 87 "gis_pars.y"
{ assign_single(yyvsp[-3].str_val, yyvsp[-2].long_val, yyvsp[-1].long_val) ; }
break;
case 22:
#line 89 "gis_pars.y"
{ ask_weights(yyvsp[-1].str_val) ; }
break;
case 23:
#line 91 "gis_pars.y"
{ assign_mult(yyvsp[-4].str_val, yyvsp[-3].long_val, yyvsp[-2].long_val, yyvsp[-1].long_val) ; }
break;
case 24:
#line 94 "gis_pars.y"
{ unchoose_map(yyvsp[-1].str_val) ; }
break;
case 25:
#line 96 "gis_pars.y"
{
					unchoose_map(yyvsp[-2].str_val) ;
					unchoose_map(yyvsp[-1].str_val) ;
				}
break;
case 26:
#line 101 "gis_pars.y"
{
					unchoose_map(yyvsp[-3].str_val) ;
					unchoose_map(yyvsp[-2].str_val) ;
					unchoose_map(yyvsp[-1].str_val) ;
				}
break;
case 27:
#line 107 "gis_pars.y"
{
					unchoose_map(yyvsp[-4].str_val) ;
					unchoose_map(yyvsp[-3].str_val) ;
					unchoose_map(yyvsp[-2].str_val) ;
					unchoose_map(yyvsp[-1].str_val) ;
				}
break;
case 28:
#line 114 "gis_pars.y"
{
					unchoose_map(yyvsp[-5].str_val) ;
					unchoose_map(yyvsp[-4].str_val) ;
					unchoose_map(yyvsp[-3].str_val) ;
					unchoose_map(yyvsp[-2].str_val) ;
					unchoose_map(yyvsp[-1].str_val) ;
				}
break;
case 29:
#line 122 "gis_pars.y"
{
					unchoose_map(yyvsp[-6].str_val) ;
					unchoose_map(yyvsp[-5].str_val) ;
					unchoose_map(yyvsp[-4].str_val) ;
					unchoose_map(yyvsp[-3].str_val) ;
					unchoose_map(yyvsp[-2].str_val) ;
					unchoose_map(yyvsp[-1].str_val) ;
				}
break;
case 30:
#line 132 "gis_pars.y"
{ choose_map(yyvsp[-1].str_val) ; }
break;
case 31:
#line 134 "gis_pars.y"
{
					choose_map(yyvsp[-2].str_val) ;
					choose_map(yyvsp[-1].str_val) ;
				}
break;
case 32:
#line 139 "gis_pars.y"
{
					choose_map(yyvsp[-3].str_val) ;
					choose_map(yyvsp[-2].str_val) ;
					choose_map(yyvsp[-1].str_val) ;
				}
break;
case 33:
#line 145 "gis_pars.y"
{
					choose_map(yyvsp[-4].str_val) ;
					choose_map(yyvsp[-3].str_val) ;
					choose_map(yyvsp[-2].str_val) ;
					choose_map(yyvsp[-1].str_val) ;
				}
break;
case 34:
#line 152 "gis_pars.y"
{
					choose_map(yyvsp[-5].str_val) ;
					choose_map(yyvsp[-4].str_val) ;
					choose_map(yyvsp[-3].str_val) ;
					choose_map(yyvsp[-2].str_val) ;
					choose_map(yyvsp[-1].str_val) ;
				}
break;
case 35:
#line 160 "gis_pars.y"
{
					choose_map(yyvsp[-6].str_val) ;
					choose_map(yyvsp[-5].str_val) ;
					choose_map(yyvsp[-4].str_val) ;
					choose_map(yyvsp[-3].str_val) ;
					choose_map(yyvsp[-2].str_val) ;
					choose_map(yyvsp[-1].str_val) ;
				}
break;
case 36:
#line 170 "gis_pars.y"
{ save(yyvsp[-1].str_val) ; }
break;
case 37:
#line 172 "gis_pars.y"
{ save(NUL_STR) ; }
break;
case 38:
#line 175 "gis_pars.y"
{ recover(yyvsp[-1].str_val) ; }
break;
case 39:
#line 177 "gis_pars.y"
{ recover(NUL_STR) ; }
break;
case 40:
#line 180 "gis_pars.y"
{ execute() ; }
break;
case 41:
#line 183 "gis_pars.y"
{ set_to_add() ; }
break;
case 42:
#line 185 "gis_pars.y"
{ set_to_mult() ; }
break;
case 43:
#line 187 "gis_pars.y"
{ if (at_console())
					    {
						R_color(D_translate_color("black")) ;
						D_erase_window() ;
						R_flush() ;
					    }
					}
break;
case 44:
#line 195 "gis_pars.y"
{ system("ls -l") ; }
break;
case 45:
#line 197 "gis_pars.y"
{ system("ls -l") ; }
break;
case 46:
#line 199 "gis_pars.y"
{ system("ls -l") ; }
break;
case 47:
#line 201 "gis_pars.y"
{ system("ls -l") ; }
break;
case 48:
#line 204 "gis_pars.y"
{;}
break;
case 49:
#line 207 "gis_pars.y"
{ G_gishelp("WEIGHT", "general") ; }
break;
case 50:
#line 209 "gis_pars.y"
{ G_gishelp("WEIGHT", "COLR") ; }
break;
case 51:
#line 211 "gis_pars.y"
{ G_gishelp("WEIGHT", "LST") ; }
break;
case 52:
#line 213 "gis_pars.y"
{ G_gishelp("WEIGHT", "PRT") ; }
break;
case 53:
#line 215 "gis_pars.y"
{ G_gishelp("WEIGHT", "ASG") ; }
break;
case 54:
#line 217 "gis_pars.y"
{ G_gishelp("WEIGHT", "ANAL") ; }
break;
case 55:
#line 219 "gis_pars.y"
{ G_gishelp("WEIGHT", "UNCH") ; }
break;
case 56:
#line 221 "gis_pars.y"
{ G_gishelp("WEIGHT", "CHOS") ; }
break;
case 57:
#line 223 "gis_pars.y"
{ G_gishelp("WEIGHT", "REC") ; }
break;
case 58:
#line 225 "gis_pars.y"
{ G_gishelp("WEIGHT", "SAV") ; }
break;
case 59:
#line 227 "gis_pars.y"
{ G_gishelp("WEIGHT", "MAP") ; }
break;
case 60:
#line 229 "gis_pars.y"
{ G_gishelp("WEIGHT", "CATS") ; }
break;
case 61:
#line 231 "gis_pars.y"
{ G_gishelp("WEIGHT", "EXEC") ; }
break;
case 62:
#line 233 "gis_pars.y"
{ G_gishelp("WEIGHT", "QUIT") ; }
break;
case 63:
#line 235 "gis_pars.y"
{ G_gishelp("WEIGHT", "HELP") ; }
break;
case 64:
#line 237 "gis_pars.y"
{ G_gishelp("WEIGHT", "ADD_MULT") ; }
break;
case 65:
#line 239 "gis_pars.y"
{ G_gishelp("WEIGHT", "ADD_MULT") ; }
break;
case 66:
#line 241 "gis_pars.y"
{ G_gishelp("WEIGHT", yyvsp[-1].str_val) ; }
break;
case 67:
#line 243 "gis_pars.y"
{;}
break;
#line 735 "y.tab.c"
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
