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
#include <stdlib.h>
#include <string.h>
#include "tree.h"
#include "lvw.h"
#include "local_proto.h"

struct Cell_head *get_cur_win() ;
extern struct Node *yytree ;
extern struct Node *e_expr_list[512] ;
extern FILE *yyin ;

struct Group e_yygroup[256]   ;/* limit of 256 nested group expressions */
int   e_yygrp_i=0      ;/* index into current array              */
#line 17 "gis_pars.y"
typedef union
{
	int              int_val;
	struct Node     *nod_val;
	char            *str_val;
} YYSTYPE;
#line 34 "y.tab.c"
#define AND_TKN 257
#define OR_TKN 258
#define NOT_TKN 259
#define GRP_TKN 260
#define CATS_TKN 261
#define EXPR_TKN 262
#define RANGE_TKN 263
#define NAM_TKN 264
#define OVR_TKN 265
#define COV_TKN 266
#define WIN_TKN 267
#define BYE_TKN 268
#define ERA_TKN 269
#define HST_TKN 270
#define HLP_TKN 271
#define NAM_STR 272
#define INUMBER 273
#define LP 274
#define RP 275
#define SEMI 276
#define YYERRCODE 256
short yylhs[] = {                                        -1,
    0,    0,    0,    0,    0,    0,    0,    0,    1,    1,
    1,    1,    1,    1,    1,    1,    9,    1,    1,    1,
    1,    2,    2,    8,    8,    8,    3,    5,    6,    4,
    4,    4,    4,    4,    4,    4,    4,    4,    4,    4,
    4,    4,    4,    4,    4,    4,    4,    4,    7,
};
short yylen[] = {                                         2,
    1,    1,    1,    1,    1,    1,    1,    1,    5,    6,
    6,    5,    5,    5,    5,    4,    0,    6,    4,    3,
    1,    2,    1,    0,    2,    4,    2,    1,    1,    3,
    1,    2,    2,    2,    2,    2,    2,    2,    2,    2,
    2,    2,    2,    2,    2,    2,    2,    2,    1,
};
short yydefred[] = {                                      0,
    8,    0,    0,   49,   29,   28,    0,   21,    0,    0,
    1,    2,    3,    4,    5,    6,    7,   27,   22,   34,
   35,   36,   37,   38,   39,   40,   41,   42,   43,   44,
   45,   46,   47,   32,   33,    0,    0,    0,   17,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   24,
    0,    0,    0,    0,    0,   30,   20,    0,    0,   16,
    0,   19,    0,    0,    0,    0,    0,   14,   15,    0,
    0,    9,    0,    0,   12,   13,    0,   18,   10,   11,
   26,
};
short yydgoto[] = {                                      10,
   11,   12,   13,   14,   15,   16,   17,   61,   50,
};
short yysindex[] = {                                   -235,
    0, -258, -255,    0,    0,    0, -217,    0, -201,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0, -259, -259, -259,    0, -250,
 -248, -263, -245, -244, -237, -185, -259, -259, -215,    0,
 -213, -259, -243, -252, -259,    0,    0, -209, -208,    0,
 -269,    0, -207, -259, -259, -206, -199,    0,    0, -181,
 -197,    0, -192, -191,    0,    0, -188,    0,    0,    0,
    0,
};
short yyrindex[] = {                                      0,
    0,    0,   86,    0,    0,    0,   88,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0, -186,    0,    0,    0,    0, -266,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,
};
short yygindex[] = {                                      0,
  -36,    0,    0,    0,    0,    0,    0,    0,    0,
};
#define YYTABLESIZE 89
short yytable[] = {                                      47,
   48,   49,    8,   70,   46,   25,   25,   25,   53,   54,
   58,   59,    8,   18,   46,   63,   19,   66,   67,   65,
    1,   46,   51,   52,   71,    2,   55,   73,   74,   64,
   56,    3,    4,    5,    6,    7,    8,   57,    9,   20,
   21,   22,   23,   24,   25,   26,   27,   28,   29,   30,
   31,   32,   33,   34,   35,   36,   37,   38,   39,   60,
   40,   62,   41,   42,   43,   68,   69,   72,   75,   44,
   45,   36,   37,   38,   39,   76,   40,   78,   41,   42,
   43,   77,   79,   80,   81,   23,   45,   31,   21,
};
short yycheck[] = {                                      36,
   37,   38,  272,  273,  274,  272,  273,  274,  272,  273,
   47,   48,  272,  272,  274,   52,  272,   54,   55,  272,
  256,  274,  273,  272,   61,  261,  272,   64,   65,  273,
  275,  267,  268,  269,  270,  271,  272,  275,  274,  257,
  258,  259,  260,  261,  262,  263,  264,  265,  266,  267,
  268,  269,  270,  271,  272,  257,  258,  259,  260,  275,
  262,  275,  264,  265,  266,  275,  275,  275,  275,  271,
  272,  257,  258,  259,  260,  275,  262,  275,  264,  265,
  266,  263,  275,  275,  273,    0,  272,    0,  275,
};
#define YYFINAL 10
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 276
#if YYDEBUG
char *yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"AND_TKN","OR_TKN","NOT_TKN",
"GRP_TKN","CATS_TKN","EXPR_TKN","RANGE_TKN","NAM_TKN","OVR_TKN","COV_TKN",
"WIN_TKN","BYE_TKN","ERA_TKN","HST_TKN","HLP_TKN","NAM_STR","INUMBER","LP","RP",
"SEMI",
};
char *yyrule[] = {
"$accept : map_cmnd",
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
"$$1 :",
"map_expr : LP GRP_TKN $$1 grp_expr map_expr RP",
"map_expr : LP EXPR_TKN INUMBER RP",
"map_expr : LP NAM_STR RP",
"map_expr : NAM_STR",
"win_expr : WIN_TKN NAM_STR",
"win_expr : WIN_TKN",
"grp_expr :",
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
#line 275 "gis_pars.y"
/* ========================================================================= */
int yyerror(message) char *message ;
{
/*
	fprintf (stderr, "parser: %s, try again:\n", message);
*/
	return 0;
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
		*node->name = '\0' ;

	/* fprintf(stderr, "making node for %s\n", name) ; */

	return (node) ;
}
/* ========================================================================= */
#line 260 "y.tab.c"
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
#line 49 "gis_pars.y"
{
			yytree = yyvsp[0].nod_val ;
			return(MAP_EXPR) ;
		}
break;
case 2:
#line 54 "gis_pars.y"
{
			return(WIN_EXPR) ;
		}
break;
case 3:
#line 58 "gis_pars.y"
{
			return(CATS_EXPR) ;
		}
break;
case 4:
#line 62 "gis_pars.y"
{
			return(HELP_EXPR) ;
		}
break;
case 5:
#line 66 "gis_pars.y"
{
			return(HIST_EXPR) ;
		}
break;
case 6:
#line 70 "gis_pars.y"
{
			return(ERAS_EXPR) ;
		}
break;
case 7:
#line 74 "gis_pars.y"
{
			return(EXIT_EXPR) ;
		}
break;
case 8:
#line 78 "gis_pars.y"
{
			return(ERR_EXPR) ;
		}
break;
case 9:
#line 83 "gis_pars.y"
{
			yyval.nod_val = make_node(NAM_OPR,     yyvsp[-1].nod_val, NUL_EXP,      yyvsp[-2].str_val) ;
		}
break;
case 10:
#line 87 "gis_pars.y"
{
			yyval.nod_val = make_node(OVR_OPR+yyvsp[-2].int_val,     yyvsp[-1].nod_val, NUL_EXP,      yyvsp[-3].str_val) ;
		}
break;
case 11:
#line 91 "gis_pars.y"
{
			yyval.nod_val = make_node(OVR_OPR+yyvsp[-3].int_val,     yyvsp[-1].nod_val, NUL_EXP,      yyvsp[-2].str_val) ;
		}
break;
case 12:
#line 95 "gis_pars.y"
{
			yyval.nod_val = make_node(OVR_OPR+yyvsp[-2].int_val,     yyvsp[-1].nod_val, NUL_EXP,    NULL) ;
		}
break;
case 13:
#line 99 "gis_pars.y"
{
			yyval.nod_val = make_node(COV_OPR,     yyvsp[-1].nod_val, NUL_EXP,      yyvsp[-2].str_val) ;
		}
break;
case 14:
#line 103 "gis_pars.y"
{
			yyval.nod_val = make_node(AND_OPR,      yyvsp[-2].nod_val,      yyvsp[-1].nod_val,   "and") ;
		}
break;
case 15:
#line 107 "gis_pars.y"
{
			yyval.nod_val = make_node( OR_OPR,      yyvsp[-2].nod_val,      yyvsp[-1].nod_val,    "or") ;
		}
break;
case 16:
#line 111 "gis_pars.y"
{
			yyval.nod_val = make_node(GRP_OPR,      yyvsp[-1].nod_val, NUL_EXP, "group") ;
			init_group (&yyval.nod_val->group);
			mark_group (&yyval.nod_val->group, 0, 0);
		}
break;
case 17:
#line 117 "gis_pars.y"
{
			/* GRAB CONTROL EARLY */
			/* multiple group arrays necessary for nested expr */
			init_group(&e_yygroup[e_yygrp_i]);

			/* set index to next array for nested expression   */
			e_yygrp_i++ ;
		}
break;
case 18:
#line 126 "gis_pars.y"
{
			yyval.nod_val = make_node(GRP_OPR,      yyvsp[-1].nod_val, NUL_EXP, "group") ;

			/* set index back for this array */
			e_yygrp_i-- ;

			/* set the 'which group' table,min,max in the node itself */
			yyval.nod_val->group.max   = e_yygroup[e_yygrp_i].max;
			yyval.nod_val->group.min   = e_yygroup[e_yygrp_i].min;
			yyval.nod_val->group.table = e_yygroup[e_yygrp_i].table;
		}
break;
case 19:
#line 138 "gis_pars.y"
{
			yyval.nod_val = e_expr_list[yyvsp[-1].int_val - 1] ;
		}
break;
case 20:
#line 142 "gis_pars.y"
{
			yyval.nod_val = make_node(LEAF_OPR, NUL_EXP, NUL_EXP,      yyvsp[-1].str_val) ;
		}
break;
case 21:
#line 146 "gis_pars.y"
{
			yyval.nod_val = make_node(LEAF_OPR, NUL_EXP, NUL_EXP,      yyvsp[0].str_val) ;
		}
break;
case 22:
#line 152 "gis_pars.y"
{
			if(get_win(yyvsp[0].str_val) == 0)
				yyerror("window error (map name doesn't exist)");
		}
break;
case 23:
#line 157 "gis_pars.y"
{
			write_window( get_cur_win()) ;
		}
break;
case 24:
#line 163 "gis_pars.y"
{
		}
break;
case 25:
#line 166 "gis_pars.y"
{
			mark_group (&e_yygroup[e_yygrp_i-1], yyvsp[0].int_val, yyvsp[0].int_val);
		}
break;
case 26:
#line 170 "gis_pars.y"
{
			mark_group (&e_yygroup[e_yygrp_i-1], yyvsp[-2].int_val, yyvsp[0].int_val);
		}
break;
case 27:
#line 176 "gis_pars.y"
{
			if(get_cats(yyvsp[0].str_val) == 0)
				yyerror("categories error");
		}
break;
case 28:
#line 183 "gis_pars.y"
{
		}
break;
case 29:
#line 188 "gis_pars.y"
{
		}
break;
case 30:
#line 193 "gis_pars.y"
{
			G_gishelp("COMBINE",NULL) ;
		}
break;
case 31:
#line 197 "gis_pars.y"
{
			G_gishelp("COMBINE",NULL) ;
		}
break;
case 32:
#line 201 "gis_pars.y"
{
			G_gishelp("COMBINE",NULL) ;
		}
break;
case 33:
#line 205 "gis_pars.y"
{
			G_gishelp("COMBINE",yyvsp[0].str_val) ;
		}
break;
case 34:
#line 209 "gis_pars.y"
{
			G_gishelp("COMBINE","AND") ;
		}
break;
case 35:
#line 213 "gis_pars.y"
{
			G_gishelp("COMBINE","OR") ;
		}
break;
case 36:
#line 217 "gis_pars.y"
{
			G_gishelp("COMBINE","NOT") ;
		}
break;
case 37:
#line 221 "gis_pars.y"
{
			G_gishelp("COMBINE","GRP") ;
		}
break;
case 38:
#line 225 "gis_pars.y"
{
			G_gishelp("COMBINE","CATS") ;
		}
break;
case 39:
#line 229 "gis_pars.y"
{
			G_gishelp("COMBINE","EXPR") ;
		}
break;
case 40:
#line 233 "gis_pars.y"
{
			G_gishelp("COMBINE","RANGE") ;
		}
break;
case 41:
#line 237 "gis_pars.y"
{
			G_gishelp("COMBINE","NAM") ;
		}
break;
case 42:
#line 241 "gis_pars.y"
{
			G_gishelp("COMBINE","OVR") ;
		}
break;
case 43:
#line 245 "gis_pars.y"
{
			G_gishelp("COMBINE","COV") ;
		}
break;
case 44:
#line 249 "gis_pars.y"
{
			G_gishelp("COMBINE","WIN") ;
		}
break;
case 45:
#line 253 "gis_pars.y"
{
			G_gishelp("COMBINE","BYE") ;
		}
break;
case 46:
#line 257 "gis_pars.y"
{
			G_gishelp("COMBINE","ERA") ;
		}
break;
case 47:
#line 261 "gis_pars.y"
{
			G_gishelp("COMBINE","HST") ;
		}
break;
case 48:
#line 265 "gis_pars.y"
{
			G_gishelp("COMBINE","HLP") ;
		}
break;
case 49:
#line 271 "gis_pars.y"
{
		}
break;
#line 709 "y.tab.c"
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
