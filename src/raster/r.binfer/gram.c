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
#line 13 "binfer.y"

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

#line 35 "binfer.y"
typedef union {
    char *y_sym;
} YYSTYPE;
#line 38 "y.tab.c"
#define Identifier 257
#define String 258
#define Constant 259
#define LAYER 260
#define CONTEXT 261
#define SUBJECTIVE 262
#define INFERRED 263
#define QUESTION 264
#define THRU 265
#define NO_COMBINED_MAP 266
#define NO_PROBABILITY_MAPS 267
#define COMBINED_MAP 268
#define ASPECT 269
#define GREY 270
#define HISTO 271
#define RAINBOW 272
#define RAMP 273
#define RANDOM 274
#define REDYELLOWGREEN 275
#define WAVE 276
#define YYERRCODE 256
short yylhs[] = {                                        -1,
   11,    0,   10,   12,   12,   14,   14,   14,   14,   14,
   14,   14,   14,   14,   14,   13,   13,   13,   20,   15,
   19,   19,   25,   22,   24,   24,   27,   27,   30,   26,
   29,   29,    6,    6,    6,    6,    5,    4,    4,    2,
   33,   17,   32,   32,   36,   34,   35,   38,   38,   39,
   37,   40,   18,   42,   16,   44,   41,   43,   43,   46,
   46,   45,   45,   50,   48,   49,   49,   49,   49,   49,
   49,   49,   49,   49,   51,    9,    9,    8,    8,    7,
    3,    3,    1,   28,   23,   47,   31,   21,
};
short yylen[] = {                                         2,
    0,    2,    2,    0,    2,    1,    1,    2,    2,    2,
    2,    2,    2,    2,    2,    2,    3,    4,    0,    5,
    2,    3,    0,    5,    0,    4,    2,    3,    0,    4,
    0,    3,    1,    2,    1,    2,    3,    2,    1,    1,
    0,    5,    2,    3,    0,    5,    4,    2,    3,    0,
    3,    0,    6,    0,    5,    0,    8,    0,    3,    1,
    3,    1,    3,    0,    9,    0,    1,    1,    1,    1,
    1,    1,    1,    1,    3,    2,    3,    1,    3,    1,
    0,    4,    1,    1,    1,    1,    1,    1,
};
short yydefred[] = {                                      1,
    0,    4,    2,    0,    0,    6,    7,    0,    3,    5,
    0,   19,    8,    9,   10,   11,   12,   13,   14,   15,
    0,    0,   16,    0,    0,   41,   54,    0,   17,    0,
   83,    0,    0,    0,    0,    0,   52,   18,   23,   88,
   20,    0,   85,   21,    0,    0,    0,    0,    0,    0,
    0,   22,   45,   42,    0,   43,   56,   55,    0,    0,
    0,    0,   44,    0,    0,   29,    0,    0,   24,    0,
    0,    0,    0,   53,    0,    0,    0,    0,   50,    0,
   46,   60,    0,    0,    0,    0,   27,   84,    0,   26,
    0,    0,    0,    0,   86,    0,   59,   64,    0,   62,
   40,    0,    0,    0,    0,   30,   28,   82,   51,   48,
    0,   47,   61,    0,    0,    0,    0,    0,   34,   39,
    0,   87,   32,   49,   67,   68,   69,   70,   71,   72,
   73,   74,    0,   63,   57,   37,   38,    0,    0,    0,
    0,   75,   80,   78,    0,    0,    0,    0,   79,    0,
    0,    0,   65,
};
short yydgoto[] = {                                       1,
   45,  102,   69,  103,  104,  105,  144,  145,  148,    3,
    2,    4,    9,   10,   11,   23,   24,   30,   33,   25,
   41,   34,   44,   61,   51,   67,   77,   90,   86,   75,
  123,   46,   35,   47,   71,   62,   80,   94,   92,   50,
   49,   36,   73,   64,   99,   83,   97,  100,  133,  114,
  139,
};
short yysindex[] = {                                      0,
    0,    0,    0, -214,  -27,    0,    0, -174,    0,    0,
 -240,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  -17,  -15,    0, -201, -199,    0,    0,    7,    0, -197,
    0,    9,  -37,   18, -199, -199,    0,    0,    0,    0,
    0,   18,    0,    0,   11,  -37,   18,   12,   34, -199,
   33,    0,    0,    0,   18,    0,    0,    0,   18, -199,
  -49,   36,    0,  -46,   34,    0,   35, -186,    0, -199,
  -49, -199,   40,    0,  -10, -199,    4, -176,    0,   42,
    0,    0,  -34, -199, -175,  -49,    0,    0, -199,    0,
  -33,  -49, -199,    6,    0, -199,    0,    0,   15,    0,
    0, -177, -175, -175,   -4,    0,    0,    0,    0,    0,
 -199,    0,    0, -166, -199,   18, -175, -177,    0,    0,
 -175,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   51,    0,    0,    0,    0, -147,    2,   52,
 -146,    0,    0,    0,  -26, -146, -146,   56,    0,   72,
  -92,   72,    0,
};
short yyrindex[] = {                                      0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  -40,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   71,    0,    0,   78,    0,    0,    0,    0,    0,    0,
   71,    0,    0,    0,  -36,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   16,    0,    0,    0,    0,
    0,   16,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  -91,   26,   27,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   61,    0,    0,    0,  -90,    0,    0,
   29,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   64,
    0,   65,    0,
};
short yygindex[] = {                                      0,
  -21,  -77,  -57,   21,   23,    0,  -19, -117,    0,    0,
    0,    0,    0,    0,    0,  -11,    0,    0,    0,    0,
  -29,   95,  -31,    0,    0,  -67,    0,  -62,    0,    0,
  -22,    0,    0,   -8,    0,    0,  -86,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   39,   17,    0,    0,
    0,
};
#define YYTABLESIZE 220
short yytable[] = {                                      40,
  122,   39,   38,   32,   31,   25,  110,   31,   87,   96,
   52,   32,   29,   81,   48,   56,   54,  146,   38,   58,
   21,  107,   22,   63,  124,  118,  120,   65,  106,  150,
   12,  112,  147,  152,  109,   74,  116,   55,   66,  136,
   26,   59,   27,  137,   88,    5,   88,   89,   79,  111,
   82,    6,    7,    8,   66,   88,   81,   31,  115,   81,
   28,   22,   98,   43,   37,   22,   39,   66,   53,   57,
   40,   79,   60,   68,  113,   70,   72,   78,   76,   84,
   85,   91,   25,  101,  135,   93,   31,  117,  122,   79,
   95,   95,  141,   98,   13,   14,   15,   16,   17,   18,
   19,   20,  125,  126,  127,  128,  129,  130,  131,  132,
  138,  140,  143,  142,  151,  146,   81,   58,   33,   35,
   66,   36,   76,   77,  121,  119,  149,   42,  153,  108,
    0,  134,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  143,   39,   38,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   31,
};
short yycheck[] = {                                      37,
   93,   93,   93,   25,   41,   46,   93,   44,   76,   44,
   42,   33,   24,   71,   36,   47,   46,   44,   30,   49,
  261,   89,  263,   55,  111,  103,  104,   59,   86,  147,
   58,   94,   59,  151,   92,   65,   99,   46,   60,  117,
   58,   50,   58,  121,   41,  260,   41,   44,   70,   44,
   72,  266,  267,  268,   76,   41,   41,  257,   44,   44,
  262,  263,   84,   46,   58,  263,   58,   89,   58,   58,
   37,   93,   40,  123,   96,   40,  123,  264,   44,   40,
   91,  258,  123,  259,  116,   44,  123,  265,   93,  111,
  125,  125,   91,  115,  269,  270,  271,  272,  273,  274,
  275,  276,  269,  270,  271,  272,  273,  274,  275,  276,
   60,  259,  259,   62,   59,   44,   46,   40,   93,   93,
   60,   93,   59,   59,  104,  103,  146,   33,  151,   91,
   -1,  115,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  259,  259,  259,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  257,
};
#define YYFINAL 1
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 276
#if YYDEBUG
char *yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,"'%'",0,0,"'('","')'",0,0,"','",0,"'.'",0,0,0,0,0,0,0,0,0,0,0,"':'","';'",
"'<'",0,"'>'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'['",0,
"']'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'{'",0,"'}'",0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,"Identifier","String","Constant","LAYER","CONTEXT",
"SUBJECTIVE","INFERRED","QUESTION","THRU","NO_COMBINED_MAP",
"NO_PROBABILITY_MAPS","COMBINED_MAP","ASPECT","GREY","HISTO","RAINBOW","RAMP",
"RANDOM","REDYELLOWGREEN","WAVE",
};
char *yyrule[] = {
"$accept : Program",
"$$1 :",
"Program : $$1 Script",
"Script : Output_Options MainScript",
"Output_Options :",
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
"$$2 :",
"Layer_Section : LAYER ':' $$2 Layer_Att_Declarations end",
"Layer_Att_Declarations : Layer_Att_Declaration ep",
"Layer_Att_Declarations : Layer_Att_Declarations Layer_Att_Declaration ep",
"$$3 :",
"Layer_Att_Declaration : Name ':' $$3 Layer_Value_List Question_Attachment",
"Layer_Value_List :",
"Layer_Value_List : '(' Layer_Value Layer_Value_Sublist rp",
"Layer_Value_Sublist : ',' Layer_Value",
"Layer_Value_Sublist : Layer_Value_Sublist ',' Layer_Value",
"$$4 :",
"Layer_Value : Name $$4 Category_Range Question_Attachment",
"Category_Range :",
"Category_Range : '[' Reclass_Rule rbr",
"Reclass_Rule : Input_Cat_List",
"Reclass_Rule : Input_Cat_List Input_Cat_Range",
"Reclass_Rule : Input_Cat_Range",
"Reclass_Rule : Input_Cat_Range Input_Cat_List",
"Input_Cat_Range : Input_Cat THRU Input_Cat",
"Input_Cat_List : Input_Cat_List Input_Cat",
"Input_Cat_List : Input_Cat",
"Input_Cat : Constant",
"$$5 :",
"Context_Section : CONTEXT ':' $$5 Context_Att_Declarations end",
"Context_Att_Declarations : Context_Att_Declaration ep",
"Context_Att_Declarations : Context_Att_Declarations Context_Att_Declaration ep",
"$$6 :",
"Context_Att_Declaration : Name ':' $$6 Context_Value_List Question_Attachment",
"Context_Value_List : '(' Context_Value Context_Value_Sublist rp",
"Context_Value_Sublist : ',' Context_Value",
"Context_Value_Sublist : Context_Value_Sublist ',' Context_Value",
"$$7 :",
"Context_Value : Name $$7 Question_Attachment",
"$$8 :",
"Subjective_Section : SUBJECTIVE ':' $$8 Context_Att_Declaration ep end",
"$$9 :",
"Inferred_Section : INFERRED ':' $$9 Inferred_Att_Declaration end",
"$$10 :",
"Inferred_Att_Declaration : Name ':' $$10 Determinant_List '(' Inferred_Value_List rp ep",
"Determinant_List :",
"Determinant_List : '{' Att_List rb",
"Att_List : Name",
"Att_List : Att_List ',' Name",
"Inferred_Value_List : Inferred_Value",
"Inferred_Value_List : Inferred_Value_List ',' Inferred_Value",
"$$11 :",
"Inferred_Value : Name $$11 Optional_Color_Table Prior_Probability '[' Probability_List Conditional_Probability_Table ';' rbr",
"Optional_Color_Table :",
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
"Question_Attachment :",
"Question_Attachment : '{' QUESTION String rb",
"Name : Identifier",
"rp : ')'",
"ep : '.'",
"rb : '}'",
"rbr : ']'",
"end : '%'",
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
#line 511 "binfer.y"


#include <stdio.h>
#line 338 "y.tab.c"
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
#line 80 "binfer.y"
{ init(); }
break;
case 2:
#line 81 "binfer.y"
{ 
                 check_table(&yyparse_return,verbose);
                 if ( verbose ) fprintf(stderr,"\nAll input parsed.\n");
                 return(-1);
             }
break;
case 6:
#line 95 "binfer.y"
{ 
                          combinedmap = 0;
                          fprintf(stderr,"NoCombinedMap option set.\n"); 
                      }
break;
case 7:
#line 101 "binfer.y"
{ 
                          probabilitymaps = 0;
                          fprintf(stderr,"NoProbabiltyMaps option set.\n"); 
                      }
break;
case 8:
#line 107 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = AspectColors;
                              fprintf(stderr,"Combined map colortable set to aspect colors.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      }
break;
case 9:
#line 117 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = GreyScale;
                              fprintf(stderr,"Combined map colortable set to grey scale.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      }
break;
case 10:
#line 127 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = HistoGreyScale;
                              fprintf(stderr,"Combined map colortable set to histogram stretched grey scale.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      }
break;
case 11:
#line 137 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = Rainbow;
                              fprintf(stderr,"Combined map colortable set to rainbow colors.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      }
break;
case 12:
#line 147 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = Ramp;
                              fprintf(stderr,"Combined map colortable set to color ramp.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      }
break;
case 13:
#line 157 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = Random;
                              fprintf(stderr,"Combined map colortable set to random colors.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      }
break;
case 14:
#line 167 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = RYG;
                              fprintf(stderr,"Combined map colortable set to red yellow green.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      }
break;
case 15:
#line 177 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = Wave;
                              fprintf(stderr,"Combined map colortable set to color wave.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      }
break;
case 19:
#line 194 "binfer.y"
{ 
                       expected_type = AttributeSymbol;
                       value_type = LayerAttribute; 
                   }
break;
case 20:
#line 199 "binfer.y"
{ 
                       if ( verbose ) fprintf(stderr,"\nParsed layers section.\n"); 
                   }
break;
case 23:
#line 208 "binfer.y"
{ 
                   cur_sym = s_create(yyvsp[-1].y_sym,expected_type,value_type);
                   expected_type = ValueSymbol; 
               }
break;
case 24:
#line 214 "binfer.y"
{ 
                   expected_type = AttributeSymbol;
                   if ( yyvsp[0].y_sym != (char *)0 )
                       cur_att->question = strsave(yyvsp[0].y_sym); 
                   valno = 1;
               }
break;
case 29:
#line 230 "binfer.y"
{ 
               cur_sym = s_create(yyvsp[0].y_sym,expected_type,value_type); 
           }
break;
case 30:
#line 234 "binfer.y"
{ 
               if ( yyvsp[0].y_sym != (char *)0 )
                   cur_sym->question = strsave(yyvsp[0].y_sym); 
           }
break;
case 32:
#line 243 "binfer.y"
{ 
               cur_sym->element.val->desc.layer->cat_num = (CELL)valno;
            
               sprintf(cur_sym->reclass,"%s = %d %s",strsave(yyvsp[-1].y_sym),valno++,
                       cur_sym->name);
           }
break;
case 33:
#line 251 "binfer.y"
{ yyval.y_sym = strsave(yyvsp[0].y_sym); }
break;
case 34:
#line 252 "binfer.y"
{ sprintf(reclassbuf,"%s %s",yyvsp[-1].y_sym,yyvsp[0].y_sym);
                                       yyval.y_sym = strsave(reclassbuf); }
break;
case 35:
#line 254 "binfer.y"
{ yyval.y_sym = strsave(yyvsp[0].y_sym); }
break;
case 36:
#line 255 "binfer.y"
{ sprintf(reclassbuf,"%s %s",yyvsp[-1].y_sym,yyvsp[0].y_sym);
                                       yyval.y_sym = strsave(reclassbuf); }
break;
case 37:
#line 260 "binfer.y"
{ sprintf(reclassbuf,"%s thru %s",yyvsp[-2].y_sym,yyvsp[0].y_sym);
                                 yyval.y_sym = strsave(reclassbuf); }
break;
case 38:
#line 264 "binfer.y"
{ sprintf(reclassbuf,"%s %s",yyvsp[-1].y_sym,yyvsp[0].y_sym);
                                 yyval.y_sym = strsave(reclassbuf); }
break;
case 39:
#line 266 "binfer.y"
{ yyval.y_sym = strsave(yyvsp[0].y_sym); }
break;
case 40:
#line 269 "binfer.y"
{ yyval.y_sym = strsave(yyvsp[0].y_sym);}
break;
case 41:
#line 273 "binfer.y"
{ 
                      expected_type = AttributeSymbol;
                      value_type = ContextAttribute; 
                  }
break;
case 42:
#line 278 "binfer.y"
{ 
               if ( verbose ) fprintf(stderr,"\nParsed context section.\n"); 
           }
break;
case 45:
#line 287 "binfer.y"
{ 
                    cur_att = cur_sym = s_create(yyvsp[-1].y_sym,expected_type,value_type);
                    expected_type = ValueSymbol; 
               }
break;
case 46:
#line 292 "binfer.y"
{ 
                   expected_type = AttributeSymbol;
                   if ( yyvsp[0].y_sym != (char *)0 )
                       cur_att->question = strsave(yyvsp[0].y_sym); 
               }
break;
case 50:
#line 307 "binfer.y"
{ 
               cur_sym = s_create(yyvsp[0].y_sym,expected_type,value_type); 
           }
break;
case 51:
#line 311 "binfer.y"
{ 
               if ( yyvsp[0].y_sym != (char *)0 )
                   cur_sym->question = strsave(yyvsp[0].y_sym); 
           }
break;
case 52:
#line 318 "binfer.y"
{ 
                         expected_type = AttributeSymbol;
                         value_type = SubjectiveAttribute; 
                     }
break;
case 53:
#line 323 "binfer.y"
{ 
                         if ( verbose ) 
                             fprintf(stderr,"\nParsed subjective section.\n"); 
                     }
break;
case 54:
#line 329 "binfer.y"
{ 
                        expected_type = AttributeSymbol;
                        value_type = InferredAttribute; 
                    }
break;
case 55:
#line 334 "binfer.y"
{ 
                        if ( verbose ) 
                            fprintf(stderr,"\nParsed inferred section.\n"); 
                    }
break;
case 56:
#line 340 "binfer.y"
{ 
                   cur_sym = s_create(yyvsp[-1].y_sym,expected_type,value_type);
                   expected_type = ValueSymbol; 
               }
break;
case 57:
#line 345 "binfer.y"
{ 
                   expected_type = AttributeSymbol; 
               }
break;
case 60:
#line 355 "binfer.y"
{ 
               if (!add_name(yyvsp[0].y_sym)) yyerror("Name not stored"); 
           }
break;
case 61:
#line 359 "binfer.y"
{ 
               if (!add_name(yyvsp[0].y_sym)) yyerror("Name not stored"); 
           }
break;
case 64:
#line 369 "binfer.y"
{ 
               cur_sym = s_create(yyvsp[0].y_sym,expected_type,value_type); 
           }
break;
case 65:
#line 374 "binfer.y"
{
                sprintf(probbuf,"%s%s;",yyvsp[-3].y_sym,yyvsp[-2].y_sym);
                if (!add_prob_list(probbuf)) yyerror("Problist not stored");
            }
break;
case 67:
#line 382 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = AspectColors;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           }
break;
case 68:
#line 392 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = GreyScale;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           }
break;
case 69:
#line 402 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = HistoGreyScale;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           }
break;
case 70:
#line 412 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = Rainbow;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           }
break;
case 71:
#line 422 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = Ramp;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           }
break;
case 72:
#line 432 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = Random;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           }
break;
case 73:
#line 442 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = RYG;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           }
break;
case 74:
#line 452 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = Wave;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           }
break;
case 75:
#line 465 "binfer.y"
{ 
                cur_sym->element.val->desc.infr->prior_prob = atof(yyvsp[-1].y_sym); 
            }
break;
case 76:
#line 472 "binfer.y"
{ 
                sprintf(probbuf,";%s",yyvsp[0].y_sym); 
                yyval.y_sym = strsave(probbuf);
            }
break;
case 77:
#line 477 "binfer.y"
{ 
                sprintf(probbuf,"%s;%s",yyvsp[-2].y_sym,yyvsp[0].y_sym);
                yyval.y_sym = strsave(probbuf);
            }
break;
case 78:
#line 484 "binfer.y"
{ 
                yyval.y_sym = strsave(yyvsp[0].y_sym); 
            }
break;
case 79:
#line 488 "binfer.y"
{ 
                sprintf(probbuf,"%s,%s",yyvsp[-2].y_sym,yyvsp[0].y_sym);
                yyval.y_sym = strsave(probbuf);
            }
break;
case 80:
#line 494 "binfer.y"
{ yyval.y_sym = strsave(yyvsp[0].y_sym); }
break;
case 81:
#line 497 "binfer.y"
{ yyval.y_sym = (char *)0; }
break;
case 82:
#line 498 "binfer.y"
{ yyval.y_sym = strsave(yyvsp[-1].y_sym); }
break;
case 84:
#line 504 "binfer.y"
{ yyerrok; }
break;
case 85:
#line 505 "binfer.y"
{ yyerrok; }
break;
case 86:
#line 506 "binfer.y"
{ yyerrok; }
break;
case 87:
#line 507 "binfer.y"
{ yyerrok; }
break;
case 88:
#line 508 "binfer.y"
{ yyerrok; }
break;
#line 964 "y.tab.c"
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
