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

#include "xgrass.h"

XgMenuItemRec *curMenuItem;
XgMenuItemListRec *menuItemList, *curMenuItemList = NULL;
XgMenuBarListRec *menuBarList, *curMenuBarList = NULL;
char *curParentLabel = NULL;
Boolean parsingMenuBar = False;

#line 16 "grammar.y"
typedef union {
 char *cval;
} YYSTYPE;
#line 27 "y.tab.c"
#define MenuKey 257
#define DefaultMenuKey 258
#define F_Quit 259
#define F_Label 260
#define F_Separator 261
#define F_Pulldown 262
#define F_Pullright 263
#define F_HMenu 264
#define F_VMenu 265
#define F_Exec 266
#define F_ExecCapture 267
#define F_ExecHist 268
#define F_ExecCaptureHist 269
#define F_Xclip 270
#define F_DBSet 271
#define F_HistoryToggle 272
#define F_HistoryClear 273
#define F_HistoryEdit 274
#define F_HistoryReplay 275
#define String 276
#define YYERRCODE 256
short yylhs[] = {                                        -1,
    0,    5,    3,    4,    4,    7,    7,    9,    8,    6,
    6,   11,   12,   14,   10,    1,    1,    2,    2,   13,
   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
   13,   13,   13,   13,   13,   13,
};
short yylen[] = {                                         2,
    2,    0,    6,    0,    1,    2,    1,    0,    6,    2,
    1,    0,    0,    0,    7,    0,    2,    0,    1,    1,
    1,    1,    2,    2,    2,    2,    2,    2,    2,    2,
    2,    1,    1,    1,    1,    1,
};
short yydefred[] = {                                      0,
    0,    0,    0,    2,    0,    1,    0,    7,    0,    8,
    6,    0,    0,   12,    0,   11,    0,    0,    3,   10,
    0,    0,   13,    9,   17,    0,   19,   14,    0,   20,
   21,   22,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   32,   33,   34,   35,   36,   15,   23,   24,   25,
   26,   27,   28,   29,   30,   31,
};
short yydgoto[] = {                                       2,
   23,   28,    3,    6,    9,   15,    7,    8,   13,   16,
   18,   26,   47,   29,
};
short yysindex[] = {                                   -202,
 -201,    0, -199,    0, -220,    0, -199,    0,  -64,    0,
    0, -215,  -61,    0, -125,    0, -215,  -32,    0,    0,
 -124, -212,    0,    0,    0, -211,    0,    0, -239,    0,
    0,    0, -210, -209, -208, -207, -206, -205, -204, -203,
 -200,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,
};
short yyrindex[] = {                                      0,
    0,    0,   74,    0,    0,    0,   75,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0, -257,    0,    0,
    0,    0,    0,    0,    0, -222,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,
};
short yygindex[] = {                                      0,
    0,    0,    0,    0,    0,   60,    0,   71,    0,   39,
    0,    0,    0,    0,
};
#define YYTABLESIZE 152
short yytable[] = {                                      19,
   24,   16,   16,   16,   16,   16,   16,   16,   16,   16,
   16,   16,   16,   16,   16,   16,   16,   16,   16,   30,
   31,   32,   33,   34,   35,   36,   37,   38,   39,   40,
   41,   42,   43,   44,   45,   46,   18,   18,   18,   18,
   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,
   18,   18,   18,   20,    1,   10,    4,    5,   12,   20,
   14,   17,   22,   25,   27,   48,   49,   50,   51,   52,
   53,   54,   55,    4,    5,   56,   21,   11,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   14,   14,
};
short yycheck[] = {                                     125,
  125,  259,  260,  261,  262,  263,  264,  265,  266,  267,
  268,  269,  270,  271,  272,  273,  274,  275,  276,  259,
  260,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,  271,  272,  273,  274,  275,  259,  260,  261,  262,
  263,  264,  265,  266,  267,  268,  269,  270,  271,  272,
  273,  274,  275,   15,  257,  276,  258,  257,  123,   21,
  276,  123,   95,  276,  276,  276,  276,  276,  276,  276,
  276,  276,  276,    0,    0,  276,   17,    7,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  276,  276,
};
#define YYFINAL 2
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 276
#if YYDEBUG
char *yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'_'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,"'{'",0,"'}'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"MenuKey",
"DefaultMenuKey","F_Quit","F_Label","F_Separator","F_Pulldown","F_Pullright",
"F_HMenu","F_VMenu","F_Exec","F_ExecCapture","F_ExecHist","F_ExecCaptureHist",
"F_Xclip","F_DBSet","F_HistoryToggle","F_HistoryClear","F_HistoryEdit",
"F_HistoryReplay","String",
};
char *yyrule[] = {
"$accept : MenuDescription",
"MenuDescription : MenuBar OptMenuList",
"$$1 :",
"MenuBar : MenuKey DefaultMenuKey $$1 '{' MenuItemList '}'",
"OptMenuList :",
"OptMenuList : MenuList",
"MenuList : MenuList Menu",
"MenuList : Menu",
"$$2 :",
"Menu : MenuKey String $$2 '{' MenuItemList '}'",
"MenuItemList : MenuItemList MenuItem",
"MenuItemList : MenuItem",
"$$3 :",
"$$4 :",
"$$5 :",
"MenuItem : String $$3 OptMnemonic $$4 OptAccelerator $$5 FunctionDesc",
"OptMnemonic :",
"OptMnemonic : '_' String",
"OptAccelerator :",
"OptAccelerator : String",
"FunctionDesc : F_Quit",
"FunctionDesc : F_Label",
"FunctionDesc : F_Separator",
"FunctionDesc : F_Pulldown String",
"FunctionDesc : F_Pullright String",
"FunctionDesc : F_HMenu String",
"FunctionDesc : F_VMenu String",
"FunctionDesc : F_Exec String",
"FunctionDesc : F_ExecCapture String",
"FunctionDesc : F_ExecHist String",
"FunctionDesc : F_ExecCaptureHist String",
"FunctionDesc : F_Xclip String",
"FunctionDesc : F_DBSet",
"FunctionDesc : F_HistoryToggle",
"FunctionDesc : F_HistoryClear",
"FunctionDesc : F_HistoryEdit",
"FunctionDesc : F_HistoryReplay",
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
#line 54 "grammar.y"
{ 
	  LinkMenuSystem(menuBarList, menuItemList); 
	  _XG_Global.menuData.menuBarList = menuBarList;
      }
break;
case 2:
#line 62 "grammar.y"
{
          parsingMenuBar = True;
          curParentLabel = StrDup(yyvsp[0].cval);
      }
break;
case 3:
#line 67 "grammar.y"
{
          parsingMenuBar = False;
      }
break;
case 8:
#line 85 "grammar.y"
{
	  if ( curParentLabel != NULL ) XtFree(curParentLabel);
	  curParentLabel = StrDup(yyvsp[0].cval);
      }
break;
case 12:
#line 99 "grammar.y"
{
          if ( parsingMenuBar ) {
              if ( curMenuBarList == NULL ) {
                  menuBarList = curMenuBarList = (XgMenuBarListRec *)
                      XtMalloc(sizeof(XgMenuBarListRec));
              } else {
                  curMenuBarList->next = (XgMenuBarListRec *)
                      XtMalloc(sizeof(XgMenuBarListRec));
                  curMenuBarList = curMenuBarList->next;
              }
	      bzero((char *)curMenuBarList, sizeof(XgMenuBarListRec));
          } else {
              if ( curMenuItemList == NULL ) {
                  menuItemList = curMenuItemList = (XgMenuItemListRec *)
                      XtMalloc(sizeof(XgMenuItemListRec));
              } else {
                  curMenuItemList->next = (XgMenuItemListRec *)
                      XtMalloc(sizeof(XgMenuItemListRec));
                  curMenuItemList = curMenuItemList->next;
              }
	      bzero((char *)curMenuItemList, sizeof(XgMenuItemListRec));
          }
          curMenuItem = (XgMenuItemRec *)XtMalloc(sizeof(XgMenuItemRec));
          bzero((char *)curMenuItem, sizeof(XgMenuItemRec));
          curMenuItem->label = StrDup(yyvsp[0].cval);
      }
break;
case 13:
#line 126 "grammar.y"
{
          curMenuItem->mnemonic = StrDup(yyvsp[0].cval);
      }
break;
case 14:
#line 130 "grammar.y"
{
          curMenuItem->accelerator = StrDup(yyvsp[0].cval);
      }
break;
case 15:
#line 134 "grammar.y"
{
          if ( parsingMenuBar ) 
              curMenuBarList->menubar_item = curMenuItem;
          else
              curMenuItemList->menu_item = curMenuItem;
      }
break;
case 16:
#line 143 "grammar.y"
{ yyval.cval = NULL; }
break;
case 17:
#line 144 "grammar.y"
{ yyval.cval = yyvsp[0].cval; }
break;
case 18:
#line 148 "grammar.y"
{ yyval.cval = NULL; }
break;
case 19:
#line 149 "grammar.y"
{ yyval.cval = yyvsp[0].cval; }
break;
case 20:
#line 154 "grammar.y"
{
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgrassExit;
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
break;
case 21:
#line 160 "grammar.y"
{
          if ( parsingMenuBar ) {
              yyerror("label not allowed in main menu.");
          } else {
              curMenuItem->class = &xmLabelWidgetClass;
              curMenuItem->submenu_type = XG_MENU_NONE;
              curMenuItem->parent_label = StrDup(curParentLabel);
          }
      }
break;
case 22:
#line 170 "grammar.y"
{
          if ( parsingMenuBar ) {
              yyerror("separator not allowed in main menu.");
          } else {
              curMenuItem->class = &xmSeparatorWidgetClass;
              curMenuItem->submenu_type = XG_MENU_NONE;
              curMenuItem->parent_label = StrDup(curParentLabel);
          }
      }
break;
case 23:
#line 180 "grammar.y"
{
          if ( parsingMenuBar ) {
              curMenuItem->class = &xmCascadeButtonWidgetClass;
              curMenuItem->submenu_type = XG_MENU_PULLDOWN;
              curMenuItem->submenu_label = StrDup(yyvsp[0].cval);
              curMenuItem->parent_label = StrDup(curParentLabel);
          } else {
              yyerror("pulldown menu not allowed in sub menus.");
          }
      }
break;
case 24:
#line 191 "grammar.y"
{
          if ( parsingMenuBar ) {
              yyerror("pullright menu not allowed in main menu.");
          } else {
              curMenuItem->class = &xmCascadeButtonWidgetClass;
              curMenuItem->submenu_type = XG_MENU_PULLRIGHT;
              curMenuItem->submenu_label = StrDup(yyvsp[0].cval);
              curMenuItem->parent_label = StrDup(curParentLabel);
          }
      }
break;
case 25:
#line 202 "grammar.y"
{
          curMenuItem->class = &xmCascadeButtonWidgetClass;
          curMenuItem->submenu_type = XG_MENU_TEAR_OFF_H;
          curMenuItem->submenu_label = StrDup(yyvsp[0].cval);
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
break;
case 26:
#line 209 "grammar.y"
{
          curMenuItem->class = &xmCascadeButtonWidgetClass;
          curMenuItem->submenu_type = XG_MENU_TEAR_OFF_V;
          curMenuItem->submenu_label = StrDup(yyvsp[0].cval);
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
break;
case 27:
#line 216 "grammar.y"
{
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgExec;
          curMenuItem->arglist = StrDup(yyvsp[0].cval);
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
break;
case 28:
#line 223 "grammar.y"
{
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgExecCapture;
          curMenuItem->arglist = StrDup(yyvsp[0].cval);
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
break;
case 29:
#line 230 "grammar.y"
{
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgExecHist;
          curMenuItem->arglist = StrDup(yyvsp[0].cval);
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
break;
case 30:
#line 237 "grammar.y"
{
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgExecCaptureHist;
          curMenuItem->arglist = StrDup(yyvsp[0].cval);
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
break;
case 31:
#line 244 "grammar.y"
{
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgXclip;
          curMenuItem->arglist = StrDup(yyvsp[0].cval);
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
break;
case 32:
#line 251 "grammar.y"
{
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgDbSet;
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
break;
case 33:
#line 257 "grammar.y"
{
          curMenuItem->class = &xmToggleButtonWidgetClass;
          curMenuItem->callback = _XgHistoryToggle;
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
break;
case 34:
#line 263 "grammar.y"
{
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgHistoryClear;
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
break;
case 35:
#line 269 "grammar.y"
{
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgHistoryEdit;
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
break;
case 36:
#line 275 "grammar.y"
{
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgHistoryReplay;
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
break;
#line 605 "y.tab.c"
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
