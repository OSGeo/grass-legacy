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
#line 29 "grammar.y"

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

#line 77 "grammar.y"
typedef union {
    char *  cval;
    int ival;
    double dval;
    Boolean bval;
} YYSTYPE;
#line 65 "y.tab.c"
#define String 257
#define Integer 258
#define Real 259
#define Logical 260
#define Environment 261
#define InitialShells 262
#define Menu 263
#define CommandBoard 264
#define Label 265
#define Message 266
#define List 267
#define PushButton 268
#define TextEntry 269
#define Table 270
#define Separator 271
#define Slider 272
#define Toggle 273
#define PullDown 274
#define MultiLine 275
#define Font 276
#define FixedFont 277
#define EditorFont 278
#define Background 279
#define Foreground 280
#define BackgroundPixmap 281
#define TopShadowColor 282
#define TopShadowPixmap 283
#define BottomShadowColor 284
#define BottomShadowPixmap 285
#define X 286
#define DX 287
#define Y 288
#define DY 289
#define Width 290
#define Height 291
#define ForceSize 292
#define MaxWidth 293
#define MaxHeight 294
#define Columns 295
#define Override 296
#define Functions 297
#define Decorations 298
#define Popup 299
#define Popdown 300
#define Destroy 301
#define Exit 302
#define Help 303
#define Eval 304
#define UpdateObject 305
#define PostNotice 306
#define RunForeground 307
#define RunBackground 308
#define CommandShell 309
#define InteractiveShell 310
#define InputFrom 311
#define CaptureOutput 312
#define NotifyComplete 313
#define UpdateFrom 314
#define Pane 315
#define PaneType 316
#define Store 317
#define Highlight 318
#define GetEnv 319
#define Clear 320
#define CommandArg 321
#define TableArg 322
#define Set 323
#define Alignment 324
#define ListElement 325
#define ListSeparator 326
#define ListType 327
#define VisibleItems 328
#define ScrollBar 329
#define ValueString 330
#define Scrolled 331
#define LabelPixmap 332
#define MaxLength 333
#define Minimum 334
#define Maximum 335
#define StartValue 336
#define SliderWidth 337
#define SliderHeight 338
#define Orientation 339
#define DecimalPoints 340
#define EntryFont 341
#define Rows 342
#define RowsDisplayed 343
#define FixedRows 344
#define FixedColumns 345
#define ColumnsDisplayed 346
#define ColumnHeadings 347
#define RowHeadings 348
#define RowValue 349
#define TableValue 350
#define RowHeight 351
#define ColumnWidth 352
#define Newline 353
#define TitleString 354
#define ToggleType 355
#define ToggleState 356
#define SeparatorType 357
#define Sensitive 358
#define Insensitive 359
#define YYERRCODE 256
short yylhs[] = {                                        -1,
    8,    0,    9,    9,   12,   10,   13,   13,   15,   14,
    4,    4,   16,   16,   16,   16,   19,   17,   20,   21,
   18,    5,    5,    5,    5,    5,    5,    5,    5,    5,
    5,   11,   11,   22,   22,    1,   23,   23,    3,    6,
    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
    6,    6,    6,    6,    7,    7,   24,   24,   25,   25,
   25,   25,   25,   25,   25,   25,   25,   25,    2,    2,
    2,    2,
};
short yylen[] = {                                         2,
    0,    3,    2,    1,    0,    7,    2,    1,    0,    7,
    1,    1,    2,    2,    1,    1,    0,    5,    0,    0,
    9,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    0,    1,    2,    1,    1,    3,    3,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    0,    1,    2,    1,    3,    3,
    3,    3,    3,    3,    3,    3,    3,    3,    1,    1,
    1,    1,
};
short yydefred[] = {                                      1,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  128,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    4,  127,  129,
  130,  131,  132,  133,  134,  135,  136,  137,  138,   36,
    5,    3,    0,   40,  117,   41,   42,   43,   44,   45,
   46,   47,   48,   49,   50,   51,   52,   53,   54,   55,
   56,   57,   58,   59,   60,   61,   62,   63,   64,   65,
   66,   67,   68,   69,  122,   70,   71,   72,   73,   74,
   75,   76,   77,   78,   80,   79,   81,   82,   83,   84,
   85,   86,   39,   87,   88,   89,   90,   91,   92,   93,
   94,   95,   96,   97,   98,   99,  100,  101,  102,  103,
  104,  105,  106,  107,  108,  109,  110,  111,  112,  113,
  114,  115,  116,  118,  119,  120,  121,  123,  124,    0,
    0,    0,    0,   35,    0,    0,    0,   34,  139,  140,
  141,  142,   38,   37,   11,   12,    0,    0,    8,    9,
    6,    7,    0,    0,    0,   22,   23,   24,   25,   26,
   28,   29,   30,   31,   19,   27,    0,    0,   15,   16,
    0,   17,   10,   13,   14,   20,    0,    0,    0,    0,
   18,    0,    0,    0,   21,
};
short yydgoto[] = {                                       1,
   41,  143,  130,  147,  167,  131,   13,    2,   27,   28,
  132,   43,  148,  149,  153,  168,  169,  170,  177,  171,
  178,  133,  134,   14,   15,
};
short yysindex[] = {                                      0,
    0, -147,  -56,  -48,  -45,  -44,  -42,   47,   64,   65,
   67,   83, -246, -147,    0, -138, -115, -114, -113, -112,
 -111, -110, -109, -108, -107, -106, -246,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0, -259,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   94,
   95,   31, -259,    0, -253, -253, -255,    0,    0,    0,
    0,    0,    0,    0,    0,    0, -106, -124,    0,    0,
    0,    0, -259,  115, -158,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0, -106,  -41,    0,    0,
 -106,    0,    0,    0,    0,    0, -259, -259,   97,  117,
    0, -158,  -30,   99,    0,
};
short yyrindex[] = {                                      0,
    0, -102,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0, -101,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  161,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   39,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   61,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  123,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  105,  123,    0,    0,
    0,    0,    0,    0,    0,
};
short yygindex[] = {                                      0,
  -43,   29,    0,    0,    0,    0,    0,    0,    0,  139,
  -51,    0,    0,   19,    0,  -14,  -65,  -62,    0,    0,
    0,    0,   36,    0,  156,
};
#define YYTABLESIZE 245
short yytable[] = {                                     173,
  151,   16,   44,  139,  140,  141,  142,  145,  146,   17,
  184,   45,   18,   19,   26,   20,   46,   47,   48,   49,
   50,   51,   52,   53,   54,   55,   56,   57,   58,   59,
   60,   61,   62,   63,   64,   65,   66,   67,   68,   69,
   70,   71,   72,   73,   74,   75,   76,   77,   78,   79,
   80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
   90,   91,   92,   93,   94,   95,   96,   97,   98,   99,
  100,  101,  102,  103,  104,  105,  106,  107,  108,  109,
  110,  111,  112,  113,  114,  115,  116,  117,  118,  119,
  120,  121,  122,  123,  124,  125,  126,  127,  128,  129,
   33,  154,  174,  150,   21,  175,  156,  157,  158,  159,
  160,  161,  162,  163,  164,  165,  166,  174,   30,   33,
  175,   22,   23,  172,   24,  179,  180,  176,    3,    4,
    5,    6,    7,    8,    9,   10,   11,   12,  145,  146,
   25,   31,   32,   33,   34,   35,   36,   37,   38,   39,
   40,  135,  136,  137,  155,  181,  182,  185,  125,  126,
    2,   32,   32,   32,  144,   42,  152,  183,  138,   29,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   33,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  156,  157,  158,  159,  160,  161,  162,
  163,  164,  165,  166,  156,  157,  158,  159,  160,  161,
  162,  163,  164,  165,  166,
};
short yycheck[] = {                                      41,
  125,   58,  262,  257,  258,  259,  260,  263,  264,   58,
   41,  271,   58,   58,  261,   58,  276,  277,  278,  279,
  280,  281,  282,  283,  284,  285,  286,  287,  288,  289,
  290,  291,  292,  293,  294,  295,  296,  297,  298,  299,
  300,  301,  302,  303,  304,  305,  306,  307,  308,  309,
  310,  311,  312,  313,  314,  315,  316,  317,  318,  319,
  320,  321,  322,  323,  324,  325,  326,  327,  328,  329,
  330,  331,  332,  333,  334,  335,  336,  337,  338,  339,
  340,  341,  342,  343,  344,  345,  346,  347,  348,  349,
  350,  351,  352,  353,  354,  355,  356,  357,  358,  359,
   40,  153,  168,  147,   58,  168,  265,  266,  267,  268,
  269,  270,  271,  272,  273,  274,  275,  183,  257,   59,
  183,   58,   58,  167,   58,  177,  178,  171,  276,  277,
  278,  279,  280,  281,  282,  283,  284,  285,  263,  264,
   58,  257,  257,  257,  257,  257,  257,  257,  257,  257,
  257,   58,   58,  123,   40,   59,   40,   59,  261,  261,
    0,  123,   40,   59,  136,   27,  148,  182,  133,   14,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  123,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  265,  266,  267,  268,  269,  270,  271,
  272,  273,  274,  275,  265,  266,  267,  268,  269,  270,
  271,  272,  273,  274,  275,
};
#define YYFINAL 1
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 359
#if YYDEBUG
char *yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,"'('","')'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"':'","';'",0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'{'",0,"'}'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"String",
"Integer","Real","Logical","Environment","InitialShells","Menu","CommandBoard",
"Label","Message","List","PushButton","TextEntry","Table","Separator","Slider",
"Toggle","PullDown","MultiLine","Font","FixedFont","EditorFont","Background",
"Foreground","BackgroundPixmap","TopShadowColor","TopShadowPixmap",
"BottomShadowColor","BottomShadowPixmap","X","DX","Y","DY","Width","Height",
"ForceSize","MaxWidth","MaxHeight","Columns","Override","Functions",
"Decorations","Popup","Popdown","Destroy","Exit","Help","Eval","UpdateObject",
"PostNotice","RunForeground","RunBackground","CommandShell","InteractiveShell",
"InputFrom","CaptureOutput","NotifyComplete","UpdateFrom","Pane","PaneType",
"Store","Highlight","GetEnv","Clear","CommandArg","TableArg","Set","Alignment",
"ListElement","ListSeparator","ListType","VisibleItems","ScrollBar",
"ValueString","Scrolled","LabelPixmap","MaxLength","Minimum","Maximum",
"StartValue","SliderWidth","SliderHeight","Orientation","DecimalPoints",
"EntryFont","Rows","RowsDisplayed","FixedRows","FixedColumns",
"ColumnsDisplayed","ColumnHeadings","RowHeadings","RowValue","TableValue",
"RowHeight","ColumnWidth","Newline","TitleString","ToggleType","ToggleState",
"SeparatorType","Sensitive","Insensitive",
};
char *yyrule[] = {
"$accept : Program",
"$$1 :",
"Program : $$1 OptGlobals EnvironList",
"EnvironList : EnvironList Environ",
"EnvironList : Environ",
"$$2 :",
"Environ : Environment Name $$2 OptResourceList '{' ShellList '}'",
"ShellList : ShellList Shell",
"ShellList : Shell",
"$$3 :",
"Shell : ShellType Name $$3 OptResourceList '(' ObjectList ')'",
"ShellType : Menu",
"ShellType : CommandBoard",
"ObjectList : ObjectList Object",
"ObjectList : ObjectList PulldownObject",
"ObjectList : Object",
"ObjectList : PulldownObject",
"$$4 :",
"Object : ObjectType Name $$4 OptResourceList ';'",
"$$5 :",
"$$6 :",
"PulldownObject : PullDown $$5 Name $$6 OptResourceList '(' ObjectList ')' ';'",
"ObjectType : Label",
"ObjectType : Message",
"ObjectType : List",
"ObjectType : PushButton",
"ObjectType : TextEntry",
"ObjectType : MultiLine",
"ObjectType : Table",
"ObjectType : Separator",
"ObjectType : Slider",
"ObjectType : Toggle",
"OptResourceList :",
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
"ResourceType : EditorFont",
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
"ResourceType : ForceSize",
"ResourceType : MaxWidth",
"ResourceType : MaxHeight",
"ResourceType : Columns",
"ResourceType : Override",
"ResourceType : Functions",
"ResourceType : Decorations",
"ResourceType : Popup",
"ResourceType : Popdown",
"ResourceType : Destroy",
"ResourceType : Exit",
"ResourceType : Help",
"ResourceType : Eval",
"ResourceType : PostNotice",
"ResourceType : RunForeground",
"ResourceType : RunBackground",
"ResourceType : CommandShell",
"ResourceType : InteractiveShell",
"ResourceType : InputFrom",
"ResourceType : CaptureOutput",
"ResourceType : NotifyComplete",
"ResourceType : UpdateFrom",
"ResourceType : PaneType",
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
"ResourceType : Scrolled",
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
"ResourceType : FixedRows",
"ResourceType : FixedColumns",
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
"ResourceType : ToggleState",
"ResourceType : SeparatorType",
"ResourceType : UpdateObject",
"ResourceType : Sensitive",
"ResourceType : Insensitive",
"OptGlobals :",
"OptGlobals : Globals",
"Globals : Globals GlobalElement",
"Globals : GlobalElement",
"GlobalElement : Font ':' String",
"GlobalElement : FixedFont ':' String",
"GlobalElement : EditorFont ':' String",
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
#line 210 "grammar.y"
{ 
			xgenGD.screen = DefaultScreen(xgenGD.display);
			xgenGD.scrptr = XtScreen(xgenGD.applShell);
			xgenGD.cmap = DefaultColormap(xgenGD.display,xgenGD.screen);
		}
break;
case 2:
#line 217 "grammar.y"
{
			return(fatalError);
		}
break;
case 5:
#line 230 "grammar.y"
{
			if ( !UniqueEnvName(yyvsp[0].cval) ) {
				char errorbuf[80];

				sprintf(errorbuf,"Non-unique environment name \"%s\"",yyvsp[0].cval);
				yyerror(errorbuf,False);
			}
			curEnv = AllocEnviron(); 
			curEnv->name = yyvsp[0].cval;
			AddEnviron(curEnv);
			tailEnv = curEnv;
			resourceType = ENVIRONMENT;
			itemType = ENV;
		}
break;
case 9:
#line 258 "grammar.y"
{
			if ( !UniqueShellName(yyvsp[0].cval) ) {
				char errorbuf[80];

				sprintf(errorbuf,"Non-unique shell name \"%s\"",yyvsp[0].cval);
				yyerror(errorbuf,False);
			}
			curShell = AllocShell(); 
			curShell->type = yyvsp[-1].ival;
			curShell->name = yyvsp[0].cval;
			AddShell(curShell,tailEnv);
			tailShell = curShell;
			resourceType = SHELL;
			itemType = SHL;
		}
break;
case 11:
#line 278 "grammar.y"
{ yyval.ival = MENU; shellType = MU; }
break;
case 12:
#line 280 "grammar.y"
{ yyval.ival = COMMANDBOARD; shellType = CO; }
break;
case 17:
#line 293 "grammar.y"
{
			curObject = AllocObject(); 
			curObject->type = yyvsp[-1].ival;
			curObject->name = SaveString(yyvsp[0].cval);
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
		}
break;
case 19:
#line 324 "grammar.y"
{
			objectType = PU;
		}
break;
case 20:
#line 328 "grammar.y"
{
			doingPulldown = True;
			curObject = AllocObject(); 
			curObject->type = PULLDOWN;
			curObject->name = SaveString(yyvsp[0].cval);
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
		}
break;
case 21:
#line 348 "grammar.y"
{
			  doingPulldown = False;
		}
break;
case 22:
#line 355 "grammar.y"
{ yyval.ival = LABEL; objectType = LA; }
break;
case 23:
#line 357 "grammar.y"
{ yyval.ival = MESSAGE; objectType = ME; }
break;
case 24:
#line 359 "grammar.y"
{ yyval.ival = LIST; objectType = LI; }
break;
case 25:
#line 361 "grammar.y"
{ yyval.ival = PUSHBUTTON; objectType = PB; }
break;
case 26:
#line 363 "grammar.y"
{ yyval.ival = TEXTENTRY; objectType = TE; }
break;
case 27:
#line 365 "grammar.y"
{ yyval.ival = MULTILINE; objectType = ML; }
break;
case 28:
#line 367 "grammar.y"
{ yyval.ival = TABLE; objectType = TA; }
break;
case 29:
#line 369 "grammar.y"
{ yyval.ival = SEPARATOR; objectType = SE; }
break;
case 30:
#line 371 "grammar.y"
{ yyval.ival = SLIDER; objectType = SL; }
break;
case 31:
#line 373 "grammar.y"
{ yyval.ival = TOGGLE; objectType = TO; }
break;
case 37:
#line 392 "grammar.y"
{
			curRes = AllocResource(); 
			if ( rindex(yyvsp[0].cval,'$')) 
				curRes->variable = True;
			if ( !curRes->variable) {
			    switch(ResourceDataType(yyvsp[-2].ival)) {
				    case INTEGER:
					    if (!CheckType(yyvsp[0].cval,Int)) yyerror(errorbuf);
					    break;
				    case REAL:
					    if (!CheckType(yyvsp[0].cval,Real)) yyerror(errorbuf);
					    break;
				    case BOOLEAN:
					    if (!CheckType(yyvsp[0].cval,OnOff)) yyerror(errorbuf);
					    break;
				}
			}
			switch(resourceType) {
				case ENVIRONMENT:
					if ( !(ResourceValid(yyvsp[-2].ival) & itemType) ) {
						char errorbuf[80];

						sprintf(errorbuf,"invalid environment resource: %s",
							ResourceString(yyvsp[-2].ival));
						XgenFatalError("parser",errorbuf);
					}
					AddResource(curRes,(char *)curEnv,resourceType,yyvsp[0].cval,yyvsp[-2].ival);
					break;
				case SHELL:
					if ( !(ResourceValid(yyvsp[-2].ival) & shellType) ) {
						char errorbuf[80];

						sprintf(errorbuf,"invalid shell resource: %s",
							ResourceString(yyvsp[-2].ival));
						XgenFatalError("parser",errorbuf);
					}
					AddResource(curRes,(char *)curShell,resourceType,yyvsp[0].cval,yyvsp[-2].ival);
					break;
				case OBJECT:
					if ( !(ResourceValid(yyvsp[-2].ival) & objectType) ) {
						char errorbuf[80];

						sprintf(errorbuf,"invalid object resource: %s",
							ResourceString(yyvsp[-2].ival));
						XgenFatalError("parser",errorbuf);
					}
					AddResource(curRes,(char *)curObject,resourceType,yyvsp[0].cval,yyvsp[-2].ival);
					break;
			}
			tailRes = curRes;
		}
break;
case 38:
#line 444 "grammar.y"
{
			char buf[80];

			/*
			 * go ahead and set the resource to x=y. Will check later 
			 * for modifiers that need to wait til runtime to get a value.
			 */
			sprintf(buf,"%s=%s",yyvsp[-2].cval,yyvsp[0].cval);
			curRes = AllocResource(); 
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
		}
break;
case 39:
#line 470 "grammar.y"
{ yyval.cval = yyvsp[0].cval; }
break;
case 40:
#line 474 "grammar.y"
{ yyval.ival = InitialShells; }
break;
case 41:
#line 476 "grammar.y"
{ yyval.ival =  Font ; }
break;
case 42:
#line 478 "grammar.y"
{ yyval.ival =  FixedFont ; }
break;
case 43:
#line 480 "grammar.y"
{ yyval.ival =  EditorFont ; }
break;
case 44:
#line 482 "grammar.y"
{ yyval.ival =  Background ; }
break;
case 45:
#line 484 "grammar.y"
{ yyval.ival =  Foreground ; }
break;
case 46:
#line 486 "grammar.y"
{ yyval.ival =  BackgroundPixmap ; }
break;
case 47:
#line 488 "grammar.y"
{ yyval.ival =  TopShadowColor ; }
break;
case 48:
#line 490 "grammar.y"
{ yyval.ival =  TopShadowPixmap ; }
break;
case 49:
#line 492 "grammar.y"
{ yyval.ival =  BottomShadowColor ; }
break;
case 50:
#line 494 "grammar.y"
{ yyval.ival =  BottomShadowPixmap ; }
break;
case 51:
#line 496 "grammar.y"
{ yyval.ival =  X ; }
break;
case 52:
#line 498 "grammar.y"
{ yyval.ival =  DX ; }
break;
case 53:
#line 500 "grammar.y"
{ yyval.ival =  Y ; }
break;
case 54:
#line 502 "grammar.y"
{ yyval.ival =  DY ; }
break;
case 55:
#line 504 "grammar.y"
{ yyval.ival =  Width ; }
break;
case 56:
#line 506 "grammar.y"
{ yyval.ival =  Height ; }
break;
case 57:
#line 508 "grammar.y"
{ yyval.ival =  ForceSize ; }
break;
case 58:
#line 510 "grammar.y"
{ yyval.ival =  MaxWidth ; }
break;
case 59:
#line 512 "grammar.y"
{ yyval.ival =  MaxHeight ; }
break;
case 60:
#line 514 "grammar.y"
{ yyval.ival =  Columns ; }
break;
case 61:
#line 516 "grammar.y"
{ yyval.ival =  Override ; }
break;
case 62:
#line 518 "grammar.y"
{ yyval.ival =  Functions ; }
break;
case 63:
#line 520 "grammar.y"
{ yyval.ival =  Decorations ; }
break;
case 64:
#line 522 "grammar.y"
{ yyval.ival =  Popup ; }
break;
case 65:
#line 524 "grammar.y"
{ yyval.ival =  Popdown ; }
break;
case 66:
#line 526 "grammar.y"
{ yyval.ival =  Destroy ; }
break;
case 67:
#line 528 "grammar.y"
{ yyval.ival =  Exit ; }
break;
case 68:
#line 530 "grammar.y"
{ yyval.ival =  Help ; }
break;
case 69:
#line 532 "grammar.y"
{ yyval.ival =  Eval ; }
break;
case 70:
#line 534 "grammar.y"
{ yyval.ival =  PostNotice ; }
break;
case 71:
#line 536 "grammar.y"
{ yyval.ival =  RunForeground ; }
break;
case 72:
#line 538 "grammar.y"
{ yyval.ival =  RunBackground ; }
break;
case 73:
#line 540 "grammar.y"
{ yyval.ival =  CommandShell ; }
break;
case 74:
#line 542 "grammar.y"
{ yyval.ival =  InteractiveShell ; }
break;
case 75:
#line 544 "grammar.y"
{ yyval.ival =  InputFrom ; }
break;
case 76:
#line 546 "grammar.y"
{ yyval.ival =  CaptureOutput ; }
break;
case 77:
#line 548 "grammar.y"
{ yyval.ival =  NotifyComplete ; }
break;
case 78:
#line 550 "grammar.y"
{ yyval.ival =  UpdateFrom ; }
break;
case 79:
#line 552 "grammar.y"
{ yyval.ival =  PaneType ; }
break;
case 80:
#line 554 "grammar.y"
{ yyval.ival =  Pane ; }
break;
case 81:
#line 556 "grammar.y"
{ yyval.ival =  Store ; }
break;
case 82:
#line 558 "grammar.y"
{ yyval.ival =  Highlight ; }
break;
case 83:
#line 560 "grammar.y"
{ yyval.ival =  GetEnv ; }
break;
case 84:
#line 562 "grammar.y"
{ yyval.ival =  Clear ; }
break;
case 85:
#line 564 "grammar.y"
{ yyval.ival =  CommandArg ; }
break;
case 86:
#line 566 "grammar.y"
{ yyval.ival =  TableArg ; }
break;
case 87:
#line 568 "grammar.y"
{ yyval.ival =  Alignment ; }
break;
case 88:
#line 570 "grammar.y"
{ yyval.ival =  ListElement ; }
break;
case 89:
#line 572 "grammar.y"
{ yyval.ival =  ListSeparator ; }
break;
case 90:
#line 574 "grammar.y"
{ yyval.ival =  ListType ; }
break;
case 91:
#line 576 "grammar.y"
{ yyval.ival =  VisibleItems ; }
break;
case 92:
#line 578 "grammar.y"
{ yyval.ival =  ScrollBar ; }
break;
case 93:
#line 580 "grammar.y"
{ yyval.ival =  ValueString ; }
break;
case 94:
#line 582 "grammar.y"
{ yyval.ival =  Scrolled ; }
break;
case 95:
#line 584 "grammar.y"
{ yyval.ival =  LabelPixmap ; }
break;
case 96:
#line 586 "grammar.y"
{ yyval.ival =  MaxLength ; }
break;
case 97:
#line 588 "grammar.y"
{ yyval.ival =  Minimum ; }
break;
case 98:
#line 590 "grammar.y"
{ yyval.ival =  Maximum ; }
break;
case 99:
#line 592 "grammar.y"
{ yyval.ival =  StartValue ; }
break;
case 100:
#line 594 "grammar.y"
{ yyval.ival =  SliderWidth ; }
break;
case 101:
#line 596 "grammar.y"
{ yyval.ival =  SliderHeight ; }
break;
case 102:
#line 598 "grammar.y"
{ yyval.ival =  Orientation ; }
break;
case 103:
#line 600 "grammar.y"
{ yyval.ival =  DecimalPoints ; }
break;
case 104:
#line 602 "grammar.y"
{ yyval.ival =  EntryFont ; }
break;
case 105:
#line 604 "grammar.y"
{ yyval.ival =  Rows ; }
break;
case 106:
#line 606 "grammar.y"
{ yyval.ival =  RowsDisplayed ; }
break;
case 107:
#line 608 "grammar.y"
{ yyval.ival =  FixedRows ; }
break;
case 108:
#line 610 "grammar.y"
{ yyval.ival =  FixedColumns ; }
break;
case 109:
#line 612 "grammar.y"
{ yyval.ival =  ColumnsDisplayed ; }
break;
case 110:
#line 614 "grammar.y"
{ yyval.ival =  ColumnHeadings ; }
break;
case 111:
#line 616 "grammar.y"
{ yyval.ival =  RowHeadings ; }
break;
case 112:
#line 618 "grammar.y"
{ yyval.ival =  RowValue ; }
break;
case 113:
#line 620 "grammar.y"
{ yyval.ival =  TableValue ; }
break;
case 114:
#line 622 "grammar.y"
{ yyval.ival =  RowHeight ; }
break;
case 115:
#line 624 "grammar.y"
{ yyval.ival =  ColumnWidth ; }
break;
case 116:
#line 626 "grammar.y"
{ yyval.ival =  Newline ; }
break;
case 117:
#line 628 "grammar.y"
{ yyval.ival =  Separator ; }
break;
case 118:
#line 630 "grammar.y"
{ yyval.ival =  TitleString ; }
break;
case 119:
#line 632 "grammar.y"
{ yyval.ival =  ToggleType ; }
break;
case 120:
#line 634 "grammar.y"
{ yyval.ival =  ToggleState ; }
break;
case 121:
#line 636 "grammar.y"
{ yyval.ival =  SeparatorType ; }
break;
case 122:
#line 638 "grammar.y"
{ yyval.ival =  UpdateObject ; }
break;
case 123:
#line 640 "grammar.y"
{ yyval.ival =  Sensitive ; }
break;
case 124:
#line 642 "grammar.y"
{ yyval.ival =  Insensitive ; }
break;
case 129:
#line 655 "grammar.y"
{
			xgenGD.g_font = SaveString(yyvsp[0].cval);
			if ( (xgenGD.g_fs = XLoadQueryFont(xgenGD.display,xgenGD.g_font)) == 0 ) {
				char errorbuf[80];

				sprintf(errorbuf,"font %s not found",xgenGD.g_font);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		}
break;
case 130:
#line 666 "grammar.y"
{
			xgenGD.g_ffont = SaveString(yyvsp[0].cval);
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
		}
break;
case 131:
#line 685 "grammar.y"
{
			xgenGD.g_edfont = SaveString(yyvsp[0].cval);
			if ((xgenGD.g_edfs = XLoadQueryFont(xgenGD.display,xgenGD.g_edfont)) != 0 ) {
				if (xgenGD.g_edfs->min_bounds.width !=  xgenGD.g_edfs->max_bounds.width) {
					char errorbuf[80];
	
					sprintf(errorbuf,"fixed font %s, not fixed",xgenGD.g_edfont);
					XgenFatalWarning("parser",errorbuf);
					fatalError = True;
				} 
			} else {
					char errorbuf[80];
	
					sprintf(errorbuf,"fixed font %s not found",xgenGD.g_edfont);
					XgenFatalWarning("parser",errorbuf);
					fatalError = True;
			}
		}
break;
case 132:
#line 704 "grammar.y"
{
			xgenGD.g_bg = SaveString(yyvsp[0].cval);
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
		}
break;
case 133:
#line 723 "grammar.y"
{
			xgenGD.g_fg = SaveString(yyvsp[0].cval);
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
		}
break;
case 134:
#line 742 "grammar.y"
{ 
			Pixel tfg = ((xgenGD.g_fgs.pixel) ? 
				(xgenGD.g_fgs.pixel):BlackPixel(xgenGD.display,xgenGD.screen));
			Pixel tbg = ((xgenGD.g_bgs.pixel) ? 
				(xgenGD.g_bgs.pixel):WhitePixel(xgenGD.display,xgenGD.screen));

			xgenGD.g_bgpix = SaveString(yyvsp[0].cval);
			xgenGD.g_bgpm = XmGetPixmap(xgenGD.scrptr, xgenGD.g_bgpix, tfg, tbg);
			if ( xgenGD.g_bgpm == XmUNSPECIFIED_PIXMAP ) {
				char errorbuf[80];

				sprintf(errorbuf,
					"invalid background pixmap %s",xgenGD.g_bgpix);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		}
break;
case 135:
#line 760 "grammar.y"
{ 
			xgenGD.g_ts = SaveString(yyvsp[0].cval);
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
		}
break;
case 136:
#line 780 "grammar.y"
{ 
			Pixel tfg = ((xgenGD.g_fgs.pixel) ? 
				(xgenGD.g_fgs.pixel):BlackPixel(xgenGD.display,xgenGD.screen));
			Pixel tbg = ((xgenGD.g_bgs.pixel) ? 
				(xgenGD.g_bgs.pixel):WhitePixel(xgenGD.display,xgenGD.screen));

			xgenGD.g_tspix = SaveString(yyvsp[0].cval);
			xgenGD.g_tspm = XmGetPixmap(xgenGD.scrptr, xgenGD.g_tspix, tfg, tbg);
			if ( xgenGD.g_tspm == XmUNSPECIFIED_PIXMAP ) {
				char errorbuf[80];

				sprintf(errorbuf,
					"invalid top shadow pixmap %s",xgenGD.g_tspix);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		}
break;
case 137:
#line 798 "grammar.y"
{ 
			xgenGD.g_bs = SaveString(yyvsp[0].cval);
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
		}
break;
case 138:
#line 818 "grammar.y"
{ 
			Pixel tfg = ((xgenGD.g_fgs.pixel) ? 
				(xgenGD.g_fgs.pixel):BlackPixel(xgenGD.display,xgenGD.screen));
			Pixel tbg = ((xgenGD.g_bgs.pixel) ? 
				(xgenGD.g_bgs.pixel):WhitePixel(xgenGD.display,xgenGD.screen));

			xgenGD.g_bspix = SaveString(yyvsp[0].cval);
			xgenGD.g_bspm = XmGetPixmap(xgenGD.scrptr, xgenGD.g_bspix, tfg, tbg);
			if ( xgenGD.g_bspm == XmUNSPECIFIED_PIXMAP ) {
				char errorbuf[80];

				sprintf(errorbuf,
					"invalid bottom shadow pixmap %s",xgenGD.g_bspix);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		}
break;
case 139:
#line 839 "grammar.y"
{
			yyval.cval = SaveString(yyvsp[0].cval);
		}
break;
case 140:
#line 843 "grammar.y"
{
			char buf[80];

			sprintf(buf,"%d",yyvsp[0].ival);
			yyval.cval = SaveString(buf);
		}
break;
case 141:
#line 850 "grammar.y"
{
			char buf[80];

			sprintf(buf,"%f",yyvsp[0].dval);
			yyval.cval = SaveString(buf);
		}
break;
case 142:
#line 857 "grammar.y"
{
			if ( yyvsp[0].bval == True ) yyval.cval = SaveString("True");
			else yyval.cval = SaveString("False");
		}
break;
#line 1487 "y.tab.c"
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
