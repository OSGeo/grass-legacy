#ifndef lint
/*static char zzsccsid[] = "from: @(#)yaccpar	1.9 (Berkeley) 02/21/93";*/
static char zzrcsid[] = "$Id: skeleton.c,v 1.4 1993/12/21 18:45:32 jtc Exp $";
#endif
#define ZZBYACC 1
#define ZZMAJOR 1
#define ZZMINOR 9
#define zzclearin (zzchar=(-1))
#define zzerrok (zzerrflag=0)
#define ZZRECOVERING (zzerrflag!=0)
#define ZZPREFIX "zz"
#line 5 "xc.grammar.y"

#include "xc.xclip.h"

int dialogLevel = 0;
XCInterfaceData *interface = NULL;
XCInterfaceData *curPtr = NULL;
XCInterfaceData *tempPtr = NULL;
extern XclipGlobalData *zzGlobal;

#define CURFLAG (curPtr->flag)
#define CURPARM (curPtr->parm)

XCInterfaceData *_xc_stack[50];
int _xc_size = 0;
#define PUSHDIALOGPTR(p) ((void)(_xc_stack[_xc_size++] = (p)))
#define POPDIALOGPTR     (_xc_stack[_xc_size] = NULL , _xc_stack[--_xc_size])

#line 26 "xc.grammar.y"
typedef union {
    char *  cval;
    int ival;
    double dval;
    Boolean bval;
} ZZSTYPE;
#line 38 "y.tab.c"
#define String 257
#define Integer 258
#define Real 259
#define Logical 260
#define Program 261
#define Title 262
#define Description 263
#define Help 264
#define HelpFile 265
#define HelpWidgetRef 266
#define Capture 267
#define ErrorCodes 268
#define CommandString 269
#define Dialog 270
#define Flag 271
#define Parameter 272
#define Key 273
#define Requires 274
#define Precludes 275
#define Optional 276
#define Input 277
#define Multiple 278
#define Type 279
#define TypeEnumerate 280
#define TypeFileName 281
#define TypeDatabaseElement 282
#define TypeCharacter 283
#define TypeInteger 284
#define TypeFloat 285
#define TypeDouble 286
#define TypeLogical 287
#define DatabaseElementRaster 288
#define DatabaseElementAsciiDlg 289
#define DatabaseElementDlg 290
#define DatabaseElementAsciiVector 291
#define DatabaseElementVector 292
#define DatabaseElementSites 293
#define DatabaseElementRegion 294
#define DatabaseElementIcon 295
#define DatabaseElementLabel 296
#define DatabaseElementGroup 297
#define DatabaseElementSubGroup 298
#define StatusOld 299
#define StatusNew 300
#define SelectType 301
#define Default 302
#define ZZERRCODE 256
short zzlhs[] = {                                        -1,
    3,    5,    0,    6,    7,    9,    2,    8,    8,   10,
   10,   11,   11,   11,   11,   11,   11,    4,    4,   12,
   12,   13,   13,   13,   18,   16,   17,   17,   19,   19,
   19,   20,   21,   23,   14,   22,   22,   24,   24,   25,
   25,   25,   25,   25,   27,   29,   15,   26,   26,   30,
   32,   30,   31,   31,   31,   31,   31,   31,   31,   38,
   31,   33,   33,   34,   34,   35,   35,   36,   36,   40,
   37,   41,   37,   42,   37,   43,   37,   44,   37,   45,
   37,   46,   37,   47,   37,   48,   37,   49,   37,   50,
   37,   37,   51,   39,   52,   39,   53,   39,   28,   28,
   54,   54,   54,   54,   54,   54,   54,   54,   54,    1,
    1,
};
short zzlen[] = {                                         2,
    0,    0,    4,    0,    0,    0,   13,    0,    1,    2,
    1,    3,    3,    3,    3,    3,    3,    0,    3,    2,
    1,    1,    1,    1,    0,    8,    0,    3,    0,    3,
    3,    0,    0,    0,   13,    0,    1,    2,    1,    3,
    3,    3,    5,    5,    0,    0,    7,    2,    1,    3,
    0,    2,    3,    5,    4,    4,    4,    4,    4,    0,
    4,    0,    2,    0,    2,    0,    2,    0,    2,    0,
    5,    0,    5,    0,    5,    0,    5,    0,    5,    0,
    5,    0,    5,    0,    5,    0,    5,    0,    5,    0,
    5,    3,    0,    0,    0,    3,    0,    3,    2,    1,
    3,    3,    3,    3,    5,    5,    3,    3,    3,    1,
    1,
};
short zzdefred[] = {                                      1,
    0,    0,    0,    2,    0,    0,    4,    0,    3,    0,
    0,    0,    0,    0,   21,   22,   23,   24,    0,   25,
   32,   45,   19,   20,    0,    0,    0,    0,    5,    0,
    0,    0,    0,    0,   49,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   48,    0,   52,    0,   28,    0,
    0,    0,   33,   50,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  100,    0,    0,   30,   31,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   47,   99,    0,    0,   53,    0,    0,    0,    0,    0,
    6,   26,    0,  103,  104,  110,  111,    0,    0,  109,
  108,  107,  101,  102,    0,    0,   59,    0,   55,    0,
   56,   57,    0,   58,    0,   61,    0,    0,    0,    0,
   54,   69,   63,   65,   67,    0,    0,    0,    0,    0,
    0,    0,    7,    0,   11,   34,  106,  105,   70,   72,
   74,   76,   78,   80,   82,   84,   86,   88,   90,   92,
    0,    0,    0,    0,    0,    0,   10,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   12,
   13,   14,   15,   16,   17,    0,    0,    0,    0,    0,
    0,    0,   39,    0,   71,   73,   75,   77,   79,   81,
   83,   85,   87,   89,   91,    0,    0,    0,    0,    0,
   35,   38,    0,    0,   40,   41,    0,    0,   42,   96,
   98,    0,    0,   44,   43,
};
short zzdgoto[] = {                                       1,
   98,    4,    2,    9,    6,   10,   37,  133,  117,  134,
  135,   14,   15,   16,   17,   18,   31,   26,   41,   27,
   71,  181,  158,  182,  183,   34,   28,   64,   44,   35,
   47,   36,  109,  111,  114,  107,  116,   90,  185,  159,
  160,  161,  162,  163,  164,  165,  166,  167,  168,  169,
  150,  203,  204,   65,
};
short zzsindex[] = {                                      0,
    0, -226,   -8,    0, -206,  -57,    0, -259,    0, -195,
 -189, -187, -177, -124,    0,    0,    0,    0,   25,    0,
    0,    0,    0,    0, -173, -178, -176, -163,    0,   28,
 -216,   29,   43, -163,    0, -175, -167, -154,   53,   54,
  -10, -143, -134, -106,    0,   66,    0,   69,    0, -127,
 -126, -259,    0,    0,   76,   77,   78,   79,   80,   81,
   82,   83,  102,  -59,    0, -247,  -96,    0,    0, -121,
 -101,  -94,  -93, -254, -254,  -92,  -91,  -90,  -84,  -83,
    0,    0,  117,  118,    0,  119,  120,  120,  134,  -89,
    0,    0,  136,    0,    0,    0,    0,  139,  140,    0,
    0,    0,    0,    0,  -58,  -56,    0,  -55,    0,  -54,
    0,    0,  -53,    0,  142,    0, -146,  -50,  -49,  -48,
    0,    0,    0,    0,    0, -107,  152,  153,  154,  155,
  156,  172,    0, -146,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  -26,  -25,  -24,  -23,  -22,  -21,    0, -233,  179,  179,
  179,  179,  179,  179,  179,  179,  179,  179,  179,    0,
    0,    0,    0,    0,    0,  180,  181,  182,  183,  186,
  187, -233,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  -12,   -9, -254, -254,   -6,
    0,    0,  -52,  -51,    0,    0,  192,  194,    0,    0,
    0,   -4,   -3,    0,    0,
};
short zzrindex[] = {                                      0,
    0,    0,    0,    0,    0,  255,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0, -120,    0,  -20,    0,    0,
  133,    0,    0, -122,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  -19,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0, -255,    0, -249, -220, -220, -203,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    5,    0,    0,    0,
    0,    0,    0,    0,    0, -186,    0,    0,    0,    0,
    0,    0,    0,    6,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  198, -169, -169,
 -169, -169, -169, -169, -169, -169, -169, -169, -169,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  199,    0, -235,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,
};
short zzgindex[] = {                                      0,
  -73,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  126,  209,   -7,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   84,    0,    0,    0,    0,  228,
    0,    0,    0,  176,    0,    0,    0,    0,   60,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  201,
};
#define ZZTABLESIZE 266
short zztable[] = {                                      81,
   23,   99,   27,   92,    8,    9,   24,   68,   68,   68,
   11,   12,   13,   62,   62,   62,   96,   97,   68,   68,
   68,   68,   68,   68,   62,   62,   62,   62,   62,   62,
  176,  177,   83,   84,    3,   85,   86,   87,   88,   89,
  178,  179,   64,   64,   64,   68,   68,   39,   40,    5,
    7,   62,   62,   64,   64,   64,   64,   64,   64,   66,
   66,   66,   24,   95,   97,    8,   19,   20,  180,   21,
   66,   66,   66,   66,   66,   66,   93,   93,   93,   22,
   64,   64,   25,   29,   30,   38,   42,   93,   93,   93,
   93,   93,   93,   94,   94,   94,   32,   66,   66,   33,
   43,   48,   49,   46,   94,   94,   94,   94,   94,   94,
   50,   51,   52,   53,   93,   93,  127,  128,  129,  130,
  131,  132,   54,   66,  207,  208,   67,    8,    9,   68,
   69,   94,   94,   72,   73,   74,   75,   76,   77,   78,
   79,   46,   46,   27,   27,   11,   12,   13,   11,   12,
   13,   46,   46,   46,   46,   46,   51,   55,   56,   80,
   91,   93,   94,   95,  100,  101,  102,   57,   58,   59,
   60,   61,  103,  104,  105,  106,  108,  110,   46,   46,
  139,  140,  141,  142,  143,  144,  145,  146,  147,  148,
  149,  113,  115,  118,   62,   63,  119,  120,  121,  126,
  122,  123,  124,  125,   55,   56,  136,  137,  138,  151,
  152,  153,  154,  155,   57,   58,   59,   60,   61,  186,
  187,  188,  189,  190,  191,  192,  193,  194,  195,  156,
  170,  171,  172,  173,  174,  175,  184,  196,  197,  198,
  199,   62,   63,  200,  205,  201,  210,  206,  211,  212,
  209,  213,  214,  215,   18,   29,   36,   37,   51,  157,
   70,   45,   60,  112,   82,  202,
};
short zzcheck[] = {                                      59,
  125,   75,  123,  125,    0,    0,   14,  263,  264,  265,
  270,  271,  272,  263,  264,  265,  271,  272,  274,  275,
  276,  277,  278,  279,  274,  275,  276,  277,  278,  279,
  264,  265,  280,  281,  261,  283,  284,  285,  286,  287,
  274,  275,  263,  264,  265,  301,  302,  264,  265,   58,
  257,  301,  302,  274,  275,  276,  277,  278,  279,  263,
  264,  265,   70,  299,  300,  123,  262,  257,  302,  257,
  274,  275,  276,  277,  278,  279,  263,  264,  265,  257,
  301,  302,   58,  257,  263,   58,   58,  274,  275,  276,
  277,  278,  279,  263,  264,  265,  273,  301,  302,  263,
   58,  269,  257,  279,  274,  275,  276,  277,  278,  279,
   58,   58,  123,  257,  301,  302,  263,  264,  265,  266,
  267,  268,  257,   58,  198,  199,   58,  123,  123,  257,
  257,  301,  302,   58,   58,   58,   58,   58,   58,   58,
   58,  264,  265,  264,  265,  270,  271,  272,  270,  271,
  272,  274,  275,  276,  277,  278,  279,  264,  265,   58,
  257,  263,  257,  257,  257,  257,  257,  274,  275,  276,
  277,  278,  257,  257,   58,   58,   58,   58,  301,  302,
  288,  289,  290,  291,  292,  293,  294,  295,  296,  297,
  298,   58,  282,   58,  301,  302,   58,   58,  257,   58,
  257,  257,  257,  257,  264,  265,  257,  257,  257,   58,
   58,   58,   58,   58,  274,  275,  276,  277,  278,  160,
  161,  162,  163,  164,  165,  166,  167,  168,  169,   58,
  257,  257,  257,  257,  257,  257,   58,   58,   58,   58,
   58,  301,  302,   58,  257,   59,  299,  257,  300,   58,
  257,   58,  257,  257,    0,  123,   59,   59,  279,  134,
   52,   34,  282,   88,   64,  182,
};
#define ZZFINAL 1
#ifndef ZZDEBUG
#define ZZDEBUG 0
#endif
#define ZZMAXTOKEN 302
#if ZZDEBUG
char *zzname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"':'","';'",0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,"'{'",0,"'}'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"String",
"Integer","Real","Logical","Program","Title","Description","Help","HelpFile",
"HelpWidgetRef","Capture","ErrorCodes","CommandString","Dialog","Flag",
"Parameter","Key","Requires","Precludes","Optional","Input","Multiple","Type",
"TypeEnumerate","TypeFileName","TypeDatabaseElement","TypeCharacter",
"TypeInteger","TypeFloat","TypeDouble","TypeLogical","DatabaseElementRaster",
"DatabaseElementAsciiDlg","DatabaseElementDlg","DatabaseElementAsciiVector",
"DatabaseElementVector","DatabaseElementSites","DatabaseElementRegion",
"DatabaseElementIcon","DatabaseElementLabel","DatabaseElementGroup",
"DatabaseElementSubGroup","StatusOld","StatusNew","SelectType","Default",
};
char *zzrule[] = {
"$accept : Interface",
"$$1 :",
"$$2 :",
"Interface : $$1 ProgramHeader $$2 MainProgram",
"$$3 :",
"$$4 :",
"$$5 :",
"ProgramHeader : Program ':' String $$3 Title ':' String $$4 CommandString ':' String $$5 OptionalHeaderElementList",
"OptionalHeaderElementList :",
"OptionalHeaderElementList : HeaderElementList",
"HeaderElementList : HeaderElementList HeaderElement",
"HeaderElementList : HeaderElement",
"HeaderElement : Description ':' String",
"HeaderElement : Help ':' String",
"HeaderElement : HelpFile ':' String",
"HeaderElement : HelpWidgetRef ':' String",
"HeaderElement : Capture ':' String",
"HeaderElement : ErrorCodes ':' String",
"MainProgram :",
"MainProgram : '{' FlagOrParmOrDialogDescriptionList '}'",
"FlagOrParmOrDialogDescriptionList : FlagOrParmOrDialogDescriptionList FlagOrParmOrDialogDescription",
"FlagOrParmOrDialogDescriptionList : FlagOrParmOrDialogDescription",
"FlagOrParmOrDialogDescription : FlagDescription",
"FlagOrParmOrDialogDescription : ParmDescription",
"FlagOrParmOrDialogDescription : DialogDescription",
"$$6 :",
"DialogDescription : Dialog String $$6 OptionalDescription OptionalDialogElementList '{' FlagOrParmOrDialogDescriptionList '}'",
"OptionalDescription :",
"OptionalDescription : Description ':' String",
"OptionalDialogElementList :",
"OptionalDialogElementList : Help ':' String",
"OptionalDialogElementList : HelpFile ':' String",
"$$7 :",
"$$8 :",
"$$9 :",
"FlagDescription : Flag String $$7 Key ':' String $$8 Description ':' String $$9 OptFlagDescList ';'",
"OptFlagDescList :",
"OptFlagDescList : OptionalFlagDescList",
"OptionalFlagDescList : OptionalFlagDescList OptionalFlagDesc",
"OptionalFlagDescList : OptionalFlagDesc",
"OptionalFlagDesc : Help ':' String",
"OptionalFlagDesc : HelpFile ':' String",
"OptionalFlagDesc : Default ':' String",
"OptionalFlagDesc : Precludes ':' FlagOrParmKey ':' String",
"OptionalFlagDesc : Requires ':' FlagOrParmKey ':' String",
"$$10 :",
"$$11 :",
"ParmDescription : Parameter String $$10 ParmReqList $$11 ParmOptionList ';'",
"ParmReqList : ParmReqList ParmReq",
"ParmReqList : ParmReq",
"ParmReq : Description ':' String",
"$$12 :",
"ParmReq : $$12 TypeOption",
"TypeOption : Type ':' TypeCharacter",
"TypeOption : Type ':' TypeEnumerate ':' String",
"TypeOption : Type ':' TypeInteger OptIntegerModifier",
"TypeOption : Type ':' TypeFloat OptDoubleModifier",
"TypeOption : Type ':' TypeDouble OptDoubleModifier",
"TypeOption : Type ':' TypeLogical OptLogicalModifier",
"TypeOption : Type ':' TypeFileName OptFilenameModifier",
"$$13 :",
"TypeOption : Type ':' $$13 DatabaseElementDesc",
"OptIntegerModifier :",
"OptIntegerModifier : ':' String",
"OptDoubleModifier :",
"OptDoubleModifier : ':' String",
"OptLogicalModifier :",
"OptLogicalModifier : ':' String",
"OptFilenameModifier :",
"OptFilenameModifier : ':' String",
"$$14 :",
"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementRaster $$14 OptionalStatus",
"$$15 :",
"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementAsciiDlg $$15 OptionalStatus",
"$$16 :",
"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementDlg $$16 OptionalStatus",
"$$17 :",
"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementAsciiVector $$17 OptionalStatus",
"$$18 :",
"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementVector $$18 OptionalStatus",
"$$19 :",
"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementSites $$19 OptionalStatus",
"$$20 :",
"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementRegion $$20 OptionalStatus",
"$$21 :",
"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementIcon $$21 OptionalStatus",
"$$22 :",
"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementLabel $$22 OptionalStatus",
"$$23 :",
"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementGroup $$23 OptionalStatus",
"$$24 :",
"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementSubGroup $$24 OptionalStatus",
"DatabaseElementDesc : TypeDatabaseElement ':' DatabaseElementUserDefined",
"DatabaseElementUserDefined :",
"OptionalStatus :",
"$$25 :",
"OptionalStatus : ':' $$25 StatusOld",
"$$26 :",
"OptionalStatus : ':' $$26 StatusNew",
"ParmOptionList : ParmOptionList ParmOption",
"ParmOptionList : ParmOption",
"ParmOption : SelectType ':' String",
"ParmOption : Default ':' String",
"ParmOption : Help ':' String",
"ParmOption : HelpFile ':' String",
"ParmOption : Precludes ':' FlagOrParmKey ':' String",
"ParmOption : Requires ':' FlagOrParmKey ':' String",
"ParmOption : Multiple ':' String",
"ParmOption : Input ':' String",
"ParmOption : Optional ':' String",
"FlagOrParmKey : Flag",
"FlagOrParmKey : Parameter",
};
#endif
#ifdef ZZSTACKSIZE
#undef ZZMAXDEPTH
#define ZZMAXDEPTH ZZSTACKSIZE
#else
#ifdef ZZMAXDEPTH
#define ZZSTACKSIZE ZZMAXDEPTH
#else
#define ZZSTACKSIZE 500
#define ZZMAXDEPTH 500
#endif
#endif
int zzdebug;
int zznerrs;
int zzerrflag;
int zzchar;
short *zzssp;
ZZSTYPE *zzvsp;
ZZSTYPE zzval;
ZZSTYPE zzlval;
short zzss[ZZSTACKSIZE];
ZZSTYPE zzvs[ZZSTACKSIZE];
#define zzstacksize ZZSTACKSIZE
#line 1106 "xc.grammar.y"
zzinitParser()
{
interface = NULL;
_xc_size = 0;
curPtr = NULL;
tempPtr = NULL;
}
#line 430 "y.tab.c"
#define ZZABORT goto zzabort
#define ZZREJECT goto zzabort
#define ZZACCEPT goto zzaccept
#define ZZERROR goto zzerrlab
int
#if defined(__STDC__)
zzparse(void)
#else
zzparse()
#endif
{
    register int zzm, zzn, zzstate;
#if ZZDEBUG
    register char *zzs;
    extern char *getenv();

    if (zzs = getenv("ZZDEBUG"))
    {
        zzn = *zzs;
        if (zzn >= '0' && zzn <= '9')
            zzdebug = zzn - '0';
    }
#endif

    zznerrs = 0;
    zzerrflag = 0;
    zzchar = (-1);

    zzssp = zzss;
    zzvsp = zzvs;
    *zzssp = zzstate = 0;

zzloop:
    if ((zzn = zzdefred[zzstate]) != 0) goto zzreduce;
    if (zzchar < 0)
    {
        if ((zzchar = zzlex()) < 0) zzchar = 0;
#if ZZDEBUG
        if (zzdebug)
        {
            zzs = 0;
            if (zzchar <= ZZMAXTOKEN) zzs = zzname[zzchar];
            if (!zzs) zzs = "illegal-symbol";
            printf("%sdebug: state %d, reading %d (%s)\n",
                    ZZPREFIX, zzstate, zzchar, zzs);
        }
#endif
    }
    if ((zzn = zzsindex[zzstate]) && (zzn += zzchar) >= 0 &&
            zzn <= ZZTABLESIZE && zzcheck[zzn] == zzchar)
    {
#if ZZDEBUG
        if (zzdebug)
            printf("%sdebug: state %d, shifting to state %d\n",
                    ZZPREFIX, zzstate, zztable[zzn]);
#endif
        if (zzssp >= zzss + zzstacksize - 1)
        {
            goto zzoverflow;
        }
        *++zzssp = zzstate = zztable[zzn];
        *++zzvsp = zzlval;
        zzchar = (-1);
        if (zzerrflag > 0)  --zzerrflag;
        goto zzloop;
    }
    if ((zzn = zzrindex[zzstate]) && (zzn += zzchar) >= 0 &&
            zzn <= ZZTABLESIZE && zzcheck[zzn] == zzchar)
    {
        zzn = zztable[zzn];
        goto zzreduce;
    }
    if (zzerrflag) goto zzinrecovery;
    zzerror("syntax error");
#ifdef lint
    goto zzerrlab;
#endif
zzerrlab:
    ++zznerrs;
zzinrecovery:
    if (zzerrflag < 3)
    {
        zzerrflag = 3;
        for (;;)
        {
            if ((zzn = zzsindex[*zzssp]) && (zzn += ZZERRCODE) >= 0 &&
                    zzn <= ZZTABLESIZE && zzcheck[zzn] == ZZERRCODE)
            {
#if ZZDEBUG
                if (zzdebug)
                    printf("%sdebug: state %d, error recovery shifting\
 to state %d\n", ZZPREFIX, *zzssp, zztable[zzn]);
#endif
                if (zzssp >= zzss + zzstacksize - 1)
                {
                    goto zzoverflow;
                }
                *++zzssp = zzstate = zztable[zzn];
                *++zzvsp = zzlval;
                goto zzloop;
            }
            else
            {
#if ZZDEBUG
                if (zzdebug)
                    printf("%sdebug: error recovery discarding state %d\n",
                            ZZPREFIX, *zzssp);
#endif
                if (zzssp <= zzss) goto zzabort;
                --zzssp;
                --zzvsp;
            }
        }
    }
    else
    {
        if (zzchar == 0) goto zzabort;
#if ZZDEBUG
        if (zzdebug)
        {
            zzs = 0;
            if (zzchar <= ZZMAXTOKEN) zzs = zzname[zzchar];
            if (!zzs) zzs = "illegal-symbol";
            printf("%sdebug: state %d, error recovery discards token %d (%s)\n",
                    ZZPREFIX, zzstate, zzchar, zzs);
        }
#endif
        zzchar = (-1);
        goto zzloop;
    }
zzreduce:
#if ZZDEBUG
    if (zzdebug)
        printf("%sdebug: state %d, reducing by rule %d (%s)\n",
                ZZPREFIX, zzstate, zzn, zzrule[zzn]);
#endif
    zzm = zzlen[zzn];
    zzval = zzvsp[1-zzm];
    switch (zzn)
    {
case 1:
#line 105 "xc.grammar.y"
{
        bzero((char *)&zzGlobal->_xc_Data, sizeof(_XC_data));
    }
break;
case 2:
#line 109 "xc.grammar.y"
{ 
        if ( XCverbose) {
            DoFprintf(dialogLevel,"COMPLETED -> Header Section\n"); 
        }
    }
break;
case 3:
#line 115 "xc.grammar.y"
{ 
        zzGlobal->_xc_Data.data = interface;
        /* check precludes and requirements for valid names */
        _XcValidDependencies(zzGlobal->_xc_Data.data,zzGlobal);
        if ( XCverbose) {
            DoFprintf(dialogLevel,"COMPLETED -> Program Section\n"); 
        }
    }
break;
case 4:
#line 129 "xc.grammar.y"
{ 
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"BEGIN -> Description of Program: %s\n",zzvsp[0].cval);
        }
        zzGlobal->_xc_Data.prog = _XgStrDup(zzvsp[0].cval);
    }
break;
case 5:
#line 136 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tTitle: %s\n",zzvsp[0].cval);
        }
        zzGlobal->_xc_Data.title = _XgStrDup(zzvsp[0].cval);
    }
break;
case 6:
#line 143 "xc.grammar.y"
{
        zzGlobal->_xc_Data.argString = _XgStrDup(zzvsp[0].cval);
    }
break;
case 12:
#line 169 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tDesc: %s\n",zzvsp[0].cval);
            DoFprintf(dialogLevel,"\tOptional Header Elements:\n");
        }
        zzGlobal->_xc_Data.description = _XgStrDup(zzvsp[0].cval);
    }
break;
case 13:
#line 178 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelp: %s\n",zzvsp[0].cval);
        }
        zzGlobal->_xc_Data.help = _XgStrDup(zzvsp[0].cval);
    }
break;
case 14:
#line 186 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelpFile: %s\n",zzvsp[0].cval);
        }
        zzGlobal->_xc_Data.helpFile.filename = _XgStrDup(zzvsp[0].cval);
    }
break;
case 15:
#line 194 "xc.grammar.y"
{
	char *ptr = _XgStrDup(zzvsp[0].cval);
	XCHelpWidgetData *hptr = zzGlobal->_xc_Data.helpData;

        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelpWidgetRef: %s\n",zzvsp[0].cval);
        }
	if ( hptr ) {

	    while ( hptr->next ) hptr = hptr->next;
	    hptr->next = 
		(XCHelpWidgetData *)_XgMalloc(sizeof(XCHelpWidgetData));
	    hptr = hptr->next;
	    bzero((char *)hptr, sizeof(XCHelpWidgetData));
	} else {
	    hptr = zzGlobal->_xc_Data.helpData = 
		(XCHelpWidgetData *)_XgMalloc(sizeof(XCHelpWidgetData));
	    bzero((char *)hptr, sizeof(XCHelpWidgetData));
	}
	if ( !_XcParseHelpData(hptr,ptr) ) {
          sprintf(errorbuf,"Illegal help data [%s]",ptr);
          zzerror(errorbuf);
	}
	_XgFree(ptr);
    }
break;
case 16:
#line 221 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tCapture: %s\n",zzvsp[0].cval);
        }
        zzGlobal->_xc_Data.capture = (ISONSTR(zzvsp[0].cval) || ISTRUESTR(zzvsp[0].cval));
    }
break;
case 17:
#line 229 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tErrorcodes: %s\n",zzvsp[0].cval);
        }
        zzGlobal->_xc_Data.nerrcodes = _XcParseErrorCodes(zzvsp[0].cval,&(zzGlobal->_xc_Data.errorcodes));
	if ( zzGlobal->_xc_Data.nerrcodes == -1 ) {
          sprintf(errorbuf,"Illegal errorcode string [%s]", zzvsp[0].cval);
          zzerror(errorbuf);
	}
    }
break;
case 25:
#line 276 "xc.grammar.y"
{
        /* 
        * Allocate a new pointer and push it on to the dialog stack.
        */
        if ( interface == NULL ) {
            curPtr = interface = 
                (XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
            bzero((char *)interface, sizeof(XCInterfaceData));
	    PUSHDIALOGPTR(curPtr);
	    curPtr->type = XC_DIALOG;
	    curPtr->name = _XgStrDup(zzvsp[0].cval);
        }  else {
	    tempPtr = 
		(XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
	    bzero((char *)tempPtr, sizeof(XCInterfaceData));
	    /* 
	     * If the current pointer is NOT of type dialog, go left, else
	     * if the right branch of the current pointer is NULL, go right,
	     * else, go left.
	     */
	    if ( curPtr->type != XC_DIALOG ) {
		curPtr = curPtr->left = tempPtr;
	    } else if ( curPtr->right == NULL ) {
		curPtr = curPtr->right = tempPtr;
	    } else {
		curPtr = curPtr->left = tempPtr;
	    }
	    PUSHDIALOGPTR(curPtr);
	    curPtr->type = XC_DIALOG;
	    curPtr->name = _XgStrDup(zzvsp[0].cval);
	}
        dialogLevel++;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"BEGIN -> Dialog Description\n");
            DoFprintf(dialogLevel,"\tDialog Desc: %s\n",zzvsp[0].cval);
        }
    }
break;
case 26:
#line 318 "xc.grammar.y"
{
        /*
        * Done with this dialog, pop it off the stack and resume processing.
        */
        curPtr = POPDIALOGPTR;
        dialogLevel--;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"END -> Dialog Description\n");
        }
    }
break;
case 28:
#line 335 "xc.grammar.y"
{
	curPtr->desc = _XgStrDup(zzvsp[0].cval);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Desc: %s\n",zzvsp[0].cval);
        }
    }
break;
case 30:
#line 348 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelp: %s\n",zzvsp[0].cval);
        }
        curPtr->help = _XgStrDup(zzvsp[0].cval);
    }
break;
case 31:
#line 356 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelpFile: %s\n",zzvsp[0].cval);
        }
        curPtr->helpFile.filename = _XgStrDup(zzvsp[0].cval);
    }
break;
case 32:
#line 368 "xc.grammar.y"
{ 
	/* 
	* Check for uniqueness of the handle name
	*/
	if ( !_XcUniqueName(interface, zzvsp[0].cval) ) {
	    sprintf(errorbuf,"Non-unique flag or parameter name [%s]",zzvsp[0].cval);
	    zzerror(errorbuf);
	}
        /*
        * Allocate a new pointer and link it in to the list.
        */
        if ( interface == NULL ) {
            curPtr = interface = 
                (XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
            bzero((char *)interface, sizeof(XCInterfaceData));
        } else {
	    tempPtr = 
		(XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
	    bzero((char *)tempPtr, sizeof(XCInterfaceData));
	    if ( curPtr->type != XC_DIALOG ) {
		curPtr = curPtr->left = tempPtr;
	    } else if ( curPtr->right == NULL ) {
		curPtr = curPtr->right = tempPtr;
	    } else {
		curPtr = curPtr->left = tempPtr;
	    }
        }
        curPtr->type = XC_FLAG;
        curPtr->name = _XgStrDup(zzvsp[0].cval);
	CURFLAG = (XCFlagData *)_XgMalloc(sizeof(XCFlagData));
	bzero((char *)CURFLAG,sizeof(XCFlagData));
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"BEGIN -> Flag Description\n");
            DoFprintf(dialogLevel,"\tName: %s\n",zzvsp[0].cval);
        }
    }
break;
case 33:
#line 405 "xc.grammar.y"
{ 
	CURFLAG->key = _XgStrDup(zzvsp[0].cval);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tKey: %s\n",zzvsp[0].cval);
        }
    }
break;
case 34:
#line 412 "xc.grammar.y"
{
	CURFLAG->desc = _XgStrDup(zzvsp[-4].cval);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tDesc: %s\n",zzvsp[0].cval);
            DoFprintf(dialogLevel,"\tOptional Flag Elements:\n");
        }
    }
break;
case 35:
#line 421 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"COMPLETED -> Flag\n");
        }
    }
break;
case 40:
#line 448 "xc.grammar.y"
{
	CURFLAG->help = _XgStrDup(zzvsp[0].cval);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelp: %s\n",zzvsp[0].cval);
        }
    }
break;
case 41:
#line 456 "xc.grammar.y"
{
	CURFLAG->helpFile.filename = _XgStrDup(zzvsp[0].cval);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelpFile: %s\n",zzvsp[0].cval);
        }
    }
break;
case 42:
#line 464 "xc.grammar.y"
{
	CURFLAG->def = ISONSTR(zzvsp[0].cval);
	CURFLAG->flagSet = True;
	CURFLAG->answer = curPtr->flag->def;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tDefault: %s\n",zzvsp[0].cval);
        }
    }
break;
case 43:
#line 474 "xc.grammar.y"
{
	if (CURFLAG->precludes ) {
	    XCRequireData *rPtr = &CURFLAG->preclude;

	    while ( rPtr->next ) {
		rPtr = rPtr->next;
	    }
	    rPtr->next = (XCRequireData *)XtMalloc(sizeof(XCRequireData));
	    rPtr = rPtr->next;
	    rPtr->isFlag = ISFLAGSTR(zzvsp[-2].cval);
	    rPtr->name = _XgStrDup(zzvsp[0].cval);
	    rPtr->next = NULL;
	} else {
	    CURFLAG->precludes = True;
	    CURFLAG->preclude.isFlag = ISFLAGSTR(zzvsp[-2].cval);
	    CURFLAG->preclude.name = _XgStrDup(zzvsp[0].cval);
	    CURFLAG->preclude.next = NULL;
	}
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tPreculdes: %s -> %s\n",zzvsp[-2].cval,zzvsp[0].cval);
        }
    }
break;
case 44:
#line 498 "xc.grammar.y"
{
	if (CURFLAG->requirements ) {
	    XCRequireData *rPtr = &CURFLAG->requires;

	    while ( rPtr->next ) {
		rPtr = rPtr->next;
	    }
	    rPtr->next = (XCRequireData *)XtMalloc(sizeof(XCRequireData));
	    rPtr = rPtr->next;
	    rPtr->isFlag = ISFLAGSTR(zzvsp[-2].cval);
	    rPtr->name = _XgStrDup(zzvsp[0].cval);
	    rPtr->next = NULL;
	} else {
	    CURFLAG->requirements = True;
	    CURFLAG->requires.isFlag = ISFLAGSTR(zzvsp[-2].cval);
	    CURFLAG->requires.name = _XgStrDup(zzvsp[0].cval);
	    CURFLAG->requires.next = NULL;
	}
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tRequires: %s -> %s\n",zzvsp[-2].cval,zzvsp[0].cval);
        }
    }
break;
case 45:
#line 526 "xc.grammar.y"
{
	/* 
	* Check for uniqueness of the handle name
	*/
	if ( !_XcUniqueName(interface, zzvsp[0].cval) ) {
	    sprintf(errorbuf,"Non-unique flag or parameter name [%s]",zzvsp[0].cval);
	    zzerror(errorbuf);
	}
        if ( interface == NULL ) {
            curPtr = interface = 
                (XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
            bzero((char *)interface, sizeof(XCInterfaceData));
        } else {
	    tempPtr = 
		(XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
	    bzero((char *)tempPtr, sizeof(XCInterfaceData));
	    if ( curPtr->type != XC_DIALOG ) {
		curPtr = curPtr->left = tempPtr;
	    } else if ( curPtr->right == NULL ) {
		curPtr = curPtr->right = tempPtr;
	    } else {
		curPtr = curPtr->left = tempPtr;
	    }
        }
        curPtr->type = XC_PARM;
        curPtr->name = _XgStrDup(zzvsp[0].cval);
	curPtr->parm = (XCParmData *)_XgMalloc(sizeof(XCParmData));
	bzero((char *)curPtr->parm,sizeof(XCParmData));
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"BEGIN -> Parm Description\n");
            DoFprintf(dialogLevel,"\tKey: %s\n",zzvsp[0].cval);
        }
    }
break;
case 46:
#line 560 "xc.grammar.y"
{
        /*
        * If this is a database element and an input parameter and has a 
        * default, check it out for existence.
        */
	if ( CURPARM->type == XC_TYPE_UNKNOWN ) {
          sprintf(errorbuf,"No type given for parameter [%s]",curPtr->name);
          zzerror(errorbuf);
	}
        if ( CURPARM->parmSet && CURPARM->isInput &&
             CURPARM->type == XC_TYPE_DB_ELEMENT )
            VerifyDBElement(CURPARM);
    }
break;
case 47:
#line 574 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"COMPLETED -> Parm\n");
        }
    }
break;
case 50:
#line 593 "xc.grammar.y"
{
	CURPARM->desc = _XgStrDup(zzvsp[0].cval);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tDesc: %s\n",zzvsp[0].cval);
        }
    }
break;
case 51:
#line 601 "xc.grammar.y"
{
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tType:");
            fflush(stderr);
        }
    }
break;
case 53:
#line 614 "xc.grammar.y"
{
        CURPARM->type = XC_TYPE_CHARACTER;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Character\n");
        }
    }
break;
case 54:
#line 622 "xc.grammar.y"
{
        CURPARM->type = XC_TYPE_ENUMERATE;
	CURPARM->hasModifier = True;
        XcModifierParse(&CURPARM->modifier,XC_TYPE_ENUMERATE,zzvsp[0].cval);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Enumerate\n");
            DoFprintf(dialogLevel,"\tValues: %s\n", zzvsp[0].cval);
        }
    }
break;
case 55:
#line 633 "xc.grammar.y"
{
        CURPARM->type = XC_TYPE_INTEGER;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Integer\n");
        }
    }
break;
case 56:
#line 641 "xc.grammar.y"
{
        CURPARM->type = XC_TYPE_FLOAT;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Float\n");
        }
    }
break;
case 57:
#line 649 "xc.grammar.y"
{
        CURPARM->type = XC_TYPE_DOUBLE;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Double\n");
        }
    }
break;
case 58:
#line 657 "xc.grammar.y"
{
        CURPARM->type = XC_TYPE_LOGICAL;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Logical\n");
        }
    }
break;
case 59:
#line 665 "xc.grammar.y"
{
        CURPARM->type = XC_TYPE_FILENAME;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Filename\n");
        }
    }
break;
case 60:
#line 673 "xc.grammar.y"
{
	CURPARM->type = XC_TYPE_DB_ELEMENT;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Database Element: ");
            fflush(stderr);
        }
    }
break;
case 63:
#line 689 "xc.grammar.y"
{
        XcModifierParse(&CURPARM->modifier,XC_TYPE_INTEGER,zzvsp[0].cval);
        CURPARM->hasModifier = True;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Modifiers: %s",zzvsp[0].cval);
        }
    }
break;
case 65:
#line 704 "xc.grammar.y"
{
        XcModifierParse(&CURPARM->modifier,XC_TYPE_DOUBLE,zzvsp[0].cval);
        CURPARM->hasModifier = True;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Modifiers: %s",zzvsp[0].cval);
        }
    }
break;
case 67:
#line 719 "xc.grammar.y"
{
        XcModifierParse(&CURPARM->modifier,XC_TYPE_LOGICAL,zzvsp[0].cval);
        CURPARM->hasModifier = True;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Modifiers: %s",zzvsp[0].cval);
        }
    }
break;
case 69:
#line 734 "xc.grammar.y"
{
        XcModifierParse(&CURPARM->modifier,XC_TYPE_FILENAME,zzvsp[0].cval);
        CURPARM->hasModifier = True;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Modifiers: %s",zzvsp[0].cval);
        }
    }
break;
case 70:
#line 748 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_RASTER;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Raster [Status = ");
            fflush(stderr);
        }
    }
break;
case 72:
#line 759 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_ASCII_DLG;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"ASCII DLG [Status = ");
            fflush(stderr);
        }
    }
break;
case 74:
#line 770 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_DLG;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Binary DLG [Status = ");
            fflush(stderr);
        }
    }
break;
case 76:
#line 781 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_ASCII_VECTOR;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"ASCII Vector [Status = ");
            fflush(stderr);
        }
    }
break;
case 78:
#line 792 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_VECTOR;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Vector [Status = ");
            fflush(stderr);
        }
    }
break;
case 80:
#line 803 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_SITES;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Sites [Status = ");
            fflush(stderr);
        }
    }
break;
case 82:
#line 814 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_REGION;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Region [Status = ");
            fflush(stderr);
        }
    }
break;
case 84:
#line 825 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_ICON;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Icon [Status = ");
            fflush(stderr);
        }
    }
break;
case 86:
#line 836 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_LABEL;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Label [Status = ");
            fflush(stderr);
        }
    }
break;
case 88:
#line 847 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_IMAGE_GROUP;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Imagery Group [Status = ");
            fflush(stderr);
        }
    }
break;
case 90:
#line 858 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_IMAGE_SUBGROUP;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Imagery Sub Group [Status = ");
            fflush(stderr);
        }
    }
break;
case 92:
#line 869 "xc.grammar.y"
{
        CURPARM->value.dbval.type = XC_DB_TYPE_USER_DEFINED;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"UserDefined\n");
        }
    }
break;
case 94:
#line 887 "xc.grammar.y"
{
        CURPARM->value.dbval.status = XC_DB_STATUS_NEW;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Default(New)]\n");
        }
    }
break;
case 95:
#line 895 "xc.grammar.y"
{
        CURPARM->value.dbval.status = XC_DB_STATUS_OLD;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Old]\n");
        }
    }
break;
case 97:
#line 904 "xc.grammar.y"
{
        CURPARM->value.dbval.status = XC_DB_STATUS_NEW;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"New]\n");
        }
    }
break;
case 101:
#line 926 "xc.grammar.y"
{
        CURPARM->selecttype = XC_SELECT_UNDETERMINED;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tSelectType: %s\n",zzvsp[0].cval);
        }
    }
break;
case 102:
#line 935 "xc.grammar.y"
{
        if ( CURPARM->type == XC_TYPE_UNKNOWN  &&
             CURPARM->selecttype == XC_SELECT_FALSE ) {
            zzerror("\"default\" defined before \"type\"");
        }

        switch(CURPARM->type) {
        case XC_TYPE_CHARACTER:
            CURPARM->def.cval = _XgStrDup(zzvsp[0].cval);
            CURPARM->value.cval = _XgStrDup(zzvsp[0].cval);
            break;
        case XC_TYPE_ENUMERATE:
            CURPARM->def.cval = _XgStrDup(
		_XcEnumStringToKey(CURPARM->modifier,zzvsp[0].cval));
            CURPARM->value.cval = _XgStrDup(CURPARM->def.cval);
            break;
        case XC_TYPE_INTEGER:
            CURPARM->def.ival = atoi(zzvsp[0].cval);
            CURPARM->value.ival = CURPARM->def.ival;
            break;
        case XC_TYPE_FLOAT:
#ifndef mips
            CURPARM->def.dval = strtod(zzvsp[0].cval,(char **)NULL);
#else
            CURPARM->def.dval = atof(zzvsp[0].cval);
#endif
            CURPARM->value.dval = CURPARM->def.dval;
            break;
        case XC_TYPE_DOUBLE:
#ifndef mips
            CURPARM->def.dval = strtod(zzvsp[0].cval,(char **)NULL);
#else
            CURPARM->def.dval = atof(zzvsp[0].cval);
#endif
            CURPARM->value.dval = CURPARM->def.dval;
            break;
        case XC_TYPE_LOGICAL:
            CURPARM->def.bval = (ISONSTR(zzvsp[0].cval) || ISTRUESTR(zzvsp[0].cval));
            CURPARM->value.bval = CURPARM->def.bval;
            break;
        case XC_TYPE_FILENAME:
            CURPARM->def.cval = _XgStrDup(zzvsp[0].cval);
            if ( CURPARM->isInput && access(CURPARM->def.cval,0) != 0 ) {
		sprintf(errorbuf,"invalid default input file name [%s]",zzvsp[0].cval);
		zzerror(errorbuf);
            }
            CURPARM->value.cval = _XgStrDup(zzvsp[0].cval);
            break;
        case XC_TYPE_DB_ELEMENT:
            CURPARM->def.dbval.desc = _XgStrDup(zzvsp[0].cval);
            CURPARM->value.dbval.desc = _XgStrDup(zzvsp[0].cval);
            break;
        }
        CURPARM->parmSet = True;

        if ( XCverbose  ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tDefault: %s\n",zzvsp[0].cval);
        }
    }
break;
case 103:
#line 997 "xc.grammar.y"
{
	CURPARM->help = _XgStrDup(zzvsp[0].cval);
        if ( XCverbose  ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tHelp: %s\n",zzvsp[0].cval);
        }
    }
break;
case 104:
#line 1006 "xc.grammar.y"
{
	CURPARM->helpFile.filename = _XgStrDup(zzvsp[0].cval);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tHelpFile: %s\n",zzvsp[0].cval);
        }
    }
break;
case 105:
#line 1015 "xc.grammar.y"
{
        if (CURPARM->precludes ) {
            XCRequireData *rPtr = &CURPARM->preclude;

            while ( rPtr->next ) {
                rPtr = rPtr->next;
            }
            rPtr->next = (XCRequireData *)XtMalloc(sizeof(XCRequireData));
            rPtr = rPtr->next;
            rPtr->isFlag = ISFLAGSTR(zzvsp[-2].cval);
            rPtr->name = _XgStrDup(zzvsp[0].cval);
            rPtr->next = NULL;
        } else {
            CURPARM->precludes = True;
            CURPARM->preclude.isFlag = ISFLAGSTR(zzvsp[-2].cval);
            CURPARM->preclude.name = _XgStrDup(zzvsp[0].cval);
            CURPARM->preclude.next = NULL;
        }

        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tPrecludes: %s -> %s\n",zzvsp[-2].cval,zzvsp[0].cval);
        }
    }
break;
case 106:
#line 1040 "xc.grammar.y"
{
        if (CURPARM->requirements ) {
            XCRequireData *rPtr = &CURPARM->requires;

            while ( rPtr->next ) {
                rPtr = rPtr->next;
            }
            rPtr->next = (XCRequireData *)XtMalloc(sizeof(XCRequireData));
            rPtr = rPtr->next;
            rPtr->isFlag = ISFLAGSTR(zzvsp[-2].cval);
            rPtr->name = _XgStrDup(zzvsp[0].cval);
            rPtr->next = NULL;
        } else {
            CURPARM->requirements = True;
            CURPARM->requires.isFlag = ISFLAGSTR(zzvsp[-2].cval);
            CURPARM->requires.name = _XgStrDup(zzvsp[0].cval);
            CURPARM->requires.next = NULL;
        }
        if ( XCverbose ) DoFprintf(dialogLevel,"\t\tRequires: %s -> %s\n",zzvsp[-2].cval,zzvsp[0].cval);
    }
break;
case 107:
#line 1062 "xc.grammar.y"
{
      if ( CURPARM->type == XC_TYPE_FILENAME ||
           CURPARM->type == XC_TYPE_DB_ELEMENT ||
           CURPARM->type == XC_TYPE_ENUMERATE ) {
          CURPARM->multiple = (ISONSTR(zzvsp[0].cval) || ISTRUESTR(zzvsp[0].cval));
      } else {
          sprintf(errorbuf,"[%s] can not be multi-valued",curPtr->name);
          zzerror(errorbuf);
      }

        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tMultiple: %s\n",zzvsp[0].cval);
        }
    }
break;
case 108:
#line 1079 "xc.grammar.y"
{
        CURPARM->isInput = (ISONSTR(zzvsp[0].cval) || ISTRUESTR(zzvsp[0].cval));
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tInput: %s\n",zzvsp[0].cval);
        }
    }
break;
case 109:
#line 1088 "xc.grammar.y"
{
        CURPARM->optional  = (ISONSTR(zzvsp[0].cval) || ISTRUESTR(zzvsp[0].cval));
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tOptional: %s\n",zzvsp[0].cval);
        }
    }
break;
case 110:
#line 1100 "xc.grammar.y"
{ zzval.cval = "flag"; }
break;
case 111:
#line 1102 "xc.grammar.y"
{ zzval.cval = "parameter"; }
break;
#line 1451 "y.tab.c"
    }
    zzssp -= zzm;
    zzstate = *zzssp;
    zzvsp -= zzm;
    zzm = zzlhs[zzn];
    if (zzstate == 0 && zzm == 0)
    {
#if ZZDEBUG
        if (zzdebug)
            printf("%sdebug: after reduction, shifting from state 0 to\
 state %d\n", ZZPREFIX, ZZFINAL);
#endif
        zzstate = ZZFINAL;
        *++zzssp = ZZFINAL;
        *++zzvsp = zzval;
        if (zzchar < 0)
        {
            if ((zzchar = zzlex()) < 0) zzchar = 0;
#if ZZDEBUG
            if (zzdebug)
            {
                zzs = 0;
                if (zzchar <= ZZMAXTOKEN) zzs = zzname[zzchar];
                if (!zzs) zzs = "illegal-symbol";
                printf("%sdebug: state %d, reading %d (%s)\n",
                        ZZPREFIX, ZZFINAL, zzchar, zzs);
            }
#endif
        }
        if (zzchar == 0) goto zzaccept;
        goto zzloop;
    }
    if ((zzn = zzgindex[zzm]) && (zzn += zzstate) >= 0 &&
            zzn <= ZZTABLESIZE && zzcheck[zzn] == zzstate)
        zzstate = zztable[zzn];
    else
        zzstate = zzdgoto[zzm];
#if ZZDEBUG
    if (zzdebug)
        printf("%sdebug: after reduction, shifting from state %d \
to state %d\n", ZZPREFIX, *zzssp, zzstate);
#endif
    if (zzssp >= zzss + zzstacksize - 1)
    {
        goto zzoverflow;
    }
    *++zzssp = zzstate;
    *++zzvsp = zzval;
    goto zzloop;
zzoverflow:
    zzerror("yacc stack overflow");
zzabort:
    return (1);
zzaccept:
    return (0);
}
