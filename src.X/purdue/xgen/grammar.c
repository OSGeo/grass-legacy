
# line 2 "grammar.y"

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


# line 50 "grammar.y"
typedef union  {
    char *  cval;
    int ival;
    double dval;
    Boolean bval;
} YYSTYPE;
# define String 257
# define Integer 258
# define Real 259
# define Logical 260
# define Environment 261
# define InitialShells 262
# define Menu 263
# define CommandBoard 264
# define Label 265
# define Message 266
# define List 267
# define PushButton 268
# define TextEntry 269
# define Table 270
# define Separator 271
# define Slider 272
# define Toggle 273
# define PullDown 274
# define Font 275
# define FixedFont 276
# define Background 277
# define Foreground 278
# define BackgroundPixmap 279
# define TopShadowColor 280
# define TopShadowPixmap 281
# define BottomShadowColor 282
# define BottomShadowPixmap 283
# define X 284
# define DX 285
# define Y 286
# define DY 287
# define Width 288
# define Height 289
# define MaxWidth 290
# define MaxHeight 291
# define Columns 292
# define Override 293
# define Popup 294
# define Popdown 295
# define Destroy 296
# define Exit 297
# define Help 298
# define Eval 299
# define RunForeground 300
# define RunBackground 301
# define InputFrom 302
# define CaptureOutput 303
# define UpdateFrom 304
# define Pane 305
# define Store 306
# define Highlight 307
# define GetEnv 308
# define Clear 309
# define CommandArg 310
# define TableArg 311
# define Set 312
# define Alignment 313
# define ListElement 314
# define ListType 315
# define VisibleItems 316
# define ScrollBar 317
# define ValueString 318
# define LabelPixmap 319
# define MaxLength 320
# define Minimum 321
# define Maximum 322
# define StartValue 323
# define SliderWidth 324
# define SliderHeight 325
# define Orientation 326
# define DecimalPoints 327
# define EntryFont 328
# define Rows 329
# define RowsDisplayed 330
# define ColumnsDisplayed 331
# define ColumnHeadings 332
# define RowHeadings 333
# define RowValue 334
# define RowHeight 335
# define ColumnWidth 336
# define TitleString 337
# define ToggleType 338
# define SeparatorType 339
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 757 "grammar.y"

short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 122
# define YYLAST 281
short yyact[]={

  46, 161, 128, 120, 121, 123, 124, 125, 126, 108,
  17, 148,  30,  47,  48,  49,  50,  51,  52,  53,
  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
  64,  65,  66,  67,  68,  69,  70,  71,  72,  73,
  74,  75,  76,  77,  78,  79,  80,  81,  82,  83,
 112,  84,  85,  86,  87,  88,  89,  90,  91,  92,
  93,  94,  95,  96,  97,  98,  99, 100, 101, 102,
 103, 104, 105, 106, 107, 109, 110, 111,   6,   7,
   8,   9,  10,  11,  12,  13,  14, 139, 140, 141,
 142, 143, 144, 145, 146, 147, 138,  39,  38,  37,
  36,  35,  34,  33,  32,  31, 134, 113, 162, 157,
 136, 135, 116, 115,  27,  26,  41,  25,  24,  23,
  22,  21,  20,  19,  43, 159, 133,  29,  16,   5,
   4, 118,  42, 122,  18, 156, 152, 153, 131, 117,
 120, 121,  40,  15,  28,   3,   2,  44, 137, 119,
  45,   1,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0, 114,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0, 139, 140, 141, 142, 143,
 144, 145, 146, 147, 138, 139, 140, 141, 142, 143,
 144, 145, 146, 147, 138, 150, 149, 130, 132, 129,
 127,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0, 151, 160,   0,   0,   0,
 155, 150, 149, 158,   0,   0,   0,   0,   0,   0,
 154 };
short yypact[]={

-1000,-1000,-197,-251,-197,-1000,  65,  64,  63,  62,
  61,  60,  59,  57,  56,-251,-1000,-245,-1000,-152,
-153,-154,-155,-156,-157,-158,-159,-160,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-262, -16,-262,-1000,  55,  54,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-260,-1000,-252,-252,-123,-1000,-245,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-262,  86,-178, -30,-1000,-1000,-245,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-245,-262,-1000,  50,-262,-1000,  85,-178,
 -40,  49,-1000 };
short yypgo[]={

   0, 151, 127, 133, 150, 149, 148, 147, 146, 145,
 143, 128, 142, 116, 139, 131, 138, 106, 111, 110,
 137, 136, 135, 132, 124, 130, 129 };
short yyr1[]={

   0,   8,   1,  10,  10,  12,  11,  14,  14,  16,
  15,   5,   5,  17,  17,  17,  17,  20,  18,  21,
  22,  19,   6,   6,   6,   6,   6,   6,   6,   6,
   6,  13,  13,  23,  23,   2,  24,  24,   4,   7,
   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,
   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,
   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,
   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,
   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,
   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,
   7,   7,   7,   7,   7,   9,   9,  25,  25,  26,
  26,  26,  26,  26,  26,  26,  26,  26,   3,   3,
   3,   3 };
short yyr2[]={

   0,   0,   3,   2,   1,   0,   7,   2,   1,   0,
   7,   1,   1,   2,   2,   1,   1,   0,   5,   0,
   0,   9,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   0,   1,   2,   1,   1,   3,   3,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   0,   1,   2,   1,   3,
   3,   3,   3,   3,   3,   3,   3,   3,   1,   1,
   1,   1 };
short yychk[]={

-1000,  -1,  -8,  -9, -25, -26, 275, 276, 277, 278,
 279, 280, 281, 282, 283, -10, -11, 261, -26,  58,
  58,  58,  58,  58,  58,  58,  58,  58, -11,  -2,
 257, 257, 257, 257, 257, 257, 257, 257, 257, 257,
 -12, -13, -23, -24,  -7,  -4, 262, 275, 276, 277,
 278, 279, 280, 281, 282, 283, 284, 285, 286, 287,
 288, 289, 290, 291, 292, 293, 294, 295, 296, 297,
 298, 299, 300, 301, 302, 303, 304, 305, 306, 307,
 308, 309, 310, 311, 313, 314, 315, 316, 317, 318,
 319, 320, 321, 322, 323, 324, 325, 326, 327, 328,
 329, 330, 331, 332, 333, 334, 335, 336, 271, 337,
 338, 339, 312, 123, -24,  58,  58, -14, -15,  -5,
 263, 264,  -3, 257, 258, 259, 260,  -3, 125, -15,
  -2, -16, -13,  40, -17, -18, -19,  -6, 274, 265,
 266, 267, 268, 269, 270, 271, 272, 273,  41, -18,
 -19,  -2, -21, -20,  -2, -13, -22,  59, -13,  40,
 -17,  41,  59 };
short yydef[]={

   1,  -2, 105,   0, 106, 108,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   2,   4,   0, 107,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   3,   5,
  35, 109, 110, 111, 112, 113, 114, 115, 116, 117,
  31,   0,  32,  34,   0,   0,  39,  40,  41,  42,
  43,  44,  45,  46,  47,  48,  49,  50,  51,  52,
  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,
  63,  64,  65,  66,  67,  68,  69,  70,  71,  72,
  73,  74,  75,  76,  77,  78,  79,  80,  81,  82,
  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,
  93,  94,  95,  96,  97,  98,  99, 100, 101, 102,
 103, 104,  38,   0,  33,   0,   0,   0,   8,   0,
  11,  12,  36, 118, 119, 120, 121,  37,   6,   7,
   9,  31,   0,   0,   0,  15,  16,   0,  19,  22,
  23,  24,  25,  26,  27,  28,  29,  30,  10,  13,
  14,  17,   0,  31,  20,   0,  31,  18,   0,   0,
   0,   0,  21 };
#ifndef lint
static	char yaccpar_sccsid[] = "@(#)yaccpar 1.6 88/02/08 SMI"; /* from UCB 4.1 83/02/11 */
#endif

#
# define YYFLAG -1000
# define YYERROR goto yyerrlab
# define YYACCEPT return(0)
# define YYABORT return(1)

/*	parser for yacc output	*/

#ifdef YYDEBUG
int yydebug = 0; /* 1 for debugging */
#endif
YYSTYPE yyv[YYMAXDEPTH]; /* where the values are stored */
int yychar = -1; /* current input token number */
int yynerrs = 0;  /* number of errors */
short yyerrflag = 0;  /* error recovery flag */

yyparse() {

	short yys[YYMAXDEPTH];
	short yyj, yym;
	register YYSTYPE *yypvt;
	register short yystate, *yyps, yyn;
	register YYSTYPE *yypv;
	register short *yyxi;

	yystate = 0;
	yychar = -1;
	yynerrs = 0;
	yyerrflag = 0;
	yyps= &yys[-1];
	yypv= &yyv[-1];

 yystack:    /* put a state and value onto the stack */

#ifdef YYDEBUG
	if( yydebug  ) printf( "state %d, char 0%o\n", yystate, yychar );
#endif
		if( ++yyps>= &yys[YYMAXDEPTH] ) { yyerror( "yacc stack overflow" ); return(1); }
		*yyps = yystate;
		++yypv;
		*yypv = yyval;

 yynewstate:

	yyn = yypact[yystate];

	if( yyn<= YYFLAG ) goto yydefault; /* simple state */

	if( yychar<0 ) if( (yychar=yylex())<0 ) yychar=0;
	if( (yyn += yychar)<0 || yyn >= YYLAST ) goto yydefault;

	if( yychk[ yyn=yyact[ yyn ] ] == yychar ){ /* valid shift */
		yychar = -1;
		yyval = yylval;
		yystate = yyn;
		if( yyerrflag > 0 ) --yyerrflag;
		goto yystack;
		}

 yydefault:
	/* default state action */

	if( (yyn=yydef[yystate]) == -2 ) {
		if( yychar<0 ) if( (yychar=yylex())<0 ) yychar = 0;
		/* look through exception table */

		for( yyxi=yyexca; (*yyxi!= (-1)) || (yyxi[1]!=yystate) ; yyxi += 2 ) ; /* VOID */

		while( *(yyxi+=2) >= 0 ){
			if( *yyxi == yychar ) break;
			}
		if( (yyn = yyxi[1]) < 0 ) return(0);   /* accept */
		}

	if( yyn == 0 ){ /* error */
		/* error ... attempt to resume parsing */

		switch( yyerrflag ){

		case 0:   /* brand new error */

			yyerror( "syntax error" );
		yyerrlab:
			++yynerrs;

		case 1:
		case 2: /* incompletely recovered error ... try again */

			yyerrflag = 3;

			/* find a state where "error" is a legal shift action */

			while ( yyps >= yys ) {
			   yyn = yypact[*yyps] + YYERRCODE;
			   if( yyn>= 0 && yyn < YYLAST && yychk[yyact[yyn]] == YYERRCODE ){
			      yystate = yyact[yyn];  /* simulate a shift of "error" */
			      goto yystack;
			      }
			   yyn = yypact[*yyps];

			   /* the current yyps has no shift onn "error", pop stack */

#ifdef YYDEBUG
			   if( yydebug ) printf( "error recovery pops state %d, uncovers %d\n", *yyps, yyps[-1] );
#endif
			   --yyps;
			   --yypv;
			   }

			/* there is no state on the stack with an error shift ... abort */

	yyabort:
			return(1);


		case 3:  /* no shift yet; clobber input char */

#ifdef YYDEBUG
			if( yydebug ) printf( "error recovery discards char %d\n", yychar );
#endif

			if( yychar == 0 ) goto yyabort; /* don't discard EOF, quit */
			yychar = -1;
			goto yynewstate;   /* try again in the same state */

			}

		}

	/* reduction by production yyn */

#ifdef YYDEBUG
		if( yydebug ) printf("reduce %d\n",yyn);
#endif
		yyps -= yyr2[yyn];
		yypvt = yypv;
		yypv -= yyr2[yyn];
		yyval = yypv[1];
		yym=yyn;
			/* consult goto table to find next state */
		yyn = yyr1[yyn];
		yyj = yypgo[yyn] + *yyps + 1;
		if( yyj>=YYLAST || yychk[ yystate = yyact[yyj] ] != -yyn ) yystate = yyact[yypgo[yyn]];
		switch(yym){
			
case 1:
# line 163 "grammar.y"
{ 
			xgenGD.screen = DefaultScreen(xgenGD.display);
			xgenGD.scrptr = XtScreen(xgenGD.applShell);
			xgenGD.cmap = DefaultColormap(xgenGD.display,xgenGD.screen);
		} break;
case 2:
# line 170 "grammar.y"
{
			return(fatalError);
		} break;
case 5:
# line 183 "grammar.y"
{
			if ( !UniqueEnvName(yypvt[-0].cval) ) {
				char errorbuf[80];

				sprintf(errorbuf,"Non-unique environment name \"%s\"",yypvt[-0].cval);
				yyerror(errorbuf,False);
			}
			curEnv = AllocEnviron(); 
			curEnv->name = yypvt[-0].cval;
			AddEnviron(curEnv);
			tailEnv = curEnv;
			resourceType = ENVIRONMENT;
			itemType = ENV;
		} break;
case 9:
# line 211 "grammar.y"
{
			if ( !UniqueShellName(yypvt[-0].cval) ) {
				char errorbuf[80];

				sprintf(errorbuf,"Non-unique shell name \"%s\"",yypvt[-0].cval);
				yyerror(errorbuf,False);
			}
			curShell = AllocShell(); 
			curShell->type = yypvt[-1].ival;
			curShell->name = yypvt[-0].cval;
			AddShell(curShell,tailEnv);
			tailShell = curShell;
			resourceType = SHELL;
			itemType = SHL;
		} break;
case 11:
# line 231 "grammar.y"
{ yyval.ival = MENU; shellType = MU; } break;
case 12:
# line 233 "grammar.y"
{ yyval.ival = COMMANDBOARD; shellType = CO; } break;
case 17:
# line 246 "grammar.y"
{
			curObject = AllocObject(); 
			curObject->type = yypvt[-1].ival;
			curObject->name = SaveString(yypvt[-0].cval);
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
		} break;
case 19:
# line 277 "grammar.y"
{
			objectType = PU;
		} break;
case 20:
# line 281 "grammar.y"
{
			doingPulldown = True;
			curObject = AllocObject(); 
			curObject->type = PULLDOWN;
			curObject->name = SaveString(yypvt[-0].cval);
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
		} break;
case 21:
# line 301 "grammar.y"
{
			  doingPulldown = False;
		} break;
case 22:
# line 308 "grammar.y"
{ yyval.ival = LABEL; objectType = LA; } break;
case 23:
# line 310 "grammar.y"
{ yyval.ival = MESSAGE; objectType = ME; } break;
case 24:
# line 312 "grammar.y"
{ yyval.ival = LIST; objectType = LI; } break;
case 25:
# line 314 "grammar.y"
{ yyval.ival = PUSHBUTTON; objectType = PB; } break;
case 26:
# line 316 "grammar.y"
{ yyval.ival = TEXTENTRY; objectType = TE; } break;
case 27:
# line 318 "grammar.y"
{ yyval.ival = TABLE; objectType = TA; } break;
case 28:
# line 320 "grammar.y"
{ yyval.ival = SEPARATOR; objectType = SE; } break;
case 29:
# line 322 "grammar.y"
{ yyval.ival = SLIDER; objectType = SL; } break;
case 30:
# line 324 "grammar.y"
{ yyval.ival = TOGGLE; objectType = TO; } break;
case 36:
# line 343 "grammar.y"
{
			char tabs[20];
			curRes = AllocResource(); 
			if ( rindex(yypvt[-0].cval,'$')) 
				curRes->variable = True;
			if ( !curRes->variable) {
			    switch(ResourceDataType(yypvt[-2].ival)) {
				    case INTEGER:
					    if (!CheckType(yypvt[-0].cval,Integer)) yyerror(errorbuf);
					    break;
				    case REAL:
					    if (!CheckType(yypvt[-0].cval,Real)) yyerror(errorbuf);
					    break;
				    case BOOLEAN:
					    if (!CheckType(yypvt[-0].cval,OnOff)) yyerror(errorbuf);
					    break;
				}
			}
			switch(resourceType) {
				case ENVIRONMENT:
					if ( !(ResourceValid(yypvt[-2].ival) & itemType) ) {
						char errorbuf[80];

						sprintf(errorbuf,"invalid environment resource: %s",
							ResourceString(yypvt[-2].ival));
						XgenFatalError("parser",errorbuf);
					}
					AddResource(curRes,curEnv,resourceType,yypvt[-0].cval,yypvt[-2].ival);
					break;
				case SHELL:
					if ( !(ResourceValid(yypvt[-2].ival) & shellType) ) {
						char errorbuf[80];

						sprintf(errorbuf,"invalid shell resource: %s",
							ResourceString(yypvt[-2].ival));
						XgenFatalError("parser",errorbuf);
					}
					AddResource(curRes,curShell,resourceType,yypvt[-0].cval,yypvt[-2].ival);
					break;
				case OBJECT:
					if ( !(ResourceValid(yypvt[-2].ival) & objectType) ) {
						char errorbuf[80];

						sprintf(errorbuf,"invalid object resource: %s",
							ResourceString(yypvt[-2].ival));
						XgenFatalError("parser",errorbuf);
					}
					AddResource(curRes,curObject,resourceType,yypvt[-0].cval,yypvt[-2].ival);
					break;
			}
			tailRes = curRes;
		} break;
case 37:
# line 396 "grammar.y"
{
			char buf[80];

			/*
			 * go ahead and set the resource to x=y. Will check later 
			 * for modifiers that need to wait til runtime to get a value.
			 */
			sprintf(buf,"%s=%s",yypvt[-2].cval,yypvt[-0].cval);
			curRes = AllocResource(); 
			switch (resourceType) {
				case ENVIRONMENT:
			        AddResource(curRes,curEnv,ENVIRONMENT,SaveString(buf),Set);
				    break;
				case SHELL:
			        AddResource(curRes,curShell,SHELL,SaveString(buf),Set);
				    break;
				case OBJECT:
			        AddResource(curRes,curObject,OBJECT,SaveString(buf),Set);
				    break;
			}
			tailRes = curRes;
		} break;
case 38:
# line 421 "grammar.y"
{ yyval.cval = yypvt[-0].cval; } break;
case 39:
# line 425 "grammar.y"
{ yyval.ival = InitialShells; } break;
case 40:
# line 427 "grammar.y"
{ yyval.ival =  Font ; } break;
case 41:
# line 429 "grammar.y"
{ yyval.ival =  FixedFont ; } break;
case 42:
# line 431 "grammar.y"
{ yyval.ival =  Background ; } break;
case 43:
# line 433 "grammar.y"
{ yyval.ival =  Foreground ; } break;
case 44:
# line 435 "grammar.y"
{ yyval.ival =  BackgroundPixmap ; } break;
case 45:
# line 437 "grammar.y"
{ yyval.ival =  TopShadowColor ; } break;
case 46:
# line 439 "grammar.y"
{ yyval.ival =  TopShadowPixmap ; } break;
case 47:
# line 441 "grammar.y"
{ yyval.ival =  BottomShadowColor ; } break;
case 48:
# line 443 "grammar.y"
{ yyval.ival =  BottomShadowPixmap ; } break;
case 49:
# line 445 "grammar.y"
{ yyval.ival =  X ; } break;
case 50:
# line 447 "grammar.y"
{ yyval.ival =  DX ; } break;
case 51:
# line 449 "grammar.y"
{ yyval.ival =  Y ; } break;
case 52:
# line 451 "grammar.y"
{ yyval.ival =  DY ; } break;
case 53:
# line 453 "grammar.y"
{ yyval.ival =  Width ; } break;
case 54:
# line 455 "grammar.y"
{ yyval.ival =  Height ; } break;
case 55:
# line 457 "grammar.y"
{ yyval.ival =  MaxWidth ; } break;
case 56:
# line 459 "grammar.y"
{ yyval.ival =  MaxHeight ; } break;
case 57:
# line 461 "grammar.y"
{ yyval.ival =  Columns ; } break;
case 58:
# line 463 "grammar.y"
{ yyval.ival =  Override ; } break;
case 59:
# line 465 "grammar.y"
{ yyval.ival =  Popup ; } break;
case 60:
# line 467 "grammar.y"
{ yyval.ival =  Popdown ; } break;
case 61:
# line 469 "grammar.y"
{ yyval.ival =  Destroy ; } break;
case 62:
# line 471 "grammar.y"
{ yyval.ival =  Exit ; } break;
case 63:
# line 473 "grammar.y"
{ yyval.ival =  Help ; } break;
case 64:
# line 475 "grammar.y"
{ yyval.ival =  Eval ; } break;
case 65:
# line 477 "grammar.y"
{ yyval.ival =  RunForeground ; } break;
case 66:
# line 479 "grammar.y"
{ yyval.ival =  RunBackground ; } break;
case 67:
# line 481 "grammar.y"
{ yyval.ival =  InputFrom ; } break;
case 68:
# line 483 "grammar.y"
{ yyval.ival =  CaptureOutput ; } break;
case 69:
# line 485 "grammar.y"
{ yyval.ival =  UpdateFrom ; } break;
case 70:
# line 487 "grammar.y"
{ yyval.ival =  Pane ; } break;
case 71:
# line 489 "grammar.y"
{ yyval.ival =  Store ; } break;
case 72:
# line 491 "grammar.y"
{ yyval.ival =  Highlight ; } break;
case 73:
# line 493 "grammar.y"
{ yyval.ival =  GetEnv ; } break;
case 74:
# line 495 "grammar.y"
{ yyval.ival =  Clear ; } break;
case 75:
# line 497 "grammar.y"
{ yyval.ival =  CommandArg ; } break;
case 76:
# line 499 "grammar.y"
{ yyval.ival =  TableArg ; } break;
case 77:
# line 501 "grammar.y"
{ yyval.ival =  Alignment ; } break;
case 78:
# line 503 "grammar.y"
{ yyval.ival =  ListElement ; } break;
case 79:
# line 505 "grammar.y"
{ yyval.ival =  ListType ; } break;
case 80:
# line 507 "grammar.y"
{ yyval.ival =  VisibleItems ; } break;
case 81:
# line 509 "grammar.y"
{ yyval.ival =  ScrollBar ; } break;
case 82:
# line 511 "grammar.y"
{ yyval.ival =  ValueString ; } break;
case 83:
# line 513 "grammar.y"
{ yyval.ival =  LabelPixmap ; } break;
case 84:
# line 515 "grammar.y"
{ yyval.ival =  MaxLength ; } break;
case 85:
# line 517 "grammar.y"
{ yyval.ival =  Minimum ; } break;
case 86:
# line 519 "grammar.y"
{ yyval.ival =  Maximum ; } break;
case 87:
# line 521 "grammar.y"
{ yyval.ival =  StartValue ; } break;
case 88:
# line 523 "grammar.y"
{ yyval.ival =  SliderWidth ; } break;
case 89:
# line 525 "grammar.y"
{ yyval.ival =  SliderHeight ; } break;
case 90:
# line 527 "grammar.y"
{ yyval.ival =  Orientation ; } break;
case 91:
# line 529 "grammar.y"
{ yyval.ival =  DecimalPoints ; } break;
case 92:
# line 531 "grammar.y"
{ yyval.ival =  EntryFont ; } break;
case 93:
# line 533 "grammar.y"
{ yyval.ival =  Rows ; } break;
case 94:
# line 535 "grammar.y"
{ yyval.ival =  RowsDisplayed ; } break;
case 95:
# line 537 "grammar.y"
{ yyval.ival =  ColumnsDisplayed ; } break;
case 96:
# line 539 "grammar.y"
{ yyval.ival =  ColumnHeadings ; } break;
case 97:
# line 541 "grammar.y"
{ yyval.ival =  RowHeadings ; } break;
case 98:
# line 543 "grammar.y"
{ yyval.ival =  RowValue ; } break;
case 99:
# line 545 "grammar.y"
{ yyval.ival =  RowHeight ; } break;
case 100:
# line 547 "grammar.y"
{ yyval.ival =  ColumnWidth ; } break;
case 101:
# line 549 "grammar.y"
{ yyval.ival =  Separator ; } break;
case 102:
# line 551 "grammar.y"
{ yyval.ival =  TitleString ; } break;
case 103:
# line 553 "grammar.y"
{ yyval.ival =  ToggleType ; } break;
case 104:
# line 555 "grammar.y"
{ yyval.ival =  SeparatorType ; } break;
case 109:
# line 568 "grammar.y"
{
			xgenGD.g_font = SaveString(yypvt[-0].cval);
			if ( (xgenGD.g_fs = XLoadQueryFont(xgenGD.display,xgenGD.g_font)) == 0 ) {
				char errorbuf[80];

				sprintf(errorbuf,"font %s not found",xgenGD.g_font);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		} break;
case 110:
# line 579 "grammar.y"
{
			xgenGD.g_ffont = SaveString(yypvt[-0].cval);
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
		} break;
case 111:
# line 598 "grammar.y"
{
			xgenGD.g_bg = SaveString(yypvt[-0].cval);
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
		} break;
case 112:
# line 617 "grammar.y"
{
			xgenGD.g_fg = SaveString(yypvt[-0].cval);
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
		} break;
case 113:
# line 636 "grammar.y"
{ 
			Pixel tfg = ((xgenGD.g_fgs.pixel) ? 
				(xgenGD.g_fgs.pixel):BlackPixel(xgenGD.display,xgenGD.screen));
			Pixel tbg = ((xgenGD.g_bgs.pixel) ? 
				(xgenGD.g_bgs.pixel):WhitePixel(xgenGD.display,xgenGD.screen));

			xgenGD.g_bgpix = SaveString(yypvt[-0].cval);
			xgenGD.g_bgpm = XmGetPixmap(xgenGD.scrptr, xgenGD.g_bgpix, tfg, tbg);
			if ( xgenGD.g_bgpm == XmUNSPECIFIED_PIXMAP ) {
				char errorbuf[80];

				sprintf(errorbuf,
					"invalid background pixmap %s",xgenGD.g_bgpix);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		} break;
case 114:
# line 654 "grammar.y"
{ 
			xgenGD.g_ts = SaveString(yypvt[-0].cval);
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
		} break;
case 115:
# line 674 "grammar.y"
{ 
			Pixel tfg = ((xgenGD.g_fgs.pixel) ? 
				(xgenGD.g_fgs.pixel):BlackPixel(xgenGD.display,xgenGD.screen));
			Pixel tbg = ((xgenGD.g_bgs.pixel) ? 
				(xgenGD.g_bgs.pixel):WhitePixel(xgenGD.display,xgenGD.screen));

			xgenGD.g_tspix = SaveString(yypvt[-0].cval);
			xgenGD.g_tspm = XmGetPixmap(xgenGD.scrptr, xgenGD.g_tspix, tfg, tbg);
			if ( xgenGD.g_tspm == XmUNSPECIFIED_PIXMAP ) {
				char errorbuf[80];

				sprintf(errorbuf,
					"invalid top shadow pixmap %s",xgenGD.g_tspix);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		} break;
case 116:
# line 692 "grammar.y"
{ 
			xgenGD.g_bs = SaveString(yypvt[-0].cval);
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
		} break;
case 117:
# line 712 "grammar.y"
{ 
			Pixel tfg = ((xgenGD.g_fgs.pixel) ? 
				(xgenGD.g_fgs.pixel):BlackPixel(xgenGD.display,xgenGD.screen));
			Pixel tbg = ((xgenGD.g_bgs.pixel) ? 
				(xgenGD.g_bgs.pixel):WhitePixel(xgenGD.display,xgenGD.screen));

			xgenGD.g_bspix = SaveString(yypvt[-0].cval);
			xgenGD.g_bspm = XmGetPixmap(xgenGD.scrptr, xgenGD.g_bspix, tfg, tbg);
			if ( xgenGD.g_bspm == XmUNSPECIFIED_PIXMAP ) {
				char errorbuf[80];

				sprintf(errorbuf,
					"invalid bottom shadow pixmap %s",xgenGD.g_bspix);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		} break;
case 118:
# line 733 "grammar.y"
{
			yyval.cval = SaveString(yypvt[-0].cval);
		} break;
case 119:
# line 737 "grammar.y"
{
			char buf[80];

			sprintf(buf,"%d",yypvt[-0].ival);
			yyval.cval = SaveString(buf);
		} break;
case 120:
# line 744 "grammar.y"
{
			char buf[80];

			sprintf(buf,"%f",yypvt[-0].dval);
			yyval.cval = SaveString(buf);
		} break;
case 121:
# line 751 "grammar.y"
{
			if ( yypvt[-0].bval == True ) yyval.cval = SaveString("True");
			else yyval.cval = SaveString("False");
		} break;
		}
		goto yystack;  /* stack new state and value */

	}
