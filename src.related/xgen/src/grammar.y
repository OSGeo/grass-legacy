/*********************************************************************
   grammar.y    - the Xgen parser grammar (yacc)
 *********************************************************************/
/*******************************************************************************
Xgen was developed by Kurt Buehler, while at the Center for Advanced Decision
Support for Water and Environmental Systems (CADSWES), University of Colorado
at Boulder and at the Indiana Water Resources Research Center (IWRRC),
Purdue University for the U.S. Army Construction Engineering Research
Laboratory in support of the Geographical Resources Analysis Support
System (GRASS) software. The example scripts were developed by Ms. Christine
Poulsen of USA-CERL, much thanks goes to her for her work.

Permission to use, copy, modify and distribute without charge this software,
documentation, etc. is granted, provided that this comment is retained,
and that the names of Kurt Buehler, Christine Poulsen, CADSWES, IWRRC,
the University of Colorado at Boulder, Purdue University, or USA-CERL are not
used in advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

The author disclaims all warranties with regard to this software, including
all implied warranties of merchantability and fitness, in no event shall
the author be liable for any special, indirect or consequential damages or
any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this
software.
*******************************************************************************/
%{

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

%}

%start Program

%union {
    char *  cval;
    int ival;
    double dval;
    Boolean bval;
};

%token <cval> String
%token <ival> Integer
%token <dval> Real
%token <bval> Logical

/* Environment */
%token  Environment

%token  InitialShells

/* Shells */
%token  Menu
%token  CommandBoard

/* Objects */
%token  Label
%token  Message
%token  List
%token  PushButton
%token  TextEntry
%token  Table
%token  Separator
%token  Slider
%token  Toggle
%token  PullDown
%token  MultiLine

/* Resources */
/* Globals (can be globally set) */
%token  Font
%token  FixedFont
%token  EditorFont
%token  Background
%token  Foreground
%token  BackgroundPixmap
%token  TopShadowColor
%token  TopShadowPixmap
%token  BottomShadowColor
%token  BottomShadowPixmap

/* Nonglobals */
%token  X
%token  DX
%token  Y
%token  DY
%token  Width
%token  Height
%token  ForceSize
%token  MaxWidth
%token  MaxHeight
%token  Columns
%token  Override
%token  Functions
%token  Decorations
%token  Popup
%token  Popdown
%token  Destroy
%token  Exit
%token  Help
%token  Eval
%token  UpdateObject
%token  PostNotice
%token  RunForeground
%token  RunBackground
%token  CommandShell
%token  InteractiveShell
%token  InputFrom
%token  CaptureOutput
%token  NotifyComplete
%token  UpdateFrom
%token  Pane
%token  PaneType
%token  Store
%token  Highlight
%token  GetEnv
%token  Clear
%token  CommandArg
%token  TableArg
%token  <cval> Set
%token  Alignment
%token  ListElement
%token  ListSeparator
%token  ListType
%token  VisibleItems
%token  ScrollBar
%token  ValueString
%token  Scrolled
%token  LabelPixmap
%token  MaxLength
%token  Minimum
%token  Maximum
%token  StartValue
%token  SliderWidth
%token  SliderHeight
%token  Orientation
%token  DecimalPoints
%token  EntryFont
%token  Rows
%token  RowsDisplayed
%token  FixedRows
%token  FixedColumns
%token  ColumnsDisplayed
%token  ColumnHeadings
%token  RowHeadings
%token  RowValue
%token  TableValue
%token  RowHeight
%token  ColumnWidth
%token  Newline
%token  TitleString
%token  ToggleType
%token  ToggleState
%token  SeparatorType
%token  Sensitive
%token  Insensitive


%type <cval>	Name, Value, String, SetResourceType
%type <ival>	ShellType, ObjectType, Integer, ResourceType
%type <bval>	Logical
%type <dval>	Real

%%

Program
    : 
		{ 
			xgenGD.screen = DefaultScreen(xgenGD.display);
			xgenGD.scrptr = XtScreen(xgenGD.applShell);
			xgenGD.cmap = DefaultColormap(xgenGD.display,xgenGD.screen);
		}
      OptGlobals 
      EnvironList
		{
			return(fatalError);
		}
    ;

EnvironList
    : EnvironList Environ
    | Environ
    ;

Environ
    : Environment 
      Name 
		{
			if ( !UniqueEnvName($2) ) {
				char errorbuf[80];

				sprintf(errorbuf,"Non-unique environment name \"%s\"",$2);
				yyerror(errorbuf,False);
			}
			curEnv = AllocEnviron(); 
			curEnv->name = $2;
			AddEnviron(curEnv);
			tailEnv = curEnv;
			resourceType = ENVIRONMENT;
			itemType = ENV;
		}
      OptResourceList 
      '{' 
      ShellList 
      '}'
    ;

ShellList
    : ShellList Shell
    | Shell
    ;

Shell
    : ShellType 
	  Name 
		{
			if ( !UniqueShellName($2) ) {
				char errorbuf[80];

				sprintf(errorbuf,"Non-unique shell name \"%s\"",$2);
				yyerror(errorbuf,False);
			}
			curShell = AllocShell(); 
			curShell->type = $1;
			curShell->name = $2;
			AddShell(curShell,tailEnv);
			tailShell = curShell;
			resourceType = SHELL;
			itemType = SHL;
		}
	  OptResourceList '(' ObjectList ')'
    ;

ShellType
    : Menu
		{ $$ = MENU; shellType = MU; }
    | CommandBoard
		{ $$ = COMMANDBOARD; shellType = CO; }
    ;

ObjectList
    : ObjectList Object
    | ObjectList PulldownObject
    | Object
    | PulldownObject
    ;

Object
    : ObjectType 
	  Name 
		{
			curObject = AllocObject(); 
			curObject->type = $1;
			curObject->name = SaveString($2);
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
	  OptResourceList ';'
    ;

PulldownObject
    : PullDown 
		{
			objectType = PU;
		}
	  Name 
		{
			doingPulldown = True;
			curObject = AllocObject(); 
			curObject->type = PULLDOWN;
			curObject->name = SaveString($3);
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
	  OptResourceList 
	  '(' ObjectList ')' ';' 
		{
			  doingPulldown = False;
		}
    ;

ObjectType
    : Label 
		{ $$ = LABEL; objectType = LA; }
    | Message 
		{ $$ = MESSAGE; objectType = ME; }
	| List 
		{ $$ = LIST; objectType = LI; }
    | PushButton 
		{ $$ = PUSHBUTTON; objectType = PB; }
    | TextEntry 
		{ $$ = TEXTENTRY; objectType = TE; }
    | MultiLine 
		{ $$ = MULTILINE; objectType = ML; }
    | Table 
		{ $$ = TABLE; objectType = TA; }
    | Separator 
		{ $$ = SEPARATOR; objectType = SE; }
    | Slider 
		{ $$ = SLIDER; objectType = SL; }
    | Toggle 
		{ $$ = TOGGLE; objectType = TO; }
    ;

OptResourceList
    : /* none, its optional... */
    | ResourceList
    ;

ResourceList
    : ResourceList Resource
    | Resource
    ;

Name
    : String
    ;

Resource
    : ResourceType ':' Value
		{
			curRes = AllocResource(); 
			if ( rindex($3,'$')) 
				curRes->variable = True;
			if ( !curRes->variable) {
			    switch(ResourceDataType($1)) {
				    case INTEGER:
					    if (!CheckType($3,Int)) yyerror(errorbuf);
					    break;
				    case REAL:
					    if (!CheckType($3,Real)) yyerror(errorbuf);
					    break;
				    case BOOLEAN:
					    if (!CheckType($3,OnOff)) yyerror(errorbuf);
					    break;
				}
			}
			switch(resourceType) {
				case ENVIRONMENT:
					if ( !(ResourceValid($1) & itemType) ) {
						char errorbuf[80];

						sprintf(errorbuf,"invalid environment resource: %s",
							ResourceString($1));
						XgenFatalError("parser",errorbuf);
					}
					AddResource(curRes,(char *)curEnv,resourceType,$3,$<ival>1);
					break;
				case SHELL:
					if ( !(ResourceValid($1) & shellType) ) {
						char errorbuf[80];

						sprintf(errorbuf,"invalid shell resource: %s",
							ResourceString($1));
						XgenFatalError("parser",errorbuf);
					}
					AddResource(curRes,(char *)curShell,resourceType,$3,$<ival>1);
					break;
				case OBJECT:
					if ( !(ResourceValid($1) & objectType) ) {
						char errorbuf[80];

						sprintf(errorbuf,"invalid object resource: %s",
							ResourceString($1));
						XgenFatalError("parser",errorbuf);
					}
					AddResource(curRes,(char *)curObject,resourceType,$3,$<ival>1);
					break;
			}
			tailRes = curRes;
		}
    | SetResourceType ':' Value
		{
			char buf[80];

			/*
			 * go ahead and set the resource to x=y. Will check later 
			 * for modifiers that need to wait til runtime to get a value.
			 */
			sprintf(buf,"%s=%s",$1,$3);
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
    ;

SetResourceType
	: Set { $$ = $1; } 
	;

ResourceType
    : InitialShells  { $$ = InitialShells; }
	| Font 
		{ $$ =  Font ; }
	| FixedFont 
		{ $$ =  FixedFont ; }
	| EditorFont 
		{ $$ =  EditorFont ; }
	| Background 
		{ $$ =  Background ; }
	| Foreground 
		{ $$ =  Foreground ; }
	| BackgroundPixmap 
		{ $$ =  BackgroundPixmap ; }
	| TopShadowColor 
		{ $$ =  TopShadowColor ; }
	| TopShadowPixmap 
		{ $$ =  TopShadowPixmap ; }
	| BottomShadowColor 
		{ $$ =  BottomShadowColor ; }
	| BottomShadowPixmap 
		{ $$ =  BottomShadowPixmap ; }
	| X 
		{ $$ =  X ; }
	| DX 
		{ $$ =  DX ; }
	| Y 
		{ $$ =  Y ; }
	| DY 
		{ $$ =  DY ; }
	| Width 
		{ $$ =  Width ; }
	| Height 
		{ $$ =  Height ; }
	| ForceSize 
		{ $$ =  ForceSize ; }
	| MaxWidth 
		{ $$ =  MaxWidth ; }
	| MaxHeight 
		{ $$ =  MaxHeight ; }
	| Columns 
		{ $$ =  Columns ; }
	| Override 
		{ $$ =  Override ; }
	| Functions 
		{ $$ =  Functions ; }
	| Decorations 
		{ $$ =  Decorations ; }
	| Popup 
		{ $$ =  Popup ; }
	| Popdown 
		{ $$ =  Popdown ; }
	| Destroy 
		{ $$ =  Destroy ; }
	| Exit 
		{ $$ =  Exit ; }
	| Help 
		{ $$ =  Help ; }
	| Eval 
		{ $$ =  Eval ; }
	| PostNotice 
		{ $$ =  PostNotice ; }
	| RunForeground 
		{ $$ =  RunForeground ; }
	| RunBackground 
		{ $$ =  RunBackground ; }
	| CommandShell
		{ $$ =  CommandShell ; }
	| InteractiveShell
		{ $$ =  InteractiveShell ; }
	| InputFrom 
		{ $$ =  InputFrom ; }
	| CaptureOutput 
		{ $$ =  CaptureOutput ; }
	| NotifyComplete
		{ $$ =  NotifyComplete ; }
	| UpdateFrom 
		{ $$ =  UpdateFrom ; }
	| PaneType 
		{ $$ =  PaneType ; }
	| Pane 
		{ $$ =  Pane ; }
	| Store 
		{ $$ =  Store ; }
	| Highlight 
		{ $$ =  Highlight ; }
	| GetEnv 
		{ $$ =  GetEnv ; }
	| Clear 
		{ $$ =  Clear ; }
	| CommandArg 
		{ $$ =  CommandArg ; }
	| TableArg 
		{ $$ =  TableArg ; }
	| Alignment 
		{ $$ =  Alignment ; }
	| ListElement 
		{ $$ =  ListElement ; }
	| ListSeparator 
		{ $$ =  ListSeparator ; }
	| ListType 
		{ $$ =  ListType ; }
	| VisibleItems 
		{ $$ =  VisibleItems ; }
	| ScrollBar 
		{ $$ =  ScrollBar ; }
	| ValueString 
		{ $$ =  ValueString ; }
	| Scrolled 
		{ $$ =  Scrolled ; }
	| LabelPixmap 
		{ $$ =  LabelPixmap ; }
	| MaxLength 
		{ $$ =  MaxLength ; }
	| Minimum 
		{ $$ =  Minimum ; }
	| Maximum 
		{ $$ =  Maximum ; }
	| StartValue 
		{ $$ =  StartValue ; }
	| SliderWidth 
		{ $$ =  SliderWidth ; }
	| SliderHeight 
		{ $$ =  SliderHeight ; }
	| Orientation 
		{ $$ =  Orientation ; }
	| DecimalPoints 
		{ $$ =  DecimalPoints ; }
	| EntryFont
		{ $$ =  EntryFont ; }
	| Rows 
		{ $$ =  Rows ; }
	| RowsDisplayed
		{ $$ =  RowsDisplayed ; }
	| FixedRows
		{ $$ =  FixedRows ; }
	| FixedColumns
		{ $$ =  FixedColumns ; }
	| ColumnsDisplayed
		{ $$ =  ColumnsDisplayed ; }
	| ColumnHeadings
		{ $$ =  ColumnHeadings ; }
	| RowHeadings
		{ $$ =  RowHeadings ; }
	| RowValue
		{ $$ =  RowValue ; }
	| TableValue
		{ $$ =  TableValue ; }
	| RowHeight
		{ $$ =  RowHeight ; }
	| ColumnWidth
		{ $$ =  ColumnWidth ; }
	| Newline 
		{ $$ =  Newline ; }
	| Separator
		{ $$ =  Separator ; }
	| TitleString
		{ $$ =  TitleString ; }
	| ToggleType
		{ $$ =  ToggleType ; }
	| ToggleState
		{ $$ =  ToggleState ; }
	| SeparatorType
		{ $$ =  SeparatorType ; }
	| UpdateObject
		{ $$ =  UpdateObject ; }
	| Sensitive
		{ $$ =  Sensitive ; }
	| Insensitive
		{ $$ =  Insensitive ; }
    ; 

OptGlobals
	: /* none, they're optional */
	| Globals ;

Globals
	: Globals GlobalElement
	| GlobalElement ;

GlobalElement
    : Font ':' String 
		{
			xgenGD.g_font = SaveString($3);
			if ( (xgenGD.g_fs = XLoadQueryFont(xgenGD.display,xgenGD.g_font)) == 0 ) {
				char errorbuf[80];

				sprintf(errorbuf,"font %s not found",xgenGD.g_font);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		}
    | FixedFont ':' String 
		{
			xgenGD.g_ffont = SaveString($3);
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
    | EditorFont ':' String 
		{
			xgenGD.g_edfont = SaveString($3);
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
    | Background ':' String 
		{
			xgenGD.g_bg = SaveString($3);
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
    | Foreground ':' String 
		{
			xgenGD.g_fg = SaveString($3);
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
	| BackgroundPixmap ':' String
		{ 
			Pixel tfg = ((xgenGD.g_fgs.pixel) ? 
				(xgenGD.g_fgs.pixel):BlackPixel(xgenGD.display,xgenGD.screen));
			Pixel tbg = ((xgenGD.g_bgs.pixel) ? 
				(xgenGD.g_bgs.pixel):WhitePixel(xgenGD.display,xgenGD.screen));

			xgenGD.g_bgpix = SaveString($3);
			xgenGD.g_bgpm = XmGetPixmap(xgenGD.scrptr, xgenGD.g_bgpix, tfg, tbg);
			if ( xgenGD.g_bgpm == XmUNSPECIFIED_PIXMAP ) {
				char errorbuf[80];

				sprintf(errorbuf,
					"invalid background pixmap %s",xgenGD.g_bgpix);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		}
	| TopShadowColor ':' String
		{ 
			xgenGD.g_ts = SaveString($3);
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
	| TopShadowPixmap ':' String
		{ 
			Pixel tfg = ((xgenGD.g_fgs.pixel) ? 
				(xgenGD.g_fgs.pixel):BlackPixel(xgenGD.display,xgenGD.screen));
			Pixel tbg = ((xgenGD.g_bgs.pixel) ? 
				(xgenGD.g_bgs.pixel):WhitePixel(xgenGD.display,xgenGD.screen));

			xgenGD.g_tspix = SaveString($3);
			xgenGD.g_tspm = XmGetPixmap(xgenGD.scrptr, xgenGD.g_tspix, tfg, tbg);
			if ( xgenGD.g_tspm == XmUNSPECIFIED_PIXMAP ) {
				char errorbuf[80];

				sprintf(errorbuf,
					"invalid top shadow pixmap %s",xgenGD.g_tspix);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		}
	| BottomShadowColor ':' String
		{ 
			xgenGD.g_bs = SaveString($3);
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
	| BottomShadowPixmap ':' String
		{ 
			Pixel tfg = ((xgenGD.g_fgs.pixel) ? 
				(xgenGD.g_fgs.pixel):BlackPixel(xgenGD.display,xgenGD.screen));
			Pixel tbg = ((xgenGD.g_bgs.pixel) ? 
				(xgenGD.g_bgs.pixel):WhitePixel(xgenGD.display,xgenGD.screen));

			xgenGD.g_bspix = SaveString($3);
			xgenGD.g_bspm = XmGetPixmap(xgenGD.scrptr, xgenGD.g_bspix, tfg, tbg);
			if ( xgenGD.g_bspm == XmUNSPECIFIED_PIXMAP ) {
				char errorbuf[80];

				sprintf(errorbuf,
					"invalid bottom shadow pixmap %s",xgenGD.g_bspix);
				XgenFatalWarning("parser",errorbuf);
				fatalError = True;
			}
		}
    ;

Value
    : String
		{
			$$ = SaveString($1);
		}
    | Integer
		{
			char buf[80];

			sprintf(buf,"%d",$1);
			$$ = SaveString(buf);
		}
    | Real
		{
			char buf[80];

			sprintf(buf,"%f",$1);
			$$ = SaveString(buf);
		}
    | Logical
		{
			if ( $1 == True ) $$ = SaveString("True");
			else $$ = SaveString("False");
		}
    ;

%%
