
/*
 * FILE: brushedit.c 
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 *
 * BrushEditCB()
 * -------------
 * A callback function which calls BrushEdit().
 *
 * BrushEdit()
 * -----------
 * This function sets up the brush-edit dialog which consists of a 
 * matrix of brush-cell buttons, a postion text field, a value text
 * field, an input option list, an operation option list, a hot-spot
 * button and a match-cell button.  This functio also copies the
 * global brush into a temporary brush structure for editing.
 *
 * brushButtonCB()
 * ---------------
 * This is the callback for the brush-cell buttons.  This function 
 * figures out the row and column address of the brush-cell that was
 * pressed and sets that brush-cell as the "current-cell."  This 
 * function then places the current cell's row and column address
 * in the postion text field.  It then highlights the current 
 * cell by giving it a BIG shadow border.
 *
 * valueText()
 * -----------
 * This is the callback for the value text field.  This function 
 * will place the value that is typed into this field into the
 * current cell.
 *
 * inputListCB()
 * -------------
 * This is the input option list callback.  This function 
 * will place the input option that is selected from the list 
 * into the current cell. 
 *
 * opListCB()
 * ----------
 * This is the operation option list callback.  This function 
 * will place the operation option that is selected from the list 
 * into the current cell. 
 *
 * hotButtonCB()
 * -------------
 * his is the hot-spot button callback, it will set the current
 * cell as the brush hot spot.
 *
 * matchButtonCB()
 * ---------------
 * This is the match-cell button callback, it will set all brush
 * cells to match the current cell. 
 *
 * brushOkCB()
 * -----------
 * This is the OK button callback, it will copy the temporary 
 * brush information into the global brush structure.  It will 
 * then destroy the brush-edit dialog.
 * 
 */

#ifdef SVR4
#include "string.h"
#else
#include "strings.h"
#endif 

#include "xgre.h"

/*** FUNCTION PROTOTYPES ***/

void brushButtonCB(
#ifndef _NO_PROTO
   Widget w,
   int *bnum,
   XmRowColumnStruct *cad;
#endif
);

void brushOkCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

void hotButtonCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

void inputListCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XmListCallbackStruct cad
#endif
);

void matchButtonCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

void opListCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XmListCallbackStruct cad
#endif
);

void valueTextCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

void brushEditCancelCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/*** LOCAL GLOBALS ***/

Widget      buttons[100];     /* brush-cell buttons */
Widget      opList;           /* list of brush-cell operations */ 
Widget      opText;           /* text field for brush-cell operation */ 
Widget      inputList;        /* list of brush-cell input options */
Widget      inputText;        /* text field for brush-cell input */
Widget      posText;          /* text field for brush-cell position */
Widget      valueText;        /* text field for brush-cell value */
BrushCell   tbrush[XGRE_BCOLS][XGRE_BROWS]; /* temporary brush */
int         brushNum; 
int         brushRow;
int         brushCol;
int         hotNum;
int         hotRow;
int         hotCol;
int         fgcolor;
int         bgcolor;
XmString    opStrings[XGRE_OPCOUNT];
XmString    inpStrings[XGRE_INPCOUNT];

char *opListItems[XGRE_OPCOUNT] =   /* list of brush operations */
   {
   "Copy",                          /* 0 = XGRE_OPCOPY */
   "Null",                          /* 1 = XGRE_OPNULL */
   "Erase",                         /* 2 = XGRE_OPERASE */
   "Add",                           /* 3 = XGRE_OPADD */
   "Mult",                          /* 4 = XGRE_OPMULT */
   "Null"                           /* 5 = XGRE_OPCALC  FIX! */ 
   };

char *inpListItems[XGRE_INPCOUNT] = /* list of brush input options */
   {
   "Category",                      /* 0 = XGRE_INPCAT */ 
   "Read",                          /* 1 = XGRE_INPREAD */
   "Value",                         /* 2 = XGRE_INPVALUE */
   };

/*****************/
/*** BrushEdit ***/
/*****************/

void
#ifdef _NO_PROTO
BrushEditCB(w, cld, cad)
   Widget w;
   XtPointer cld;
   XtPointer cad;
#else
BrushEditCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
BrushEdit();
}

void
#ifdef _NO_PROTO
   BrushEdit()
#else
   BrushEdit()
#endif
{
Arg al[30];
int ac;
Widget      brushForm;        /* toplevel: Form */
Widget      brushFrame;    
Widget         brushCellBox;    
Widget         brushRC;
Widget         brushLabel; 
Widget      brushInfoRC;      /* RowColumn for brush information */ 
Widget         infoFrame;     /* frame for brushInfoRC */
Widget         posLabel;
Widget         valueLabel;
Widget         infoSep;
Widget         hotButton;     /* "set hotspot" button */
Widget         matchButton;   /* "set all cells to match" button */
Widget      brushInputRC;     /* RowColumn for setting brush cell input */ 
Widget         inputFrame;    /* frame for brushInputRC */
Widget         inputLabel;
Widget      brushOpRC;        /* RowColumn for setting brush cell operation */
Widget         opFrame;       /* frame for brushOpRc */
Widget         opLabel;
char        msg[200];
XmString    xms;
int         bx,by;
int         bcount=0;
int         count;

if (Global.FbrushEditD) return;
if (Global.FbrushCreateD) 
   {
   XgError(Global.applShell,"Brush-Create already running");
   return;
   }
Global.FbrushEditD = True;

brushNum = -1;
brushRow = -1;
brushCol = -1;

/* load temporary brush */
for (bx=0; bx < Global.brushCols; bx++)
   for (by=0; by < Global.brushRows; by++)
      {
      tbrush[bx][by].input = Global.brush[bx][by].input;
      tbrush[bx][by].op    = Global.brush[bx][by].op;
      tbrush[bx][by].value = Global.brush[bx][by].value;
      }
hotNum = -1;
hotRow = Global.brushHotRow;
hotCol = Global.brushHotCol;

/*** CREATE TOPLEVEL WIDGETS ***/

Global.brushEditD = XgCreateInteractorDialog(Global.applShell,
   "EditBrush",NULL,0);
XtAddCallback(Global.brushEditD,XmNokCallback,brushOkCB,NULL);
XtAddCallback(Global.brushEditD,XmNcancelCallback,brushEditCancelCB,NULL);

XtVaGetValues(Global.brushEditD,XmNforeground,&fgcolor,NULL);
XtVaGetValues(Global.brushEditD,XmNbackground,&bgcolor,NULL);

brushForm = XtVaCreateManagedWidget("brushForm",
   xmFormWidgetClass,Global.brushEditD,
   XmNverticalSpacing,5,
   XmNhorizontalSpacing,5,NULL);

/*** CREATE BRUSH CELL BUTTONS BOX ***/

brushFrame = XtVaCreateManagedWidget("brushFrame",
   xmFrameWidgetClass,brushForm,
   XmNleftAttachment,XmATTACH_FORM,
   XmNrightAttachment,XmATTACH_FORM,
   XmNtopAttachment,XmATTACH_FORM,NULL);

brushCellBox = XtVaCreateManagedWidget("brushCellBox",
   xmRowColumnWidgetClass,brushFrame,
   XmNorientation,XmVERTICAL,NULL);

if (Global.bname == NULL) 
   sprintf(msg,"Brush: (noname) Size: %dx%d (hotspot shown in red)",
      Global.brushCols,Global.brushRows); 
else 
   sprintf(msg,"Brush: %s Size: %dx%d (hotspot shown in red)",
      Global.bname,Global.brushCols,Global.brushRows); 

xms = XmStringCreateSimple(msg); 
brushLabel = XtVaCreateManagedWidget("brushLabel",
   xmLabelWidgetClass,brushCellBox,
   XmNlabelString,xms,NULL);
XmStringFree(xms);

brushRC = XtVaCreateManagedWidget("brushRC",
   xmRowColumnWidgetClass,brushCellBox,
   XmNorientation,XmHORIZONTAL,
   XmNpacking,XmPACK_COLUMN,
   XmNnumColumns,(short)Global.brushRows,NULL);

/* loop to create button for each brush cell */
for (bx = 0; bx < Global.brushCols; bx++)
   for (by = 0; by < Global.brushRows; by++)
      {
      /* build button name and label strings */
      char blab[100], bname[100];
      sprintf(blab,"INP| %s\n OP| %s\nVAL| %d",  
         inpListItems[tbrush[bx][by].input],
         opListItems[tbrush[bx][by].op],
         tbrush[bx][by].value );
      sprintf(bname,"brushButton%d",bcount);

      /* create button and give it a callback */
      buttons[bcount] = XtVaCreateManagedWidget(bname,
         xmPushButtonWidgetClass,brushRC,
         XmNlabelString,XmStringCreateLtoR(blab,XmSTRING_DEFAULT_CHARSET),
         XmNrecomputeSize,True,
         XmNmarginLeft,5,   
         XmNmarginRight,5,
         XmNmarginBottom,5, 
         XmNmarginTop,5,NULL);
      XtAddCallback(buttons[bcount],XmNactivateCallback,
         brushButtonCB,(int)bcount);
       
      XgAddHelpCallBackFromFile(buttons[bcount],"xgre/xgre_brush_cell");

      /* highlight the hotspot button */
      if (hotRow==by && hotCol==bx)
         {
         XtVaSetValues(buttons[bcount],
            XmNforeground,XgdGetVectColorPixelByName("white"),
            XmNbackground,XgdGetVectColorPixelByName("red"),NULL);
         hotNum = bcount;          
         }

      bcount++;
      }

/*** CREATE BRUSH INFORMATION DISPLAY BOX ***/

/* frame holds a row-column widget */
infoFrame = XtVaCreateManagedWidget("infoFrame",
   xmFrameWidgetClass,brushForm,
   XmNtopAttachment,XmATTACH_FORM,
   XmNbottomAttachment,XmATTACH_FORM,
   XmNtopAttachment,XmATTACH_WIDGET,
   XmNtopWidget,brushFrame,NULL);
brushInfoRC = XtVaCreateManagedWidget("brushInfoRC",
   xmRowColumnWidgetClass,infoFrame,XmNorientation,XmVERTICAL,NULL);

/* brush cell postion text field */
posLabel = XtVaCreateManagedWidget("posLabel",
   xmLabelWidgetClass,brushInfoRC,
   XmNlabelString,XmStringCreateSimple("Brush Cell Position:"),NULL);
posText = XtVaCreateManagedWidget("posText",
   xmTextFieldWidgetClass,brushInfoRC,
   XmNeditable,False,NULL);
XgAddHelpCallBackFromFile(posText,"xgre/xgre_brush_pos");

/* brush cell value text field */
valueLabel = XtVaCreateManagedWidget("valueLabel",
   xmLabelWidgetClass,brushInfoRC,
   XmNlabelString,XmStringCreateSimple("Brush Cell Value (VAL):"),NULL),
valueText = XtVaCreateManagedWidget("valueText",
   xmTextFieldWidgetClass,brushInfoRC,
   XmNmaxLength,9,
   XmNeditable,True,NULL);
XtAddCallback(valueText,XmNvalueChangedCallback,valueTextCB,NULL);
XgAddHelpCallBackFromFile(valueText,"xgre/xgre_brush_val");

/* separator */
infoSep = XtVaCreateManagedWidget("infoSep",
   xmSeparatorWidgetClass,brushInfoRC,NULL);

/* "set brush hotspot" button */
xms = XmStringCreateSimple("Set Current Brush Cell As Hotspot"); 
hotButton = XtVaCreateManagedWidget("hotButton",
   xmPushButtonWidgetClass,brushInfoRC,
   XmNlabelString,xms,NULL);
XmStringFree(xms);
XtAddCallback(hotButton,XmNactivateCallback,hotButtonCB,NULL);

/* "set all brush cells to match current brush cell" button */
xms = XmStringCreateSimple("Set All Brush Cells to Match");
matchButton = XtVaCreateManagedWidget("matchButton",
   xmPushButtonWidgetClass,brushInfoRC,
   XmNlabelString,xms,NULL);
XmStringFree(xms);
XtAddCallback(matchButton,XmNactivateCallback,matchButtonCB,NULL);

/*** CREATE OPERATION SPECIFICATION BOX ***/

/* frame holds a row-column widget */ 
opFrame = XtVaCreateManagedWidget("opFrame",
   xmFrameWidgetClass,brushForm,
   XmNtopAttachment,XmATTACH_WIDGET,
   XmNtopWidget,brushFrame,
   XmNbottomAttachment,XmATTACH_FORM,
   XmNleftAttachment,XmATTACH_WIDGET,
   XmNleftWidget,infoFrame,NULL);
brushOpRC = XtVaCreateManagedWidget("brushOpRC",
   xmRowColumnWidgetClass,opFrame,XmNorientation,XmVERTICAL,NULL);

/* operation text field */
opLabel = XtVaCreateManagedWidget("opLabel",
   xmLabelWidgetClass,brushOpRC,
   XmNlabelString,XmStringCreateSimple("Brush Cell Operation (OP):"),NULL);
opText = XtVaCreateManagedWidget("opText",
   xmTextFieldWidgetClass,brushOpRC,
   XmNeditable,False,NULL);

/* operation list */
count = 0;
while (count < XGRE_OPCOUNT)
   {
   opStrings[count] = XmStringCreateSimple(opListItems[count]);
   count++;
   }
ac=0;
XtSetArg(al[ac],XmNitemCount,XGRE_OPCOUNT); ac++;
XtSetArg(al[ac],XmNitems,opStrings); ac++;
XtSetArg(al[ac],XmNselectionPolicy,XmSINGLE_SELECT); ac++;
XtSetArg(al[ac],XmNvisibleItemCount,6); ac++;
opList = XmCreateScrolledList(brushOpRC,"opList",al,ac);
XtManageChild(opList);
XtAddCallback(opList,XmNsingleSelectionCallback,opListCB,NULL);
/*XgAddHelpCallBackFromFile(opList,"xgre/xgre_op_list");*/

/*** CREATE INPUT SPECIFICATION BOX ***/

/* frame holds a row-column widget */ 
inputFrame = XtVaCreateManagedWidget("inputFrame",
   xmFrameWidgetClass,brushForm,
   XmNtopAttachment,XmATTACH_WIDGET,
   XmNtopWidget,brushFrame,
   XmNbottomAttachment,XmATTACH_FORM,
   XmNrightAttachment,XmATTACH_FORM,
   XmNleftAttachment,XmATTACH_WIDGET,
   XmNleftWidget,opFrame,NULL);
brushInputRC = XtVaCreateManagedWidget("brushInputRC",
   xmRowColumnWidgetClass,inputFrame,XmNorientation,XmVERTICAL,NULL);

/* input text field */
inputLabel = XtVaCreateManagedWidget("inputLabel",
   xmLabelWidgetClass,brushInputRC,
   XmNlabelString,XmStringCreateSimple("Brush Cell Input (INP):"),NULL);
inputText = XtVaCreateManagedWidget("inputText",
   xmTextFieldWidgetClass,brushInputRC,
   XmNeditable,False,NULL);

/* input list */
count = 0;
while (count < XGRE_INPCOUNT)
   {
   inpStrings[count] = XmStringCreateSimple(inpListItems[count]);
   count++;
   }
ac=0;
XtSetArg(al[ac],XmNitemCount,XGRE_INPCOUNT); ac++;
XtSetArg(al[ac],XmNitems,inpStrings); ac++;
XtSetArg(al[ac],XmNselectionPolicy,XmSINGLE_SELECT); ac++;
XtSetArg(al[ac],XmNvisibleItemCount,6); ac++;
inputList = XmCreateScrolledList(brushInputRC,"inpList",al,ac);
XtManageChild(inputList);
XtAddCallback(inputList,XmNsingleSelectionCallback,inputListCB,NULL);
/*XgAddHelpCallBackFromFile(inputList,"xgre/xgre_input_list");*/

XtManageChild(Global.brushEditD);
}

/*********************/
/*** brushButtonCB ***/
/*********************/

void
#ifdef _NO_PROTO
brushButtonCB(w, bnum, cad)
Widget w;
int bnum;
XmRowColumnCallbackStruct *cad;
#else
brushButtonCB(Widget w, int *bnum, XmRowColumnCallbackStruct *cad)
#endif
{
char blab[100], bname[100];
int brow;
int bcol;
char cellstr[20];
char inputstr[20];
char opstr[20];
char valstr[20];

brow = bnum/Global.brushCols;
bcol = bnum%Global.brushCols;

/* un-highlight previous selection */
if (brushNum != -1)
   XtVaSetValues(buttons[brushNum],XmNshadowThickness,2,NULL);

/* highlight selected brush cell button with a thick shadow */
XtVaSetValues(w,XmNshadowThickness,5,NULL);

brushRow = brow;
brushCol = bcol;
brushNum = bnum;

sprintf(blab,"INP: %s\n OP: %s\nVAL: %d",
   inpListItems[tbrush[brushCol][brushRow].input],
   opListItems[tbrush[brushCol][brushRow].op],
   tbrush[brushCol][brushRow].value );
XtVaSetValues(w,XmNlabelString,
   XmStringCreateLtoR(blab,XmSTRING_DEFAULT_CHARSET),NULL);

/* fill in all the text fields with the brush-cell's values */ 
sprintf(cellstr,"(%d,%d)",bcol,brow);
XtVaSetValues(posText,XmNvalue,cellstr,NULL);

sprintf(valstr,"%d",tbrush[bcol][brow].value);
XtVaSetValues(valueText,XmNvalue,valstr,NULL);

sprintf(inputstr,"%s",inpListItems[tbrush[bcol][brow].input]);
XtVaSetValues(inputText,XmNvalue,inputstr,NULL);

sprintf(opstr,"%s",opListItems[tbrush[bcol][brow].op]);
XtVaSetValues(opText,XmNvalue,opstr,NULL);
}

/*******************/
/*** inputListCB ***/
/*******************/

void
#ifdef _NO_PROTO
inputListCB(w, cld, cad)
Widget w;
XtPointer cld;
XmListCallbackStruct *cad;
#else
inputListCB(Widget w, XtPointer cld, XtPointer *cad)
#endif
{
int ac;
Arg al[30];
XmString xms;
char **inputstr;
char blab[100];

if (brushNum == -1)
   {
   XgError(Global.applShell,"You must choose a\nbrush-cell (above) first"); 
   return;
   }

/* get list selection string and put it in the text field */
XmStringGetLtoR(cad->item,XmSTRING_DEFAULT_CHARSET,&inputstr);
XmTextFieldSetString(inputText,inputstr);

/* put selection in the temporary brush structure */
tbrush[brushCol][brushRow].input = cad->item_position-1;

/* put selection on brush cell's button */
sprintf(blab,"INP: %s\n OP: %s\nVAL: %d",
   inpListItems[tbrush[brushCol][brushRow].input],
   opListItems[tbrush[brushCol][brushRow].op],
   tbrush[brushCol][brushRow].value );
XtVaSetValues(buttons[brushNum],
   XmNlabelString,XmStringCreateLtoR(blab,XmSTRING_DEFAULT_CHARSET),NULL);
}

/****************/
/*** opListCB ***/
/****************/

void
#ifdef _NO_PROTO
opListCB(w, cld, cad)
Widget w;
XtPointer cld;
XmListCallbackStruct *cad;
#else
opListCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
int ac;
Arg al[30];
XmString xms;
char **opstr;
char blab[100];

if (brushNum == -1)
   {
   XgError(Global.applShell,"You must choose a\nbrush-cell (above) first"); 
   return;
   }

/* get list selection string and put it in the text field */
XmStringGetLtoR(cad->item,XmSTRING_DEFAULT_CHARSET,&opstr);
XmTextFieldSetString(opText,opstr);

/* put selection in the temporary brush structure */
tbrush[brushCol][brushRow].op = cad->item_position-1;

/* put selection on brush cell's button */
sprintf(blab,"INP: %s\n OP: %s\nVAL: %d",
   inpListItems[tbrush[brushCol][brushRow].input],
   opListItems[tbrush[brushCol][brushRow].op],
   tbrush[brushCol][brushRow].value );
#ifdef DEBUG
printf("opListCB: Button[%d,%d] set:\n",brushCol,brushRow);
printf("%s\n",blab);
#endif 
XtVaSetValues(buttons[brushNum],
   XmNlabelString,XmStringCreateLtoR(blab,XmSTRING_DEFAULT_CHARSET),NULL);
}

/*******************/
/*** valueTextCB ***/
/*******************/

void
#ifdef _NO_PROTO
valueTextCB(w, cld, cad)
Widget w;
XtPointer cld;
XtPointer cad;
#else
valueTextCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
char *valstr;
int val;

if (brushNum == -1)
   {
   XgError(Global.applShell,"You must choose a\nbrush-cell (above) first");
   return;
   }

valstr = XmTextFieldGetString(w);
if (*valstr == NULL) return;

if ((sscanf(valstr,"%d",&val) == 1) && (brushNum != -1))
   {
   char blab[100];

   if (val > MAX_CELL) val=MAX_CELL;
   if (val < MIN_CELL) val=MIN_CELL;

   /* put value in the temporary brush structure */
   tbrush[brushCol][brushRow].value = val;

   /* put value on brush cell's button */
   sprintf(blab,"INP: %s\n OP: %s\nVAL: %d",
      inpListItems[tbrush[brushCol][brushRow].input],
      opListItems[tbrush[brushCol][brushRow].op],
      tbrush[brushCol][brushRow].value );
#  ifdef DEBUG
   printf("valueTextCB: Button[%d,%d] set:\n",brushCol,brushRow);
   printf("%s\n",blab);
#  endif 
   XtVaSetValues(buttons[brushNum],
      XmNlabelString,XmStringCreateLtoR(blab,XmSTRING_DEFAULT_CHARSET),NULL);
   }
}

/*****************/
/*** brushOkCB ***/
/*****************/

void
#ifdef _NO_PROTO
brushOkCB(w, cld, cad)
   Widget w;
   XtPointer cld;
   XtPointer cad;
#else
brushOkCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
int bx,by;
char msg[100];

/* copy temporary brush to global brush structure */

for (bx=0; bx < Global.brushCols; bx++)
   {
   for (by=0; by < Global.brushRows; by++)
      {
      Global.brush[bx][by].input = tbrush[bx][by].input;
      Global.brush[bx][by].op    = tbrush[bx][by].op;
      Global.brush[bx][by].value = tbrush[bx][by].value;
      }
   }
Global.brushHotRow = hotRow;
Global.brushHotCol = hotCol;

Global.FbrushEditD = False;
XtDestroyWidget(Global.brushEditD);

sprintf(msg,"BRUSH: %dx%d (custom)",Global.brushRows,Global.brushCols);
UpdateBrushText(msg);
}

/*******************/
/*** hotButtonCB ***/
/*******************/

void
#ifdef _NO_PROTO
hotButtonCB(w, cld, cad)
   Widget w;
   XtPointer cld;
   XtPointer cad;
#else
hotButtonCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
if (brushNum == -1)
   {
   XgError(Global.applShell,"You must choose a\nbrush-cell (above) first");
   return;
   }

if (hotNum != -1)
   /* un-highlight previous hotspot */
   XtVaSetValues(buttons[hotNum],
      XmNforeground,fgcolor,
      XmNbackground,bgcolor,NULL);

hotNum = brushNum; 
hotRow = brushRow;
hotCol = brushCol;

/* highlight new hotspot */
XtVaSetValues(buttons[hotNum],
   XmNforeground,XgdGetVectColorPixelByName("white"),
   XmNbackground,XgdGetVectColorPixelByName("red"),NULL);
}

/********************/
/*** matchButtonCB ***/
/********************/

void
#ifdef _NO_PROTO
matchButtonCB(w, cld, cad)
   Widget w;
   XtPointer cld;
   XtPointer cad;
#else
matchButtonCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
int tinput,top,tvalue;
int bx,by;
int bcount;

if (brushNum == -1)
   {
   XgError(Global.applShell,"You must choose a\nbrush-cell (above) first");
   return;
   }

tinput = tbrush[brushCol][brushRow].input;
top    = tbrush[brushCol][brushRow].op;
tvalue = tbrush[brushCol][brushRow].value;

bcount=0;
for (by=0; by < Global.brushRows; by++)
   for (bx=0; bx < Global.brushCols; bx++)
      {
      char blab[100];

      /* set brush cell in temporary brush */
      tbrush[bx][by].input = tinput;
      tbrush[bx][by].op    = top;
      tbrush[bx][by].value = tvalue;

      /* reset button label */
      sprintf(blab,"INP: %s\n OP: %s\nVAL: %d",
         inpListItems[tinput],opListItems[top],tvalue);
      XtVaSetValues(buttons[bcount],
         XmNlabelString,XmStringCreateLtoR(blab,XmSTRING_DEFAULT_CHARSET),NULL);

      bcount++;
      }
}

/*************************/
/*** brushEditCancelCB ***/
/*************************/

void
#ifdef _NO_PROTO
brushEditCancelCB(w, cld, cad)
   Widget w;
   XtPointer cld, cad;
#else
brushEditCancelCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Global.FbrushEditD = False;
XtDestroyWidget(w);
}

