
/*
 * FILE: brushcreate.c 
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 *
 * BrushCreate()
 * -------------
 * Callback for the Brush-Create menu option.  This function presents
 * a dialog so that the user may specify the row and column size of 
 * the brush to be created. 
 *
 * rowArrowCB()
 * -------------
 * Callback for the row arrow buttons (rowUpArrow and rowDownArrow).  
 * This function either increases or decreases the number in the row 
 * text field (rowText).
 *
 * colArrowCB()
 * -------------
 * Callback for the column arrow buttons (colUpArrow and colDownArrow).  
 * This function either increases or decreases the number in the column 
 * text field (colText).
 *
 * bcreateOkCB()
 * -------------
 * Callback for the BrushCreate() OK button.  This function set the
 * Global.brushRows and Global.brushCols variables based on the uses's
 * selection and calls the BrushEdit() function so that the user may
 * edit the new brush.
 */

#include "xgre.h"

void rowArrowCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

void colArrowCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

void bcreateOkCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

void brushCreateCancelCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/* LOCAL GLOBALS */

int      rowval;
int      colval;
Widget   rowUpArrow;
Widget   rowDownArrow;
Widget   colUpArrow;
Widget   colDownArrow;
Widget   rowText;
Widget   colText;

/*******************/
/*** BrushCreate ***/
/*******************/

void
#ifdef _NO_PROTO
BrushCreate(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
BrushCreate(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Widget   bcreateRC;
Widget   bcreateText;
Widget   bcreateLabel;
Widget   rowForm;
Widget   rowLabel;
Widget   colForm;
Widget   colLabel;
Arg      al[15];
int      ac = 0;
XmString xms;
char buf[100];

if (Global.FbrushCreateD) return;

if (Global.FbrushEditD) 
   {
   XgError(Global.applShell,"Brush-Edit already running");
   return;
   }

Global.FbrushCreateD = True;

rowval = Global.brushRows;
colval = Global.brushCols;

/*** TOPLEVEL FORM ***/

Global.brushCreateD = XgCreateInteractorDialog(Global.applShell,
   "BrushCreate",NULL,0);
XtAddCallback(Global.brushCreateD,XmNokCallback,bcreateOkCB,cld);
XtAddCallback(Global.brushCreateD,XmNcancelCallback,brushCreateCancelCB,NULL);
bcreateRC = XtVaCreateManagedWidget("bcreateRC",
   xmRowColumnWidgetClass,Global.brushCreateD,NULL);

xms = XmStringCreateSimple("Please Specify Size of Brush to be created:");
bcreateLabel = XtVaCreateManagedWidget("bcreateLabel",
   xmLabelWidgetClass,bcreateRC,XmNlabelString,xms,NULL);
XmStringFree(xms);

/*** CREATE ARROW BUTTON TEXT BOX FOR SETTING NUMBER OF ROWS ***/

rowForm = XtVaCreateManagedWidget("rowForm",
   xmFormWidgetClass,bcreateRC,NULL);

xms = XmStringCreateSimple("Number of Rows:     ");
rowLabel = XtVaCreateManagedWidget("rowLabel",
   xmLabelWidgetClass,rowForm,
   XmNlabelString,xms,
   XmNleftAttachment,XmATTACH_FORM,
   XmNtopAttachment,XmATTACH_FORM,
   XmNbottomAttachment,XmATTACH_FORM,NULL);
XmStringFree(xms);

rowUpArrow = XtVaCreateManagedWidget("rowUpArrow",
   xmArrowButtonWidgetClass,rowForm,
   XmNarrowDirection,XmARROW_UP,
   XmNleftAttachment,XmATTACH_WIDGET,
   XmNleftWidget,rowLabel,
   XmNtopAttachment,XmATTACH_FORM,NULL);
XtAddCallback(rowUpArrow,XmNactivateCallback,rowArrowCB,NULL);

rowDownArrow = XtVaCreateManagedWidget("rowDownArrow",
   xmArrowButtonWidgetClass,rowForm,
   XmNarrowDirection,XmARROW_DOWN,
   XmNleftAttachment,XmATTACH_WIDGET,
   XmNleftWidget,rowLabel,
   XmNtopAttachment,XmATTACH_WIDGET,
   XmNtopWidget,rowUpArrow,
   XmNbottomAttachment,XmATTACH_FORM,NULL);
XtAddCallback(rowDownArrow,XmNactivateCallback,rowArrowCB,NULL);

rowText = XtVaCreateManagedWidget("rowText",
   xmTextFieldWidgetClass,rowForm,
   XmNeditable,False,
   XmNtopAttachment,XmATTACH_FORM,
   XmNleftAttachment,XmATTACH_WIDGET,
   XmNleftWidget,rowDownArrow,
   XmNbottomAttachment,XmATTACH_FORM,NULL);
sprintf(buf,"%d",rowval);
XmTextFieldSetString(rowText,buf);

/*** CREATE ARROW BUTTON TEXT BOX FOR SETTING NUMBER OF COLS ***/

colForm = XtVaCreateManagedWidget("colForm",
   xmFormWidgetClass,bcreateRC,NULL);

xms = XmStringCreateSimple("Number of Columns:  ");
colLabel = XtVaCreateManagedWidget("colLabel",
   xmLabelWidgetClass,colForm,
   XmNlabelString,xms,
   XmNleftAttachment,XmATTACH_FORM,
   XmNtopAttachment,XmATTACH_FORM,
   XmNbottomAttachment,XmATTACH_FORM,NULL);
XmStringFree(xms);

colUpArrow = XtVaCreateManagedWidget("colUpArrow",
   xmArrowButtonWidgetClass,colForm,
   XmNarrowDirection,XmARROW_UP,
   XmNleftAttachment,XmATTACH_WIDGET,
   XmNleftWidget,colLabel,
   XmNtopAttachment,XmATTACH_FORM,NULL);
XtAddCallback(colUpArrow,XmNactivateCallback,colArrowCB,NULL);

colDownArrow = XtVaCreateManagedWidget("colDownArrow",
   xmArrowButtonWidgetClass,colForm,
   XmNarrowDirection,XmARROW_DOWN,
   XmNleftAttachment,XmATTACH_WIDGET,
   XmNleftWidget,colLabel,
   XmNtopAttachment,XmATTACH_WIDGET,
   XmNtopWidget,colUpArrow,
   XmNbottomAttachment,XmATTACH_FORM,NULL);
XtAddCallback(colDownArrow,XmNactivateCallback,colArrowCB,NULL);

colText = XtVaCreateManagedWidget("colText",
   xmTextFieldWidgetClass,colForm,
   XmNvalue,xms,
   XmNeditable,False,
   XmNleftAttachment,XmATTACH_WIDGET,
   XmNleftWidget,colUpArrow,
   XmNtopAttachment,XmATTACH_FORM,
   XmNbottomAttachment,XmATTACH_FORM,NULL);
sprintf(buf,"%d",colval);
XmTextFieldSetString(colText,buf);

XtManageChild(Global.brushCreateD);
}

/*******************/
/*** bcreateOkCB ***/
/*******************/

void
#ifdef _NO_PROTO
bcreateOkCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
bcreateOkCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Global.brushRows   = rowval;
Global.brushCols   = colval;
Global.brushHotRow = 0;
Global.brushHotCol = 0;

if (Global.brushCols != 1 || Global.brushRows != 1)
   if (Global.mode != XGRE_LOCKED && Global.mode != XGRE_UNLOADED) 
      SetEditMode(XGRE_NORMAL);

Global.FbrushCreateD = False;

BuildDefaultBrush();
BrushEdit();
}

/******************/
/*** rowArrowCB ***/
/******************/

void
#ifdef _NO_PROTO
rowArrowCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
rowArrowCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
char buf[100];

if (w==rowUpArrow && rowval<XGRE_BROWS)
   rowval++;
else if (w==rowDownArrow && rowval>1)
   rowval--;

sprintf(buf,"%d",rowval);
XmTextFieldSetString(rowText,buf);
}

/******************/
/*** colArrowCB ***/
/******************/

void
#ifdef _NO_PROTO
colArrowCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
colArrowCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
char buf[100];

if (w==colUpArrow && colval<XGRE_BCOLS)
   colval++;
else if (w==colDownArrow && colval>1)
   colval--;

sprintf(buf,"%d",colval);
XmTextFieldSetString(colText,buf);
}

/***************************/
/*** brushCreateCancelCB ***/
/***************************/

void
#ifdef _NO_PROTO
brushCreateCancelCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
brushCreateCancelCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Global.FbrushCreateD = False;
XtDestroyWidget(w);
}

