
/*
 * FILE: editrange.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 *
 * EditCellAddressRange(x1,y1,x2,y2)
 * ---------------------------------
 * This function presents a dialog box with a text field 
 * so that the user can edit the cell address range 
 * parameters x1,y1,x2 and y2.
 * 
 * catApplyCB()
 * ------------
 * This is the APPLY button callback, it calls the 
 * function ApplyBrush for each cell in the cell address
 * range X1,Y1,X2,Y2. 
 *
 */

#include "xgre.h"

void carApplyCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/*** LOCAL GLOBALS ***/

int X1,Y1,X2,Y2;

Widget xgi;
Widget begRowText;
Widget begColText;
Widget endRowText;
Widget endColText;

/*** LOCAL PROTOTYPES ***/

void carApplyCB(
#ifndef _NO_PROTO
   Widget w;
   XtPointer cld;
   XtPointer cad;
#endif
);

void editRangeCancelCB(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

/****************************/
/*** EditCellAddressRange ***/
/****************************/

void
#ifdef _NO_PROTO
EditCellAddressRange(x1,y1,x2,y2)
   int x1;
   int y1;
   int x2;
   int y2;
#else
EditCellAddressRange( int x1, int y1, int x2, int y2 )
#endif
{
   Arg al[15];
   int ac = 0;
   char msg[20];
   XmString xms;
   XmString txms;
   Widget carRC;
   Widget carLab;
   Widget begRowLab;
   Widget begColLab;
   Widget endRowLab;
   Widget endColLab;

   if (Global.FeditRangeD) return;
   Global.FeditRangeD = True;

   X1 = x1;
   Y1 = y1;
   X2 = x2;
   Y2 = y2;

   xgi = XgCreateInteractorDialog(Global.applShell,
      "EditCellAddressRange",NULL,0);
   XtAddCallback(xgi,XmNapplyCallback,carApplyCB,NULL);
   XtAddCallback(xgi,XmNcancelCallback,editRangeCancelCB,NULL);
   XtManageChild(XgInteractorGetChild(xgi,XmINTERACT_APPLY_BUTTON));
   XtUnmanageChild(XgInteractorGetChild(xgi,XmINTERACT_OK_BUTTON));
   carRC = XtVaCreateManagedWidget("carRC",
      xmRowColumnWidgetClass,xgi,NULL);

   xms = XmStringCreateSimple("Enter cell address range to be edited");
   carLab = XtVaCreateManagedWidget("carLab",
      xmLabelWidgetClass,carRC,XmNlabelString,xms,NULL);

/*** BEGINNING ROW TEXT FIELD ***/

   xms = XmStringCreateSimple("Beginning row:");
   begRowLab = XtVaCreateManagedWidget("begRowLab",
      xmLabelWidgetClass,carRC,XmNlabelString,xms,NULL);

   sprintf(msg,"%d",y1);
   begRowText = XtVaCreateManagedWidget("begRowText",
      xmTextFieldWidgetClass,carRC,XmNvalue,msg,NULL);

/*** BEGINNING COLUMN TEXT FIELD ***/

   xms = XmStringCreateSimple("Beginning column:");
   begColLab = XtVaCreateManagedWidget("begColLab",
      xmLabelWidgetClass,carRC,XmNlabelString,xms,NULL);

   sprintf(msg,"%d",x1);
   begColText = XtVaCreateManagedWidget("begColText",
      xmTextFieldWidgetClass,carRC,XmNvalue,msg,NULL);

/*** END ROW TEXT FIELD ***/

   xms = XmStringCreateSimple("Ending row:");
   endRowLab = XtVaCreateManagedWidget("endRowLab",
      xmLabelWidgetClass,carRC,XmNlabelString,xms,NULL);

   sprintf(msg,"%d",y2);
   endRowText = XtVaCreateManagedWidget("endRowText",
      xmTextFieldWidgetClass,carRC,XmNvalue,msg,NULL);

/*** END COLUMN TEXT FIELD ***/

   xms = XmStringCreateSimple("Ending column:");
   endColLab = XtVaCreateManagedWidget("endColLab",
      xmLabelWidgetClass,carRC,XmNlabelString,xms,NULL);

   sprintf(msg,"%d",x2);
   endColText = XtVaCreateManagedWidget("endColText",
      xmTextFieldWidgetClass,carRC,XmNvalue,msg,NULL);

   XmStringFree(xms);
   XtManageChild(xgi);
}

/******************/
/*** carApplyCB ***/
/******************/

void 
#ifdef _NO_PROTO
carApplyCB(w,cld,cad)
   Widget w;
   XtPointer cld;
   XtPointer cad;
#else 
carApplyCB( Widget w, XtPointer cld, XtPointer cad)
#endif
{
char *bRowStr, *bColStr, *eRowStr, *eColStr;
int bRow, bCol, eRow, eCol;

if((bRowStr = XmTextFieldGetString(begRowText)) != NULL && 
   (bColStr = XmTextFieldGetString(begColText)) != NULL && 
   (eRowStr = XmTextFieldGetString(endRowText)) != NULL && 
   (eColStr = XmTextFieldGetString(endColText)) != NULL)
   {
   if((sscanf(bRowStr,"%d",&bRow)) == 1 &&
      (sscanf(bColStr,"%d",&bCol)) == 1 &&
      (sscanf(eRowStr,"%d",&eRow)) == 1 &&
      (sscanf(eColStr,"%d",&eCol)) == 1)
      {
      XtDestroyWidget(xgi);
      if (bRow <= eRow && bCol <= eCol)
         {
         int ix,iy;
         ClearUndoBuffer();
         for (iy = bRow; iy <= eRow; iy++)
            for (ix = bCol; ix <= eCol; ix++)
               ApplyBrush(ix,iy);
#        ifdef DEBUG
         printf("EditRange: ApplyBrush loop completed\n");
#        endif
         SetEditMode(XGRE_NORMAL);
         Global.FeditRangeD = False;
         return; 
         }
      else
         {
         XgError(Global.applShell,
            "Ending coordinates must be larger than beginning coordinates");
         return;
         }
      }
   else
      {
      XgError(Global.applShell,
         "Please enter valid integer coordinate values");
      return;
      }
   }
else 
   {
   XgError(Global.applShell,
      "Please enter valid integer coordinate values");
   return;
   }
}

/*************************/
/*** editRangeCancelCB ***/
/*************************/

void
#ifdef _NO_PROTO
editRangeCancelCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
editRangeCancelCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
SetEditMode(XGRE_NORMAL);
Global.FeditRangeD = False;
XtDestroyWidget(w);
}

