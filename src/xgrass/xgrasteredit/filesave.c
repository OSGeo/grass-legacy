
/*
 * FILE: filesave.c 
 *
 * PROGRAMMER: David M. Johnson
 * 
 * FUNCTIONS:
 *
 * FileSaveAs()
 * ------------
 * This callback function presents a dialog-box with a text 
 * field so that the user may enter a name for the new raster 
 * map file to be created by the save operation. 
 *
 * fsaveOkCB()
 * -----------
 * The OK button callback, this function saves the raster map
 * currently being edited to a new raster map with the name
 * Global.rname by calling the function SegmentToRaster().
 *  
 *
 */

#ifdef SVR4
#include "string.h"
#else
#include "strings.h"
#endif
#include "xgre.h"

/*** LOCAL GLOBALS ***/

Widget fsaveForm;
Widget fsaveText;
Widget fsaveLabel;

/*** LOCAL PROTOTYPES ***/

void fsaveOkCB(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

void fileSaveCancelCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/******************/
/*** FileSaveAs ***/
/******************/

void
#ifdef _NO_PROTO
FileSaveAs(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
FileSaveAs(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Arg al[15];
int ac = 0;
XmString xms;

if (Global.FfileSaveD) return;
Global.FfileSaveD = True;

if (Global.mode == XGRE_UNLOADED)
   {
   XgError(Global.applShell,"No raster map loaded, cannot save.");
   return;
   }

/*** INERACTOR WIDGET ***/

XtSetArg(al[ac],XmNapplyLabelString,XmStringCreateSimple("Save")); ac++;
Global.fileSaveD = XgCreateInteractorDialog(Global.applShell,"FileSave",al,ac);
XtManageChild(XgInteractorGetChild(Global.fileSaveD,XmINTERACT_APPLY_BUTTON));
XtUnmanageChild(XgInteractorGetChild(Global.fileSaveD,XmINTERACT_OK_BUTTON));
XtAddCallback(Global.fileSaveD,XmNapplyCallback,fsaveOkCB,cld);
XtAddCallback(Global.fileSaveD,XmNcancelCallback,fileSaveCancelCB,NULL);

/* FIX: set default button 
XtVaSetValues(Global.fileSaveD,XmNdefaultButton,
   XgInteractorGetChild(Global.fileSaveD,XmINTERACT_APPLY_BUTTON),NULL);*/

/*** TEXT FIELD WIDGET ***/

/* form widget to hold text field and label widgets */
fsaveForm = XtVaCreateManagedWidget("fsaveForm",xmFormWidgetClass,
   Global.fileSaveD,NULL);

/* label widget */
xms = XmStringCreateSimple("Please Enter Save Name for Raster Map:");
fsaveLabel = XtVaCreateManagedWidget("fsaveLabel",
   xmLabelWidgetClass,fsaveForm,
   XmNlabelString,xms,
   XmNtopAttachment,XmATTACH_FORM,
   XmNrightAttachment,XmATTACH_FORM,
   XmNleftAttachment,XmATTACH_FORM,NULL);
XmStringFree(xms);

/* text field widget */
fsaveText = XtVaCreateManagedWidget("fsaveText",
   xmTextFieldWidgetClass,fsaveForm,
   XmNtopAttachment,XmATTACH_WIDGET,
   XmNtopWidget,fsaveLabel,
   XmNbottomAttachment,XmATTACH_FORM,
   XmNrightAttachment,XmATTACH_FORM,
   XmNleftAttachment,XmATTACH_FORM,NULL);
if (Global.rname != NULL) XmTextFieldSetString(fsaveText,Global.rname);

XtManageChild(Global.fileSaveD);
}

/*****************/
/*** fsaveOkCB ***/
/*****************/

void
#ifdef _NO_PROTO
fsaveOkCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
fsaveOkCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
char *result;
char msg[100];

result = XmTextFieldGetString(fsaveText);

if ( result != NULL ) 
   {
   sscanf(result,"%s",Global.rname);
   strcpy(Global.rmapset,G_mapset());
     
   if (G_find_file("cell",Global.rname,Global.rmapset) != NULL)
      {
      /* file already exists */
      sprintf(msg,"Raster Map <%s> already exists, overwrite it?",Global.rname);
      if (XgYesNo(Global.fileSaveD,msg)) 
         {
         /* user said ok to overwrite */
         XtDestroyWidget(w);
         sprintf(msg,"Saving...");
         UpdateModeText(msg);
         XgDoHourGlass(Global.applShell);
         SegmentToRaster();
         XgUndoHourGlass(Global.applShell);
         sprintf(msg,"Saved",Global.rname);
         UpdateModeText(msg);
         }
      else return; /* not ok to overwrite */
      }
   else 
      {
      XtDestroyWidget(w);
      sprintf(msg,"Saving...");
      UpdateModeText(msg);
      XgDoHourGlass(Global.applShell);
      SegmentToRaster();
      XgUndoHourGlass(Global.applShell);
      sprintf(msg,"Saved");
      UpdateModeText(msg);
      }
   }
Global.FfileSaveD = False;
}

/************************/
/*** fileSaveCancelCB ***/
/************************/

void
#ifdef _NO_PROTO
fileSaveCancelCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
fileSaveCancelCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Global.FfileSaveD = False;
XtDestroyWidget(w);
}

