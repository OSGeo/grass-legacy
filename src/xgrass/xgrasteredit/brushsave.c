
/*
 * FILE: brushsave.c 
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 *
 * BrushSave()
 * -----------
 * This function sets up the brush-save dialog box so that the user
 * may set a file-name for the brush (Global.bname).  
 * 
 * brushSaveOkCB()
 * ---------------
 * This funtion is the OK button callback, it calls savebrush() to...
 * 
 * savebrush()
 * -----------
 * Saves the contents of the global brush structure (Global.brush) 
 * and other global brush variables in a file whose name is indicated
 * by Global.bname. 
 *
 */

#ifdef SVR4
#include "string.h"
#else
#include "strings.h"
#endif
#include "xgre.h"

/*** LOCAL GLOBALS ***/

Widget bsaveForm;
Widget bsaveText;
Widget bsaveLabel;

/*** LOCAL PROTOTYPES ***/

int savebrush(
#ifndef _NO_PROTO
#endif
);

void brushSaveOkCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

void brushSaveCancelCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/*****************/
/*** BrushSave ***/
/*****************/

void
#ifdef _NO_PROTO
BrushSave(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
BrushSave(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Arg al[15];
int ac = 0;
XmString xms,xmapset;

if (Global.FbrushSaveD) return;
Global.FbrushSaveD = True;

/*** INTERACTOR WIDGET ***/

XtSetArg(al[ac],XmNapplyLabelString,XmStringCreateSimple("Save")); ac++;
Global.brushSaveD 
   = XgCreateInteractorDialog(Global.applShell,"BrushSave",al,ac);
XtUnmanageChild(XgInteractorGetChild(Global.brushSaveD,XmINTERACT_OK_BUTTON));
XtManageChild(XgInteractorGetChild(Global.brushSaveD,XmINTERACT_APPLY_BUTTON));
XtAddCallback(Global.brushSaveD,XmNapplyCallback,brushSaveOkCB, cld);
XtAddCallback(Global.brushSaveD,XmNcancelCallback,brushSaveCancelCB,NULL);

/*** TEXT FIELD WIDGET ***/

/* form widget to hold text field and label widgets */
bsaveForm = XtVaCreateManagedWidget("bsaveForm",xmFormWidgetClass,
   Global.brushSaveD,NULL);

/* label widget */
xms = XmStringCreateSimple("Please Enter Save Name for Brush:");
bsaveLabel = XtVaCreateManagedWidget("bsaveLabel",
   xmLabelWidgetClass,bsaveForm,
   XmNlabelString,xms,
   XmNtopAttachment,XmATTACH_FORM,
   XmNrightAttachment,XmATTACH_FORM,
   XmNleftAttachment,XmATTACH_FORM,NULL);
XmStringFree(xms);

/* text field widget */
bsaveText = XtVaCreateManagedWidget("bsaveText",
   xmTextFieldWidgetClass,bsaveForm,
   XmNtopAttachment,XmATTACH_WIDGET,
   XmNtopWidget,bsaveLabel,
   XmNrightAttachment,XmATTACH_FORM,
   XmNleftAttachment,XmATTACH_FORM,NULL);
if (Global.bname != NULL) XmTextFieldSetString(bsaveText,Global.bname);

XtManageChild(Global.brushSaveD);
}

/*********************/
/*** brushSaveOkCB ***/
/*********************/

void
#ifdef _NO_PROTO
brushSaveOkCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
brushSaveOkCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
char *result;
char msg[100];

result = XmTextFieldGetString(bsaveText);

if ( *result != NULL ) 
   {
   sscanf(result,"%s",Global.bname);
   strcpy(Global.bmapset,G_mapset());

   if (G_find_file("brush",Global.bname,Global.bmapset) != NULL)
      {
      sprintf(msg,"Brush file <%s> already exists, overwrite it?",Global.bname);
      if (XgYesNo(Global.brushSaveD,msg)) 
         savebrush();
      else
         return;
      }
   else savebrush();
   }
Global.FbrushSaveD = False;
XtDestroyWidget(Global.brushSaveD);

sprintf(msg,"BRUSH: %dx%d (%s)",
   Global.brushRows,Global.brushCols,Global.bname);
UpdateBrushText(msg);
}

/*****************/
/*** savebrush ***/
/*****************/

int savebrush()
{
FILE *bfp;
int bx,by;
char path[200];
char msg[100];

if ((bfp=G_fopen_new("brush",Global.bname)) == NULL)
   {
   sprintf(msg,"Unable to open brush file <%s> for writing",Global.bname);
   XgError(Global.brushSaveD,msg);
   return(-1);
   }

fprintf(bfp,"COLS:%d\n",Global.brushCols);
fprintf(bfp,"ROWS:%d\n",Global.brushRows);
fprintf(bfp,"HCOL:%d\n",Global.brushHotCol);
fprintf(bfp,"HROW:%d\n",Global.brushHotRow);

fprintf(bfp,"BRUSH:\n");
for (by=0; by<Global.brushRows; by++)
   {
   for (bx=0; bx<Global.brushCols; bx++)
      {
      fprintf(bfp,"%d:%d:%d ",
         Global.brush[bx][by].input,
         Global.brush[bx][by].op,
         Global.brush[bx][by].value);
      }
   fprintf(bfp,"\n");
   }

fclose(bfp);
Global.FbrushSaveD = False;
return(0);
}

/*************************/
/*** brushSaveCancelCB ***/
/*************************/

void
#ifdef _NO_PROTO
brushSaveCancelCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
brushSaveCancelCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Global.FbrushSaveD = False;
XtDestroyWidget(w);
}

