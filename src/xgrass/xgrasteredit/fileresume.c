/*
 * FILE: fileresume.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 * 
 * FileResume()
 * ------------
 * This callback function presents a browser dialog so that 
 * the user may choose an edit session (aka "segment file") 
 * to resume.
 *
 * FileResumeOK()
 * --------------
 * The OK button callback function, this function opens the
 * segment file selected by the user by calling the function
 * OpenSegmentFile() and then load it into the editor by
 * calling the function LoadImage(). 
 * 
 */

#include "xgre.h"
#ifdef SVR4
#include <string.h>
#else
#include <strings.h>
#endif

/*** LOCAL PROTOTYPES ***/

void FileResumeOk(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad)
#endif
);

void fileResumeCancelCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/******************/
/*** FileResume ***/
/******************/

void
#ifdef _NO_PROTO
FileResume(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
FileResume(Widget w, XtPointer cld, XtPointer cad)
#endif
{
XmString xms, dbe, xmapset;
Arg al[15];
int ac = 0;

if (Global.FfileResumeD) return;
Global.FfileResumeD = True;

/* set up browser widget */
xmapset = XmStringCreateSimple(G_mapset());
xms = XmStringCreateSimple("Select Edit Session to Resume");
dbe = XmStringCreateSimple("seg");
XtSetArg(al[ac], XmNokLabelString,XmStringCreateSimple("Resume")); ac++;
XtSetArg(al[ac], XmNnumLists, 1); ac++;
XtSetArg(al[ac], XmNinitialMapset1,xmapset); ac++;
XtSetArg(al[ac], XmNbrowseMode, XG_USER_DEFINED); ac++;
XtSetArg(al[ac], XmNuserDBElement, dbe); ac++;
XtSetArg(al[ac], XmNselMode, XG_SINGLE_SELECT); ac++;
XtSetArg(al[ac], XmNpromptLabelString, xms); ac++;
Global.fileResumeD 
   = XgCreateBrowserDialog(Global.applShell,"FileResume",al,ac);
XgAddHelpCallBackFromFile(Global.fileResumeD,"xgre/xgre_seg_list");

/* put up label, get rid of apply button */
XtManageChild(
   XgInteractorGetChild(Global.fileResumeD,XmINTERACT_PROMPT_LABEL));
XtUnmanageChild(
   XgInteractorGetChild(Global.fileResumeD,XmINTERACT_APPLY_BUTTON));

/* FIX: set default button
XtVaSetValues(Global.fileResumeD,XmNdefaultButton,
   XgInteractorGetChild(Global.fileResumeD,XmINTERACT_OK_BUTTON),NULL);
*/

XtAddCallback(Global.fileResumeD,XmNokCallback,FileResumeOk,cld);
XtAddCallback(Global.fileResumeD,XmNcancelCallback,fileResumeCancelCB,NULL);

XmStringFree(xms);
XtManageChild(Global.fileResumeD);
}

/********************/
/*** FileResumeOk ***/
/********************/

void
#ifdef _NO_PROTO
FileResumeOk(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
FileResumeOK(Widget w, XtPointer cld, XtPointer cad)
#endif
{
char *result;
char *p1, *p2;
XmString xms;

XtVaGetValues(w, XmNresultString, &xms, NULL);
XmStringGetLtoR(xms,XmSTRING_DEFAULT_CHARSET,&result);

if ( result == NULL ) 
   {
   Global.FfileResumeD = False;
   return;
   }

Global.FfileResumeD = False;

/* if a segment file is already open then close it */
if (Global.mode != XGRE_UNLOADED) 
   {
   CloseSegmentFile();
   KillRasterDialogs();
   }

/* strip off the mapset string */
p1 = (char*)strtok(result,"@"); p2 = (char*)0;
strcpy(Global.segname,p1);

XgDoHourGlass(Global.applShell);
if (OpenSegmentFile())
   {
   XgUndoHourGlass(Global.applShell);
   return;
   } 
if (LoadImage())
   {
   XgUndoHourGlass(Global.applShell);
   return;
   }
SetEditMode(XGRE_LOCKED);
XgUndoHourGlass(Global.applShell);
}

/***************************/
/*** fileResumeCancelCB ***/
/**************************/

void
#ifdef _NO_PROTO
fileResumeCancelCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
fileResumeCancelCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Global.FfileResumeD = False;
XtDestroyWidget(w);
}

