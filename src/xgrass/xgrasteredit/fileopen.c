
/*
 * FILE: open.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 *
 * FileOpen() 
 * ----------
 * This callback funtion presents a browser dialog-box so that the
 * user may choose a raster map to be opened, segmened and loaded
 * into the editor.  The dialog is an XGRASS browser widget with
 * two additional toggle buttons so that the user may choose 
 * to either load the raster map using the raster map's 
 * region (SEG_RASTER_REG) or load the raster map using the 
 * current GRASS region (SEG_CURRENT_REGION).
 *
 * OpenToggleCB()
 * --------------
 * The toggle button callback.  Sets the variable Global.segtype
 * to either SEG_CURRENT_REG or SEG_RASTER_REG. 
 *
 * OpenOkCB() 
 * ----------
 * The OK button callback.  This function segments the raster
 * map selected by the user by calling function RasterToSegment()
 * and then loads it into the editor by calling function LoadImage().
 * 
 */

#include "Browser.h"
#ifdef SVR4
#include "string.h"
#else
#include "strings.h"
#endif 
#include "xgre.h"

/*** LOCAL FUNCTION PROTOTYPES ***/

void OpenOkCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

void OpenToggleCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad)
#endif
);

void fileOpenCancelCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/*** LOCAL GLOBALS ***/

char   oraster, omapset;   /* raster map to be opened */
Widget openForm,           /* toplevel form */
       togLabel,           /* region toggle label */
       openToggle,         /* region toggle */
       openBrowser,        /* raster browser */
       togRadioBox,        /* region toggle radio box*/ 
       togCurrent,         /* region toggle: use XGRE_CURRENT_REG */
       togRaster;          /* region toggle: use XGRE_RASTER_REG */

/****************/
/*** FileOpen ***/
/****************/

void
#ifdef _NO_PROTO
FileOpen(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
FileOpen(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Arg al[15];
int ac = 0;
XmString xms;

if (Global.FfileOpenD) return;
Global.FfileOpenD = True;

/*** INTERACTOR WIDGET ***/

XtSetArg(al[ac], XmNokLabelString,XmStringCreateSimple("Open")); ac++;
Global.fileOpenD = XgCreateInteractorDialog(Global.applShell,"FileOpen",al,ac);
XtAddCallback(Global.fileOpenD,XmNokCallback,OpenOkCB, cld);
XtAddCallback(Global.fileOpenD,XmNcancelCallback,fileOpenCancelCB,NULL);

/* FIX: set default button
XtVaSetValues(Global.fileOpenD,XmNdefaultButton,
   XgInteractorGetChild(Global.fileOpenD,XmINTERACT_OK_BUTTON),NULL);*/

/*** RASTER MAP BROWSER WIDGET ***/

/* form to hold browser and toggle button widgets */
openForm = XtVaCreateManagedWidget("openForm",
   xmFormWidgetClass,Global.fileOpenD,NULL);

/* browser widget */
openBrowser = XtVaCreateManagedWidget("openBrowser",
   browserWidgetClass,openForm,
   XmNpromptLabelString,XmStringCreateSimple("Select Raster Map to Edit"),
   XmNinitialMapset1,XmStringCreateSimple(G_mapset()),
   XmNnumLists, 1,
   XmNbrowseMode, XG_RASTER,
   XmNselMode, XG_SINGLE_SELECT,NULL);
XtUnmanageChild(XgInteractorGetChild(openBrowser,XmINTERACT_OK_BUTTON));
XtUnmanageChild(XgInteractorGetChild(openBrowser,XmINTERACT_CANCEL_BUTTON));
XtUnmanageChild(XgInteractorGetChild(openBrowser,XmINTERACT_HELP_BUTTON));
XtManageChild(XgInteractorGetChild(openBrowser,XmINTERACT_PROMPT_LABEL));
XgAddHelpCallBackFromFile(openBrowser,"xgre/xgre_raster_list");

/*** REGION TOGGLE BUTTONS ***/ 

/* form widget to hold toggle button and label widgets */
openToggle = XtVaCreateManagedWidget("openToggle",
   xmRowColumnWidgetClass, openForm,
   XmNtopAttachment, XmATTACH_WIDGET,
   XmNtopWidget, openBrowser,
   XmNorientation, XmVERTICAL,
   XmNpacking, XmPACK_TIGHT,NULL);

/* label widget */
xms = XmStringCreateSimple("Region to be used:");                
togLabel = XtVaCreateManagedWidget("togLabel",
   xmLabelWidgetClass,openToggle, 
   XmNtopAttachment,XmATTACH_FORM,
   XmNlabelString,xms,NULL); 
XmStringFree(xms);

/* row-colum "radio-box" widget for toggle buttons */ 
togRadioBox = XtVaCreateManagedWidget("togRadioBox",
   xmRowColumnWidgetClass, openToggle,
   XmNtopAttachment,XmATTACH_WIDGET,
   XmNtopWidget,togLabel,
   XmNbottomAttachment,XmATTACH_FORM,
   XmNradioBehavior,True,
   XmNradioBehavior,True,
   XmNorientation,XmVERTICAL,
   XmNpacking,XmPACK_TIGHT,NULL);

/* use-raster-region toggle button */
ac = 0; XtSetArg(al[ac],XmNset,True); ac++;
togRaster = XmCreateToggleButton(togRadioBox,"Region of the Raster Map",al,ac);
XtAddCallback(togRaster, XmNvalueChangedCallback, OpenToggleCB, cld);
XtManageChild(togRaster);
XgAddHelpCallBackFromFile(togRaster,"xgre/xgre_use_rastreg");

/* use-current-region toggle button */
ac = 0; XtSetArg(al[ac],XmNset,False); ac++;
togCurrent = XmCreateToggleButton(togRadioBox,"Current GRASS region",al,ac);
XtAddCallback(togCurrent, XmNvalueChangedCallback, OpenToggleCB, cld);
XtManageChild(togCurrent);
XgAddHelpCallBackFromFile(togCurrent,"xgre/xgre_use_curreg");

XtManageChild(Global.fileOpenD);
}

/****************/
/*** OpenOkCB ***/
/****************/

void
#ifdef _NO_PROTO
OpenOkCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
OpenOkCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    char *result;
    char *p1, *p2;
    XmString xms;

    XtVaGetValues(openBrowser, XmNresultString, &xms, NULL);
    XmStringGetLtoR(xms,XmSTRING_DEFAULT_CHARSET,&result);

    if ( result == NULL ) return; 

    Global.FfileOpenD = False;

    /* if an image is already loaded then close it */
    if (Global.mode != XGRE_UNLOADED) 
       {
       CloseSegmentFile();
       KillRasterDialogs(); 
       }

    /* set globals for raster map name and mapset */
    p1 = (char*)strtok(result,"@"); p2 = (char*)0;
    strcpy(Global.rname,p1);
    p1 = (char*)strtok(p2,"@"); 
    strcpy(Global.rmapset,p1);

    XgDoHourGlass(Global.applShell);
    if (RasterToSegment())
       {
       XgUndoHourGlass(Global.applShell);
       return;
       }
    if (LoadImage())
       {
       XgUndoHourGlass(Global.applShell);
       return;
       }
    XgUndoHourGlass(Global.applShell);
    SetEditMode(XGRE_LOCKED);
}

/********************/
/*** OpenToggleCB ***/
/********************/

void
#ifdef _NO_PROTO
OpenToggleCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
OpenToggleCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
if (w == togRaster) {
   Global.segtype = XGRE_RASTER_REG;
   }
else if (w == togCurrent) {
   Global.segtype = XGRE_CURRENT_REG;
   }
}

/************************/
/*** fileOpenCancelCB ***/
/************************/

void
#ifdef _NO_PROTO
fileOpenCancelCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
fileOpenCancelCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Global.FfileOpenD = False;
XtDestroyWidget(w);
}
