
/*
 * FILE: menubar.c 
 * 
 * CreateMenuBar()
 * ---------------
 * This function creates the raster-editor's pull-down 
 * menu bar and associates a callback function with each
 * menu option.
 *
 */

#include "xgre.h"

/****************/
/*** CancelCB ***/
/****************/

void
#ifdef _NO_PROTO
CancelCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
CancelCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
XtDestroyWidget(w);
}

void
#ifdef _NO_PROTO
PBAct(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
PBAct(Widget w, XtPointer cld, XtPointer cad)
#endif
{
if ( !strcmp((char *)cld,"Quit"))
   {
   if (Global.mode != XGRE_UNLOADED) CloseSegmentFile();
   exit(0);
   }
printf("%s Activated\n",(char *)cld);
}


/*********************/
/*** CreateMenuBar ***/
/*********************/

Widget
#ifdef _NO_PROTO
CreateMenuBar(shell)
    Widget shell;
#else
CreateMenuBar(Widget shell)
#endif
{
Widget menuBar;
Widget fileMenu;
Widget viewMenu;
Widget brushMenu;
Widget editMenu;
Widget helpMenu;
Widget cascade;
Widget widget;
XmString xms;

menuBar = XmCreateMenuBar(shell, "menu_bar", NULL, 0);

/*** FILE MENU ***/

fileMenu = XmCreatePulldownMenu(menuBar, "_pulldown", NULL, 0);
xms = XmStringCreateSimple("File");
cascade = XtVaCreateManagedWidget("file",
   xmCascadeButtonGadgetClass, menuBar,
   XmNsubMenuId, fileMenu,
   XmNlabelString, xms,
   XmNmnemonic, 'F',
   NULL);
XmStringFree(xms);

/* open */
widget = XtVaCreateManagedWidget("Open...", 
   xmPushButtonWidgetClass, fileMenu, NULL);
XtAddCallback(widget,XmNactivateCallback,
   FileOpen,(XtPointer)"Open...");

/* resume */
widget = XtVaCreateManagedWidget("Resume...", 
   xmPushButtonWidgetClass, fileMenu, NULL);
XtAddCallback(widget,XmNactivateCallback,FileResume,(XtPointer)"Resume...");

/* save */
widget = XtVaCreateManagedWidget("Save...", xmPushButtonWidgetClass,
   fileMenu, NULL);
XtAddCallback(widget,XmNactivateCallback,FileSaveAs,(XtPointer)"Save...");

/* close */
widget = XtVaCreateManagedWidget("Close", xmPushButtonWidgetClass,
   fileMenu, NULL);
XtAddCallback(widget,XmNactivateCallback,FileClose,(XtPointer)"Close");

/* close and remove */
widget = XtVaCreateManagedWidget("Close and Remove",xmPushButtonWidgetClass,
   fileMenu, NULL);
XtAddCallback(widget,XmNactivateCallback,FileRemove,(XtPointer)"Remove");

widget=XtVaCreateManagedWidget("sep",xmSeparatorWidgetClass,fileMenu,NULL);

/* quit */
widget = XtVaCreateManagedWidget("Quit", xmPushButtonWidgetClass,
   fileMenu, NULL);
XtAddCallback(widget, XmNactivateCallback, PBAct, (XtPointer)"Quit");

/*** END FILE MENU ***/

/*** VIEW MENU ***/

viewMenu = XmCreatePulldownMenu(menuBar, "_pulldown", NULL, 0);

xms = XmStringCreateSimple("View");
cascade = XtVaCreateManagedWidget("view",
   xmCascadeButtonGadgetClass, menuBar,
   XmNsubMenuId, viewMenu,
   XmNlabelString, xms,
   XmNmnemonic, 'V',NULL);
XmStringFree(xms);

/* zoom */
widget = XtVaCreateManagedWidget("Zoom...", xmPushButtonWidgetClass,
   viewMenu, NULL);
XtAddCallback(widget,XmNactivateCallback,
   ViewZoom,(XtPointer)"Zoom...");

/* Index */
widget = XtVaCreateManagedWidget("Index...", xmPushButtonWidgetClass,
   viewMenu, NULL);
XtAddCallback(widget, XmNactivateCallback, 
   ViewIndex, (XtPointer)"Index");

/* Redraw */
widget = XtVaCreateManagedWidget("Redraw", 
   xmPushButtonWidgetClass, viewMenu, NULL);
XtAddCallback(widget,XmNactivateCallback,RedrawImage,NULL);

/* BRUSH MENU */

brushMenu = XmCreatePulldownMenu(menuBar, "_pulldown", NULL, 0);

xms = XmStringCreateSimple("Brush");
cascade = XtVaCreateManagedWidget("brush",
    xmCascadeButtonGadgetClass, menuBar,
    XmNsubMenuId, brushMenu,
    XmNlabelString, xms,
    XmNmnemonic, 'B', NULL);
XmStringFree(xms);

/* category value */
widget = XtVaCreateManagedWidget("Category...", 
   xmPushButtonWidgetClass, brushMenu, NULL);
XtAddCallback(widget,XmNactivateCallback,BrushCat,(XtPointer)"Category");

/* highligh color */
widget = XtVaCreateManagedWidget("Color...", 
   xmPushButtonWidgetClass, brushMenu, NULL);
XtAddCallback(widget,XmNactivateCallback,BrushColor,(XtPointer)"Highlight");

/* edit */
widget = XtVaCreateManagedWidget("Edit...", 
   xmPushButtonWidgetClass, brushMenu, NULL);
XtAddCallback(widget,XmNactivateCallback,BrushEditCB,(XtPointer)"Edit");

/* create */
widget = XtVaCreateManagedWidget("Create...", 
   xmPushButtonWidgetClass, brushMenu, NULL);
XtAddCallback(widget,XmNactivateCallback,BrushCreate,(XtPointer)"Create");

/* load brush */
widget = XtVaCreateManagedWidget("Load...", 
   xmPushButtonWidgetClass,brushMenu, NULL);
XtAddCallback(widget,XmNactivateCallback,BrushLoad,(XtPointer)"Load");

/* save brush */
widget = XtVaCreateManagedWidget("Save...", 
    xmPushButtonWidgetClass, brushMenu, NULL);
XtAddCallback(widget, XmNactivateCallback, BrushSave, (XtPointer)"Save");

/* EDIT MENU */

editMenu = XmCreatePulldownMenu(menuBar,"_pulldown",NULL,0);

xms = XmStringCreateSimple("Edit");
cascade = XtVaCreateManagedWidget("edit",
    xmCascadeButtonGadgetClass, menuBar,
    XmNsubMenuId, editMenu,
    XmNlabelString, xms,
    XmNmnemonic, 'E', NULL);
XmStringFree(xms);

/* Lock */
Global.mbtnLock = XtVaCreateManagedWidget("Lock", 
   xmPushButtonWidgetClass,editMenu,NULL);
XtAddCallback(Global.mbtnLock,XmNactivateCallback,EditLockCB,NULL);

/* Undo */
widget = XtVaCreateManagedWidget("Undo", 
   xmPushButtonWidgetClass,editMenu,NULL);
XtAddCallback(widget,XmNactivateCallback,EditUndoCB,NULL);

/* Constrain-Vertical */
Global.mbtnVerti = XtVaCreateManagedWidget("Constrain-Vertical", 
   xmPushButtonWidgetClass,editMenu,NULL);
XtAddCallback(Global.mbtnVerti,XmNactivateCallback,
   EditVerticalCB,(XtPointer)"Constrain-Vertical");

/* Constrain-Horizontal */
Global.mbtnHoriz = XtVaCreateManagedWidget("Constrain-Horizontal", 
   xmPushButtonWidgetClass,editMenu,NULL);
XtAddCallback(Global.mbtnHoriz,XmNactivateCallback,
   EditHorizontalCB,(XtPointer)"Constrain-Horizontal");
 
/* Region-Box */
Global.mbtnBox = XtVaCreateManagedWidget("Region-Box", 
   xmPushButtonWidgetClass,editMenu,NULL);
XtAddCallback(Global.mbtnBox,XmNactivateCallback,
   EditBoxRegionCB,(XtPointer)"Region-Box");
 
/* Region-Polygon */
Global.mbtnPoly = XtVaCreateManagedWidget("Region-Polygon", 
   xmPushButtonWidgetClass,editMenu,NULL);
XtAddCallback(Global.mbtnPoly,XmNactivateCallback,
   EditPolygonRegionCB,(XtPointer)"Region-Polygon");
 
/* Categories... */
widget = XtVaCreateManagedWidget("Categories...", 
   xmPushButtonWidgetClass,editMenu,NULL);
XtAddCallback(widget,XmNactivateCallback,
   EditCats,(XtPointer)"Categories");

/*** HELP MENU ***/

helpMenu = XmCreatePulldownMenu(menuBar, "_pulldown", NULL, 0);

xms = XmStringCreateSimple("Help");
cascade = XtVaCreateManagedWidget("help",
   xmCascadeButtonGadgetClass, menuBar,
   XmNsubMenuId, helpMenu,
   XmNlabelString, xms,
   XmNmnemonic, 'H',NULL);
XmStringFree(xms);
 
XtVaSetValues(menuBar, XmNmenuHelpWidget, cascade, NULL);

/*** general ***/
widget = XtVaCreateManagedWidget("General...", xmPushButtonWidgetClass,
   helpMenu, NULL);
XtAddCallback(widget,XmNactivateCallback,HelpGeneral,(XtPointer)"GeneralHelp");

/*** current mode ***/
widget = XtVaCreateManagedWidget("Current Mode...", xmPushButtonWidgetClass,
   helpMenu, NULL);
XtAddCallback(widget,XmNactivateCallback,HelpModal,(XtPointer)"ModalHelp");

/*** END HELP MENU ***/

XtManageChild(menuBar);

return menuBar;
}

