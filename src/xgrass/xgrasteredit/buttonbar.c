
/*
 * FILE: buttonbar.c 
 * 
 * PROGRAMMER: David M. Johnson 
 *
 * FUNCTIONS: 
 *
 * CreateButtonBar()
 * -----------------
 * Creates the buttonbar, a horizontal strip of buttons that lies 
 * below the pull-down menu bar and above the image area.
 *
 * buttonBarCB()
 * -------------
 * This callback function sets the global brush (Global.brush)
 * to the default configuration by calling BuildDefaultBrush().  
 * It sets the number of brush rows and columns (Global.brushRows 
 * and Global.brushCols) to the value passed as call data (cld).
 *
 */

#include "xgre.h"
#include "bmaps.h"

/*** LOCAL PROTOTYPES ***/

void buttonBarCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/***********************/
/*** CreateButtonBar ***/
/***********************/

Widget
#ifdef _NO_PROTO
CreateButtonBar(shell)
    Widget shell;
#else
CreateButtonBar(Widget shell)
#endif
{
Widget buttonBar;
Widget btn1x1;
Widget btn2x2;
Widget btnEdit;
Widget btnCreate;
Widget btnZoom;
Widget btnIndex;
Widget btnHoriz;
Widget btnVerti;
Widget btnBox;
Widget btnPoly;
XmString xms;
Pixmap pixmap;
Pixel fg,bg;

XtVaGetValues(shell,XmNforeground,&fg,XmNbackground,&bg,NULL);

buttonBar = XtVaCreateManagedWidget("buttonBar",
   xmRowColumnWidgetClass,shell,
   XmNorientation,XmHORIZONTAL,NULL);

/* lock button */
pixmap = XCreatePixmapFromBitmapData(Global.display,
   RootWindowOfScreen(Global.screenPtr),lock_bits,
   lock_width,lock_height,
   fg,bg,DefaultDepthOfScreen(Global.screenPtr));
Global.btnLock = XtVaCreateManagedWidget("btnLock",
   xmPushButtonWidgetClass,buttonBar,
   XmNlabelType, XmPIXMAP, XmNlabelPixmap, pixmap,
   NULL);
XtAddCallback(Global.btnLock,XmNactivateCallback,EditLockCB,NULL);

/* 1x1 brush button */
pixmap = XCreatePixmapFromBitmapData(Global.display,
   RootWindowOfScreen(Global.screenPtr),brush1x1_bits,
   brush1x1_width,brush1x1_height,
   fg,bg,DefaultDepthOfScreen(Global.screenPtr));
btn1x1 = XtVaCreateManagedWidget("btn1x1",
   xmPushButtonWidgetClass,buttonBar,
   XmNlabelType, XmPIXMAP, XmNlabelPixmap, pixmap,
   NULL);
XtAddCallback(btn1x1,XmNactivateCallback,buttonBarCB,1 /* 1x1 */);
 
/* 2x2 brush button */
pixmap = XCreatePixmapFromBitmapData(Global.display,
   RootWindowOfScreen(Global.screenPtr),brush2x2_bits,
   brush2x2_width,brush2x2_height,
   fg,bg,DefaultDepthOfScreen(Global.screenPtr));
btn2x2 = XtVaCreateManagedWidget("btn2x2",
   xmPushButtonWidgetClass,buttonBar,
   XmNlabelType, XmPIXMAP, XmNlabelPixmap, pixmap,
   NULL);
XtAddCallback(btn2x2,XmNactivateCallback,buttonBarCB,2 /* 2x2 */);
   
/* brush edit button */
pixmap = XCreatePixmapFromBitmapData(Global.display,
   RootWindowOfScreen(Global.screenPtr),bedit_bits,
   bedit_width,bedit_height,
   fg,bg,DefaultDepthOfScreen(Global.screenPtr));
btnEdit = XtVaCreateManagedWidget("btnEdit",
   xmPushButtonWidgetClass,buttonBar,
   XmNlabelType, XmPIXMAP, XmNlabelPixmap, pixmap,
   NULL);
XtAddCallback(btnEdit,XmNactivateCallback,BrushEditCB,NULL);
  
/* brush create button */ 
pixmap = XCreatePixmapFromBitmapData(Global.display,
   RootWindowOfScreen(Global.screenPtr),bcreate_bits,
   bcreate_width,bcreate_height,
   fg,bg,DefaultDepthOfScreen(Global.screenPtr));
btnCreate = XtVaCreateManagedWidget("btnCreate",
   xmPushButtonWidgetClass,buttonBar,
   XmNlabelType, XmPIXMAP, XmNlabelPixmap, pixmap,
   NULL);
XtAddCallback(btnCreate,XmNactivateCallback,BrushCreate,NULL);
  
/* horizontal contraint button */ 
pixmap = XCreatePixmapFromBitmapData(Global.display,
   RootWindowOfScreen(Global.screenPtr),horiz_bits,
   horiz_width,horiz_height,
   fg,bg,DefaultDepthOfScreen(Global.screenPtr));
btnHoriz = XtVaCreateManagedWidget("btnHoriz",
   xmPushButtonWidgetClass,buttonBar,
   XmNlabelType, XmPIXMAP, XmNlabelPixmap, pixmap, NULL);
XtAddCallback(btnHoriz,XmNactivateCallback,EditHorizontalCB,NULL);
   
/* vertical contraint button */ 
pixmap = XCreatePixmapFromBitmapData(Global.display,
   RootWindowOfScreen(Global.screenPtr),verti_bits,
   verti_width,verti_height,
   fg,bg,DefaultDepthOfScreen(Global.screenPtr));
btnVerti = XtVaCreateManagedWidget("btnVerti",
   xmPushButtonWidgetClass,buttonBar,
   XmNlabelType, XmPIXMAP, XmNlabelPixmap, pixmap, NULL);
XtAddCallback(btnVerti,XmNactivateCallback,EditVerticalCB,NULL);
   
/* polygon-region edit button */ 
pixmap = XCreatePixmapFromBitmapData(Global.display,
   RootWindowOfScreen(Global.screenPtr),poly_bits,
   poly_width,poly_height,
   fg,bg,DefaultDepthOfScreen(Global.screenPtr));
btnPoly = XtVaCreateManagedWidget("btnPoly",
   xmPushButtonWidgetClass,buttonBar,
   XmNlabelType, XmPIXMAP, XmNlabelPixmap, pixmap,
   NULL);
XtAddCallback(btnPoly,XmNactivateCallback,EditPolygonRegionCB,NULL);
   
/* rubber-band-region edit button */ 
pixmap = XCreatePixmapFromBitmapData(Global.display,
   RootWindowOfScreen(Global.screenPtr),rband_bits,
   rband_width,rband_height,
   fg,bg,DefaultDepthOfScreen(Global.screenPtr));
btnBox = XtVaCreateManagedWidget("btnBox",
   xmPushButtonWidgetClass,buttonBar,
   XmNlabelType, XmPIXMAP, XmNlabelPixmap, pixmap,
   NULL);
XtAddCallback(btnBox,XmNactivateCallback,EditBoxRegionCB,NULL);
   
/* view zoom button */ 
pixmap = XCreatePixmapFromBitmapData(Global.display,
   RootWindowOfScreen(Global.screenPtr),zoom_bits,
   zoom_width,zoom_height,
   fg,bg,DefaultDepthOfScreen(Global.screenPtr));
btnZoom = XtVaCreateManagedWidget("btnZoom",
   xmPushButtonWidgetClass,buttonBar,
   XmNlabelType, XmPIXMAP, XmNlabelPixmap, pixmap,
   NULL);
XtAddCallback(btnZoom,XmNactivateCallback,ViewZoom,NULL);
  
/* view index button */ 
pixmap = XCreatePixmapFromBitmapData(Global.display,
   RootWindowOfScreen(Global.screenPtr),index_bits,
   index_width,index_height,
   fg,bg,DefaultDepthOfScreen(Global.screenPtr));
btnIndex = XtVaCreateManagedWidget("btnIndex",
   xmPushButtonWidgetClass,buttonBar,
   XmNlabelType, XmPIXMAP, XmNlabelPixmap, pixmap, NULL);
XtAddCallback(btnIndex,XmNactivateCallback,ViewIndex,NULL);
   
return buttonBar;
}

/*******************/
/*** buttonBarCB ***/
/*******************/

void
#ifdef _NO_PROTO
buttonBarCB(w, cld, cad)
Widget w;
int cld; 
XtPointer cad;
#else
buttonBarCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Global.brushRows = cld;
Global.brushCols = cld;
Global.brushHotRow = 0;
Global.brushHotCol = 0;
BuildDefaultBrush();
}

