
/* 
 * FILE: viewzoom.c
 *
 * PROGRAMMER: David M. Johnson 
 * 
 * FUNCTIONS:
 *
 * ViewZoom()
 * ----------
 * Sets up a zoom window with a drawable for the zoom image, a
 * scale for setting the magnification, a Done button and a
 * quit button.  The event handlers for this window are 
 * zoomHandleMouseEvents() and zoomHandleExposeEvents().  
 * These functions can be found in the file zoomhandler.c
 * The zoom window is configured with a slider-bar (aka 
 * "scale") so that the user can adjust the magnification,
 * a Redraw button, a Cancel button, and a Help button. 
 *
 * zoomDraw()
 * ----------
 * This function, which is called by zoomHandleMouseEvents()
 * when the user sets or resets the center point of the zoom
 * view, draws a magnified or "zoomed" view of the current
 * raster map centered on the zoom center coordinates 
 * (Global.zoomX,Global.zoomY) and the zoom magnification
 * (Global.zoomMag).
 * 
 * zoomScaleCB()
 * -------------
 * This is the callback for the magnification slider-bar.  
 * This function sets the variable Global.zoomMag to match
 * the value displayed on the slider-bar. 
 *
 * zoomRedrawCB()
 * --------------
 * This function calls RedrawImage() in loadimage.c and 
 * zoomDraw() to redraw both the main image area (Global.imageArea)
 * and the zoom image area (Global.zoomArea).
 *
 * HighlightZoomPixel()
 * --------------------
 * This function highlights the pixel (px,py) in the zoom 
 * image area (Global.zoomArea) by putting the pixel value
 * of the global highlight color (Global.highlight) into it.
 *
 */ 

#include "xgre.h"

/*** LOCAL FUNCTION PROTOTYPES ***/

void zoomDraw(
#ifndef _NO_PROTO
#endif
);

void zoomScaleCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XmScaleCallbackStruct *cad
#endif
);

void zoomRedrawCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

void zoomCancelCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

int HighlightZoomPixel(
#ifndef _NO_PROTO
   int px,
   int py
#endif
);

/****************/
/*** ViewZoom ***/
/****************/

void
#ifdef _NO_PROTO
ViewZoom(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
ViewZoom(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Widget zoomForm,       /* toplevel form */
       zoomScale,      /* magnification slider */
       zoomLab1,       /* text labels */
       zoomLab2,
       zoomLab3,
       zoomSep1,       /* separators */
       zoomSep2;
int    bpp;            /* byte/pixel in image window */
int    pad;            /* bitmap pad size for image */
Arg    al[15];
int    ac = 0;
XmString xms, axms, oxms;
XWindowAttributes xwa;
Pixel white = WhitePixel(Global.display, Global.screenNo);

if (Global.FviewZoomW) return;
Global.FviewZoomW = True;

if (Global.mode == XGRE_UNLOADED) 
   {
   XgError(Global.applShell,
     "You must open a raster map\nbefore you can zoom"); 
   Global.FviewZoomW = False;
   return;
   }
 
if (Global.mode != XGRE_LOCKED) SetEditMode(XGRE_NORMAL);
Global.zoomWidth  = 300;
Global.zoomHeight = 300;
Global.zoomMag    = 2.0;
Global.zoomLoaded = False; 
Global.zoomRunning = True; 

/*** CREATE TOPLEVEL WIDGETS AND DRAWABLE ***/

ac = 0;
oxms = XmStringCreateSimple("Dismiss");
axms = XmStringCreateSimple("Redraw");
XtSetArg(al[ac],XmNokLabelString,oxms); ac++;
XtSetArg(al[ac],XmNapplyLabelString,axms); ac++;
Global.viewZoomW = XgCreateInteractorDialog(Global.applShell,"ViewZoom",al,ac);
XtManageChild(XgInteractorGetChild(Global.viewZoomW,XmINTERACT_APPLY_BUTTON));
XtAddCallback(Global.viewZoomW,XmNapplyCallback,zoomRedrawCB,NULL);
XtAddCallback(Global.viewZoomW,XmNokCallback,zoomCancelCB,NULL);
XtAddCallback(Global.viewZoomW,XmNcancelCallback,zoomCancelCB,NULL);
zoomForm = XtVaCreateManagedWidget("zoomForm",
   xmFormWidgetClass,Global.viewZoomW,NULL);
XgAddHelpCallBackFromFile(
   XgInteractorGetChild(Global.viewZoomW,XmINTERACT_APPLY_BUTTON),
   "xgre/xgre_zoom_redraw");

/* label line one */
xms =  XmStringCreateSimple("Click left mouse button on main");
zoomLab1 = XtVaCreateManagedWidget("lab1",
   xmLabelWidgetClass,zoomForm,
   XmNtopAttachment,XmATTACH_FORM,
   XmNleftAttachment,XmATTACH_FORM,
   XmNrightAttachment,XmATTACH_FORM,
   XmNlabelString,xms,NULL);
XmStringFree(xms);

/* label line two */
xms =  XmStringCreateSimple("image to set the zoom center");
zoomLab2 = XtVaCreateManagedWidget("lab2",xmLabelWidgetClass,zoomForm,
   XmNleftAttachment,XmATTACH_FORM,
   XmNrightAttachment,XmATTACH_FORM,
   XmNtopAttachment,XmATTACH_WIDGET,
   XmNtopWidget,zoomLab1,
   XmNlabelString,xms,NULL);
XmStringFree(xms);

zoomSep1 = XtVaCreateManagedWidget("sep1",xmSeparatorWidgetClass,zoomForm,
   XmNleftAttachment,XmATTACH_FORM,
   XmNrightAttachment,XmATTACH_FORM,
   XmNtopAttachment,XmATTACH_WIDGET,
   XmNtopWidget,zoomLab2,NULL);

/* drawable */
Global.zoomArea = XtVaCreateManagedWidget("zoomArea",
   xmDrawingAreaWidgetClass, zoomForm,
   XmNtopAttachment,XmATTACH_WIDGET,
   XmNtopWidget,zoomSep1,
   XmNbackground, white,
   XmNwidth,Global.zoomWidth,
   XmNheight,Global.zoomHeight,NULL);
XgAddHelpCallBackFromFile(Global.zoomArea,"xgre/xgre_zoom_area");

/*** CREATE MAGNIFICATION SCALE ***/

/* separator */
zoomSep2 = XtVaCreateManagedWidget("sep2",xmSeparatorWidgetClass,zoomForm,
   XmNleftAttachment,XmATTACH_FORM,
   XmNrightAttachment,XmATTACH_FORM,
   XmNtopAttachment,XmATTACH_WIDGET,
   XmNtopWidget,Global.zoomArea,NULL);

/* label */
xms =  XmStringCreateSimple("Magnification factor");
zoomLab3 = XtVaCreateManagedWidget("lab3",xmLabelWidgetClass,zoomForm,
   XmNleftAttachment,XmATTACH_FORM,
   XmNrightAttachment,XmATTACH_FORM,
   XmNtopAttachment,XmATTACH_WIDGET,
   XmNtopWidget,zoomSep2,
   XmNlabelString,xms,NULL);
XmStringFree(xms);

/* scale */
zoomScale = XtVaCreateManagedWidget("zoomScale",xmScaleWidgetClass,zoomForm,
   XmNbottomAttachment,XmATTACH_FORM,
   XmNleftAttachment,XmATTACH_FORM,
   XmNrightAttachment,XmATTACH_FORM,
   XmNtopAttachment,XmATTACH_WIDGET,
   XmNtopWidget,zoomLab3,
   XmNorientation,XmHORIZONTAL,
   XmNprocessingDirection,XmMAX_ON_RIGHT,
   XmNmaximum,100,
   XmNminimum,11,
   XmNdecimalPoints,1,
   XmNvalue,(int)(10*Global.zoomMag),
   XmNshowValue,True,NULL);
XtAddCallback(zoomScale,XmNvalueChangedCallback,zoomScaleCB,NULL);
XgAddHelpCallBackFromFile(zoomScale,"xgre/xgre_zoom_mag");

XtManageChild(Global.viewZoomW);

/*** CREATE XIMAGE ***/

/* get zoomArea window attribute and figure pad size */ 
if (XGetWindowAttributes(Global.display,XtWindow(Global.zoomArea),&xwa)==0)
   {
   XgError(Global.applShell,"Unable to retrieve window attributes.");
   return;
   }
pad = (xwa.depth > 8) ? 32 : 8;

/* create it */
Global.zoomImage = XCreateImage(Global.display,xwa.visual,xwa.depth,ZPixmap,
   /*offset=*/0,/*data=*/None,Global.zoomWidth,Global.zoomHeight,
   pad,/*bytes per Line=*/0);

/* allocate image space */
bpp = (Global.image->bits_per_pixel + 7)/8;
Global.zoomImage->data= malloc(Global.zoomWidth*Global.zoomHeight*bpp);
if (Global.zoomImage->data == NULL) 
   {
   XgError(Global.applShell,"Unable to allocate space for zoom image.");
   return;
   }

/*** SETUP EVENT HANDLERS ***/ 

/* disable normal imageArea event handler */
XtRemoveEventHandler(Global.imageArea,XtAllEvents,True,
   HandleMouseEvents,XtGetMultiClickTime(Global.display));

XtAddEventHandler(Global.imageArea,
   PointerMotionMask|ButtonPressMask|ButtonReleaseMask,False,
   zoomHandleMouseEvents,XtGetMultiClickTime(Global.display));

XtAddEventHandler(Global.zoomArea,
   PointerMotionMask|ButtonPressMask|ButtonReleaseMask,False,
   zoomHandleMouseEvents,XtGetMultiClickTime(Global.display));

XtAddEventHandler(Global.zoomArea,ExposureMask,False,
   zoomHandleExposeEvents,NULL);

/*** SETUP ZOOM MODE CURSORS ***/

DoZoomCursor(Global.imageArea);
if (Global.mode != XGRE_LOCKED) DoBrushCursor(Global.zoomArea);
}

/****************/
/*** zoomDraw ***/
/****************/

void zoomDraw()
{
int xi, yi;

Global.zoomColOff = Global.zoomX - (Global.zoomWidth/2)/Global.zoomMag;
Global.zoomRowOff = Global.zoomY - (Global.zoomHeight/2)/Global.zoomMag;

for (yi=0; yi<Global.zoomHeight; yi++)
   for (xi=0; xi<Global.zoomWidth; xi++)
      {
      unsigned long value;
      int irow = Global.zoomRowOff + yi/Global.zoomMag;
      int icol = Global.zoomColOff + xi/Global.zoomMag;
      if (irow>Global.imageHeight || icol>Global.imageWidth)
         value = 0;
      else if (irow<0 || icol<0)
         value = 0;
      else 
         value = XGetPixel(Global.image,icol,irow);
      XPutPixel(Global.zoomImage,xi,yi,value);
      }

XPutImage(Global.display,XtWindow(Global.zoomArea),Global.imageGC,
   Global.zoomImage,0,0,0,0,Global.zoomWidth,Global.zoomHeight);

Global.zoomLoaded = True;
}

/**************************/
/*** HighlightZoomPixel ***/
/**************************/

int
#ifdef _NO_PROTO
HighlightZoomPixel(px,py)
   int px,py;
#else
HighlightZoomPixel(int px, int py)
#endif
{
int x1,y1,x2,y2; 

if (!(px>Global.seghd.cols || px<0 || py>Global.seghd.rows || py<0))
   {
   x1 = Global.zoomMag*(px - Global.zoomColOff);
   y1 = Global.zoomMag*(py - Global.zoomRowOff);
   x2 = Global.zoomMag*(px + 1 - Global.zoomColOff);
   y2 = Global.zoomMag*(py + 1 - Global.zoomRowOff);
   XSetForeground(Global.display,Global.imageGC,Global.highlight);
   XFillRectangle(Global.display,XtWindow(Global.zoomArea),
      Global.imageGC,x1,y1,x2-x1,y2-y1);

   /*XPutPixel(Global.zoomImage,cx,cy,Global.highlight);
   XPutImage(Global.display,XtWindow(Global.zoomArea),
      Global.imageGC,Global.zoomImage,cx,cy,cx,cy,1,1);*/
   }
return(0);
}

/*******************/
/*** zoomScaleCB ***/
/*******************/

void 
#ifdef _NO_PROTO
zoomScaleCB(w,cld,cad)
Widget w;
XtPointer cld;
XmScaleCallbackStruct *cad;
#else
zoomScaleCB(Widget w, XtPointer cld, XmScaleCallbackStruct *cad)
#endif
{
float zw = Global.zoomWidth;
float zh = Global.zoomHeight;
int x = Global.zoomX;
int y = Global.zoomY;

Global.zoomMag = ((float)cad->value)/((float)10);

if (!Global.zoomLoaded) return;

/*** RESET X CENTER OF ZOOM ***/

/* zoom image too narrow to fill zoom window */
if (Global.imageWidth*Global.zoomMag < zw)
   Global.zoomX = Global.imageWidth/2.0;

/* zoom center too far left */
else if (x < (int)(zw/(2.0*Global.zoomMag)))
   Global.zoomX = (int)(zw/(2.0*Global.zoomMag));

/* zoom center too far right */
else if (x > (Global.imageWidth - (int)(zw/(2.0*Global.zoomMag))))
   Global.zoomX = Global.imageWidth - (int)(zw/(2.0*Global.zoomMag));

/*** RESET Y CENTER OF ZOOM ***/

/* zoom image too short to fill zoom window */
if (Global.imageHeight*Global.zoomMag < zh)
   Global.zoomY = Global.imageHeight/2.0;

/* zoom center too far up */
else if (y < (int)(zh/(2.0*Global.zoomMag)))
   Global.zoomY = (int)(zh/(2.0*Global.zoomMag));

/* zoom image too far down */
else if (y > (Global.imageHeight - (int)(zh/(2.0*Global.zoomMag))))
   Global.zoomY = Global.imageHeight - (int)(zh/(2.0*Global.zoomMag));

#ifdef DEBUG
printf("z scale: mag=%f (%d,%d)\n",Global.zoomMag,Global.zoomX,Global.zoomY);
#endif

zoomDraw();
}

/********************/
/*** zoomRedrawCB ***/
/********************/

void
#ifdef _NO_PROTO
zoomRedrawCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
zoomRedrawCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
if (!Global.zoomLoaded)
   {
   XgError(Global.applShell,"Zoom display not loaded");
   return;
   }
RedrawImage(w,cld,cad);
zoomDraw();
}

/********************/
/*** zoomCancelCB ***/
/********************/

void
#ifdef _NO_PROTO
zoomCancelCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
zoomCancelCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
/*** RESET THE EVENT HANDLERS TO NORMAL MODE ***/

XtRemoveEventHandler(Global.imageArea,XtAllEvents,True,
   zoomHandleMouseEvents,XtGetMultiClickTime(Global.display));

XtRemoveEventHandler(Global.zoomArea,XtAllEvents,True,
   zoomHandleMouseEvents,XtGetMultiClickTime(Global.display));

XtRemoveEventHandler(Global.zoomArea,XtAllEvents,True,
   zoomHandleExposeEvents,NULL);

XtAddEventHandler(Global.imageArea, 
   PointerMotionMask|ButtonPressMask|ButtonReleaseMask,False,
   HandleMouseEvents,XtGetMultiClickTime(Global.display));

XtDestroyWidget(w);

Global.FviewZoomW = False;
Global.zoomRunning = False;
Global.zoomLoaded = False;
UndoCursor(Global.imageArea);
if (Global.mode != XGRE_LOCKED) 
   {
   DoBrushCursor(Global.imageArea);
   SetEditMode(XGRE_NORMAL);
   }
}

