
/*
 * FILE: viewindex.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 *
 * ViewIndex()
 * -----------
 * This callback function pops up a new window that shows
 * the entire raster map image, resampled so that it will 
 * fit in am 300x300 pixel area.  The function indexDraw()
 * is called to draw the resampled image and the function
 * outlineDraw() is called to draw a red box around the 
 * portion of the image that is visible in the main image
 * area.  The pop-up window is configured with three buttons:
 * Update, Cancel and Help.  The Update button will call the
 * callback function indexUpdateCB() to update the view-
 * index image.
 *
 * indexDraw()
 * -----------
 * Draws a resampled version of the image in the global 
 * image area (Global.imageArea) in the view-index image 
 * area (indexArea).
 *
 * outlineDraw()
 * -------------
 * Draws a red box in the view-index image area (indexArea)
 * around the portion of the raster map image which is 
 * visible in the main image area (Global.imageArea). 
 * 
 * indexUpdateCB()
 * ---------------
 * This function will redraw the view-index image to reflect
 * any scrollbar movement that may have occured in the main
 * image display area.
 * 
 * indexHandleExposeEvents()
 * -------------------------
 * This event handler handles expose events that occur within
 * the view-index image display area (indexArea).  It redraws the
 * exposed area by putting (XPutImage) the corresponding area
 * of the image structure (indexImage) into the image display 
 * area.
 *
 */

#include "xgre.h"

/*** LOCAL GLOBALS ***/

Widget  indexArea;
XImage *indexImage;
int     indexWidth;
int     indexHeight;
GC      indexGC;

/*** LOCAL FUNCTION PROTOTYPES ***/

int indexDraw(
#ifndef _NO_PROTO
#endif
);

int outlineDraw(
#ifndef _NO_PROTO
#endif
);

void indexUpdateCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

void indexHandleExposeEvents(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XEvent *event,
   Boolean *dispatch
#endif
);

void indexCancelCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/*****************/
/*** ViewIndex ***/
/*****************/

void
#ifdef _NO_PROTO
ViewIndex(w, cld, cad)
   Widget w;
   XtPointer cld, cad;
#else
ViewIndex(Widget w, XtPointer cld, XtPointer cad)
#endif
{
   Widget    indexForm,      /* toplevel form */
             indexLab1,      /* text labels */
             indexSep1;      /* separators */
   XGCValues xgc;
   int       bpp;            /* byte/pixel in image window */
   int       pad;            /* bitmap pad size for image */
   Arg       al[15];
   int       ac = 0;
   XmString  xmsi, oxms, axms;
   Pixel     white = WhitePixel(Global.display, Global.screenNo);
   XWindowAttributes xwa;
   float as_ratio = ((float)Global.imageWidth)/((float)Global.imageHeight);

   if (Global.FviewIndexW) return;
   Global.FviewIndexW = True;

   if (Global.mode == XGRE_UNLOADED) 
      {
      XgError(Global.applShell,
         "You must load a raster map before you may index."); 
      return;
      }

   indexWidth = 300;
   indexHeight = (int)((float)indexWidth) / ((float)as_ratio); 

   /*** CREATE TOPLEVEL WIDGETS AND DRAWABLE ***/
   ac = 0;
   axms = XmStringCreateSimple("Update");
   oxms = XmStringCreateSimple("Dismiss");
   XtSetArg(al[ac],XmNapplyLabelString,axms); ac++;
   XtSetArg(al[ac],XmNokLabelString,oxms); ac++;
   Global.viewIndexW = XgCreateInteractorDialog(Global.applShell,
      "ViewIndex",al,ac);
   XtManageChild(XgInteractorGetChild(Global.viewIndexW,
      XmINTERACT_APPLY_BUTTON)); 
   XtAddCallback(Global.viewIndexW,XmNokCallback,indexCancelCB,NULL);
   XtAddCallback(Global.viewIndexW,XmNapplyCallback,indexUpdateCB,NULL);
   XtAddCallback(Global.viewIndexW,XmNcancelCallback,indexCancelCB,NULL);
   indexForm = XtVaCreateManagedWidget("indexForm",xmFormWidgetClass,
      Global.viewIndexW,NULL);

   /* drawable */
   indexArea = XtVaCreateManagedWidget("indexArea",
      xmDrawingAreaWidgetClass, indexForm,
      XmNbackground, white,
      XmNwidth,indexWidth,
      XmNheight,indexHeight, NULL);
   XtAddEventHandler(indexArea,ExposureMask,False,
      indexHandleExposeEvents,NULL);
   XgAddHelpCallBackFromFile(indexArea,"xgre/xgre_index_area");
   XtManageChild(Global.viewIndexW);

   /*** CREATE XIMAGE ***/

   /* get indexArea, graphics context, window attribute, and pad size */ 
   indexGC = XCreateGC(Global.display,XtWindow(indexArea),0,&xgc);
   if (XGetWindowAttributes(Global.display,XtWindow(indexArea),&xwa)==0)
      {
      XgError(Global.applShell,"Unable to retrieve window attributes.");
      return;
      }
   pad = (xwa.depth > 8) ? 32 : 8;

   /* create it */
   indexImage = XCreateImage(Global.display,xwa.visual,xwa.depth,ZPixmap,
      /*offset=*/0,/*data=*/None,indexWidth,indexHeight,
      pad,/*bytes per Line=*/0);

   /* allocate image space */
   bpp = (Global.image->bits_per_pixel + 7)/8;
   indexImage->data= malloc(indexWidth*indexHeight*bpp);
   if (indexImage->data == NULL) 
      {
      XgError(Global.applShell,"Unable to allocate space for index image.");
      return;
      }

   /* draw the index image and outline the current viewing area */ 
   XgDoHourGlass(Global.applShell);
   indexDraw();
   outlineDraw();
   XgUndoHourGlass(Global.applShell);
}

/*****************/
/*** indexDraw ***/
/*****************/

int indexDraw()
{
   int xi, yi;
   float xscale = ((float)Global.imageWidth)  / ((float)indexWidth);
   float yscale = ((float)Global.imageHeight) / ((float)indexHeight);

   for (yi=0; yi<indexHeight; yi++)
      for (xi=0; xi<indexWidth; xi++)
         {
         unsigned long value;
         int irow = yi*yscale; 
         int icol = xi*xscale;
         value = XGetPixel(Global.image,icol,irow);
         XPutPixel(indexImage,xi,yi,value);
         }
   XPutImage(Global.display,XtWindow(indexArea),indexGC,
      indexImage,0,0,0,0,indexWidth,indexHeight);
}

/*******************/
/*** outlineDraw ***/
/*******************/

int outlineDraw()
{
   Widget    hsb;
   Widget    vsb;
   Widget    clip;
   Dimension viewHeight;
   Dimension viewWidth;
   int       rowOff, colOff;
   int       x1,y1,x2,y2;
   float     xscale = ((float)Global.imageWidth)  / ((float)indexWidth);
   float     yscale = ((float)Global.imageHeight) / ((float)indexHeight);
   Pixel     black  = BlackPixel(Global.display, Global.screenNo);

   /* get scrollbars, scrollbar values, and view area size */
   XtVaGetValues(Global.scrollWindow,
      XmNhorizontalScrollBar,&hsb,
      XmNverticalScrollBar,&vsb,
      XmNclipWindow,&clip,
      NULL);
   XtVaGetValues(hsb,XmNvalue,&colOff,NULL);
   XtVaGetValues(vsb,XmNvalue,&rowOff,NULL);
   XtVaGetValues(clip,XmNwidth,&viewWidth,XmNheight,&viewHeight,NULL);

#  ifdef DEBUG
   printf("outlineDraw: coff=%d roff=%d vh=%d vw=%d\n",
      colOff,rowOff,(int)viewHeight,(int)viewWidth);
#  endif

   /* set draw function to XOR, color, and line width to 2 */
   XSetFunction(Global.display,indexGC,GXcopy);
   XSetForeground(Global.display,indexGC,XgdGetVectColorPixelByName("red"));
   XSetLineAttributes(Global.display,indexGC,2,LineSolid,CapButt,JoinRound);

   /* draw outline box */
   x1 = colOff / xscale;
   y1 = rowOff / yscale;
   x2 = (colOff + (int)viewWidth) / xscale;
   y2 = (rowOff + (int)viewHeight) / yscale;
   XDrawLine(Global.display,XtWindow(indexArea),indexGC,x1,y1,x2,y1);
   XDrawLine(Global.display,XtWindow(indexArea),indexGC,x1,y2,x2,y2);
   XDrawLine(Global.display,XtWindow(indexArea),indexGC,x1,y1,x1,y2);
   XDrawLine(Global.display,XtWindow(indexArea),indexGC,x2,y1,x2,y2);

#  ifdef DEBUG
   printf("outlineDraw: (%d,%d) (%d,%d)\n",x1,y1,x2,y2);
#  endif

   /* reset display function to COPY */
   XSetFunction(Global.display,indexGC,GXcopy);
}

/*********************/
/*** indexUpdateCB ***/
/*********************/

void
#ifdef _NO_PROTO
indexUpdateCB(w, cld, cad)
   Widget w;
   XtPointer cld, cad;
#else
indexUpdateCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
   /* re-put image */
   XPutImage(Global.display,XtWindow(indexArea),indexGC,
      indexImage,0,0,0,0,indexWidth,indexHeight);
   /* redraw outline */
   outlineDraw();
}

/*********************/
/*** indexCancelCB ***/
/*********************/

void
#ifdef _NO_PROTO
indexCancelCB(w, cld, cad)
   Widget w;
   XtPointer cld, cad;
#else
indexCancelCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
   XFreeGC(Global.display,indexGC);
   Global.FviewIndexW = False;
   XtDestroyWidget(w);
}

/*******************************/
/*** indexHandleExposeEvents ***/
/*******************************/

void
#ifdef _NO_PROTO
indexHandleExposeEvents(w, cld, event, dispatch)
   Widget    w;
   XtPointer cld;
   XEvent   *event;
   Boolean  *dispatch;
#else
indexHandleExposeEvents(Widget w,XtPointer cld,XEvent *event,Boolean *dispatch)
#endif
{
   int x, y, width, height, loop, n, ix, iy;

   x      = event->xexpose.x;
   y      = event->xexpose.y;
   width  = event->xexpose.width;
   height = event->xexpose.height;

#  ifdef DEBUG
   printf("Index image expose event (%d,%d) %dx%d\n",x,y,width,height);
#  endif

   XPutImage(Global.display,XtWindow(indexArea),
      Global.imageGC,indexImage,x,y,x,y,width,height);

   outlineDraw();
}
