
/*
 * FILE: loadimage.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 *
 * LoadImage()
 * -----------
 * This function loads the raster map which is stored in the
 * global segment file (Global.seg) into the global X-Windows
 * image structure (Global.image).  It then puts the image
 * into the global, scrollable and drawable image 
 * area (Global.imageArea).
 *
 * HighlightImagePixel(px,py)
 * --------------------------
 * This function highlights the pixel (px,py) in the global
 * image area (Global.imageArea) by putting the pixel value
 * of the global highlight color (Global.highlight) into it.
 *
 * RedrawImage()
 * -------------
 * This function redraws the portion of the raster map that
 * has been modified since the last LoadImage() or RedrawImage() 
 * call by re-reading the modified pixels (cells) from the global
 * segment file (Global.seg), putting them into an X-Windows
 * image structure, and then putting that image structure into
 * the global image area (Global.imageArea).
 *
 * The area that has been modified is defined by the points
 * (Global.modX1,Global.modY1) and (Global.modX2,Global.modY2).
 * Whenever a change is made to a raster map cell these 
 * "modification boundary" points are updated (see ApplyBrush() 
 * in applybrush.c).  

 * RedrawImage() finishes by reseting these points and clearing 
 * the undo-buffer (ClearUndoBuffer() in editundo.c).
 *
 */

#include "xgre.h"

/*****************/
/*** LoadImage ***/
/*****************/

int LoadImage()
{
   XWindowAttributes xwa;
   CELL cmin,cmax;
   int ix,iy;
   int xlim,ylim;
   int bpp;                /* byte/pixel in image window */
   int pad;                /* bitmap pad size for image */
   char *buf;

   buf = (char*)malloc(Global.seghd.cols * sizeof(CELL));

/*** DETERMINE IMAGE SIZE AND DEPTH ***/

   if (XGetWindowAttributes(Global.display,XtWindow(Global.imageArea),&xwa)==0)
      {
      XgError(Global.applShell,"Unable to retrieve window attributes");
      return (-1);
      }
   pad = (xwa.depth > 8) ? 32 : 8;

/*** CREATE IMAGE AND ALLOCATE SPACE FOR IT ***/ 

   /* create graphics context */
   Global.imageWidth = Global.seghd.cols;
   Global.imageHeight = Global.seghd.rows;

   /* create and allocate memory for image struture */
   Global.image= XCreateImage(Global.display,xwa.visual,xwa.depth,
      ZPixmap,0,None,Global.imageWidth,Global.imageHeight,pad,0);
   bpp = (Global.image->bits_per_pixel + 7)/8;     
   Global.image->data= malloc(Global.imageWidth*Global.imageHeight*bpp); 
   if (Global.image->data == NULL)
      {
      XgError(Global.applShell,"Unable to allocate space for image"); 
      return(-1);
      }

   /* setup color lookup-table */
   AllocColors(Global.obj);

/*** PUT RASTER MAP PIXELS INTO IMAGE ***/

   G_get_color_range (&cmin,&cmax,&(Global.colors));
   /* FIX: is there a better way to make sure that 0 gets a color? */
   if (cmin > 0) cmin = 0;
   if (cmax < 0) cmax = 0;

   xlim = Global.imageWidth;
   ylim = Global.imageHeight;
   buf = (char*)G_malloc(Global.seghd.cols*sizeof(CELL));
   segment_flush(&(Global.seg));
   for (iy = 0; iy < ylim; iy++) /* rows */
      {
      CELL *vptr; 
      if (segment_get_row(&(Global.seg),buf,iy) < 0)
         {
         XgError(Global.applShell,"Error reading segment file row.");
         return(-1);
         }
      vptr = (CELL*)buf;
      for (ix = 0; ix < xlim; ix++) /* columns */
         {
         XPutPixel(Global.image,ix,iy,
            Global.obj->Obj.GeoFrame.lookup_tbl[*(vptr++)-cmin]);
         }
      }

/** PUT IMAGE INTO THE DRAWING AREA ***/

#  ifdef DEBUG
   printf("loadimage: loaded! w=%d h=%d\n",
      Global.imageWidth,Global.imageHeight);
#  endif
   XtVaSetValues(Global.imageArea,XmNwidth,Global.imageWidth,
      XmNheight,Global.imageHeight,NULL);
   XPutImage(Global.display,XtWindow(Global.imageArea),
      Global.imageGC,Global.image,0,0,0,0,
      Global.imageWidth,Global.imageHeight);

   Global.modX1 = Global.imageWidth; 
   Global.modY1 = Global.imageHeight;
   Global.modX2 = 0;
   Global.modY2 = 0; 

   return(0);
}

/***************************/
/*** HighlightImagePixel ***/
/***************************/

int 
#ifdef _NO_PROTO
HighlightImagePixel(px,py) int px,py;
#else
HighlightImagePixel(int px,int py)
#endif
{
if (!(px>Global.seghd.cols || px<0 || py>Global.seghd.rows || py<0))
   {
   /* put pixel into image */
   XPutPixel(Global.image,px,py,Global.highlight);

   /* put that one image pixel into the image drawable area */
   XPutImage(Global.display,XtWindow(Global.imageArea),
      Global.imageGC,Global.image,px,py,px,py,1,1);
   }

if (px < Global.modX1) Global.modX1 = px;
if (py < Global.modY1) Global.modY1 = py;

if (px > Global.modX2) Global.modX2 = px;
if (py > Global.modY2) Global.modY2 = py;

return(0);
}

/*******************/
/*** RedrawImage ***/
/*******************/

void
#ifdef _NO_PROTO
RedrawImage(w, cld, cad)
   Widget w;
   XtPointer cld, cad;
#else
RedrawImage(Widget w, XtPointer cld, XtPointer cad)
#endif
{
   int ix,iy,xlim,ylim;
   CELL cmin,cmax;
   char *buf; 

if (Global.mode == XGRE_UNLOADED) 
   {
   XgError(Global.applShell,"There is no raster map to redraw");
   return;
   }

/*** PUT RASTER MAP PIXELS INTO IMAGE ***/

   AllocColors(Global.obj);
   G_get_color_range (&cmin,&cmax,&(Global.colors));
   /* FIX: is there a better way to make sure that 0 gets a color? */
   if (cmin > 0) cmin = 0;
   if (cmax < 0) cmax = 0;

   buf = (char*)G_malloc(Global.seghd.cols*sizeof(CELL));
   segment_flush(&(Global.seg));

    /* do it by reading rows from the segment file */
   for (iy = Global.modY1; iy <= Global.modY2; iy++)  
      {
      CELL *vptr;
      if (segment_get_row(&(Global.seg),buf,iy) < 0)
         {
         XgError(Global.applShell,"Error reading segment file row.");
         return;
         }
      vptr = (CELL*)buf;
      for (ix = Global.modX1; ix <= Global.modX2; ix++) 
         {
         XPutPixel(Global.image,ix,iy,
            Global.obj->Obj.GeoFrame.lookup_tbl[vptr[ix]-cmin]);
         }
      }

/*** PUT IMAGE INTO THE DRAWING AREA ***/

   XPutImage(Global.display,XtWindow(Global.imageArea),
      Global.imageGC, Global.image, 
      Global.modX1, Global.modY1, Global.modX1, Global.modY1,
      Global.modX2 - Global.modX1 + 1, Global.modY2 - Global.modY1 + 1);

   Global.modX1 = Global.imageWidth;
   Global.modY1 = Global.imageHeight;
   Global.modX2 = 0;
   Global.modY2 = 0;

   /* update zoom window if its active */
   if (Global.FviewZoomW) zoomDraw();

   ClearUndoBuffer();
   return;
}
