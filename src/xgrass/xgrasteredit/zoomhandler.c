
/*
 * FILE: zoomhandler.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 *
 * zoomHandleMouseEvents()
 * -----------------------
 * This event handler handles all mouse events that occur within
 * the main image display area (Global.imageArea) and the zoom image
 * display area (Global.zoomArea).  It handles the events below:
 *
 * IN THE MAIN IMAGE AREA
 * 
 * Button 1 pressed
 *    Reset the zoom center and call zoomDraw() 
 *
 * IN THE ZOOM IMAGE AREA
 * 
 * 1) Button 1 pressed
 *    Mode XGRE_POLY_EDIT: accept point as polygon vertex
 *    Mode XGRE_BOX_EDIT: set point as base of rubberband box
 *    Mode not XGRE_LOCKED: apply brush to point by calling 
 *    ApplyBrush()
 *
 * 2) Button 3 pressed
 *    If polygon-edit underway, then call PolygonApplyBrush()
 *
 * 3) Motion with Button 1 pressed
 *    Mode XGRE_BOX_EDIT: adjust rubberband-box
 *    Mode XGRE_HORIZ_DRAG: apply horizontal drag of brush
 *    Mode XGRE_VERTI_DRAG: apply vertical drag of brush
 *
 * 4) Motion with no button pressed
 *    Always: update status message area
 *    Mode XGRE_POLY_EDIT: adjust rubberband-line
 *
 * zoomHandleExposeEvents()
 * ------------------------
 * This event handler handles expose events that occur within
 * the zoom image display area (Global.zoomArea).  It redraws the
 * exposed area by putting the corresponding area of the global 
 * image structure (Global.zoomImage) into the zoom image display
 * area.
 *
 */

#include "xgre.h"

/*****************************/
/*** zoomHandleMouseEvents ***/
/*****************************/

void
#ifdef _NO_PROTO
zoomHandleMouseEvents(w, cld, event, dispatch)
   Widget    w;
   int       cld;
   XEvent   *event;
   Boolean  *dispatch;
#else
zoomHandleMouseEvents(Widget w, int cld, XEvent *event, Boolean *dispatch)
#endif
{
int x, y;
int irow,icol;
char xstr[20], ystr[20];
float zh = Global.zoomHeight;
float zw = Global.zoomWidth;
static int lastR, lastC;         /* last raster cell coordinates */
static int baseR, baseC;         /* base of region (rubber-band) box */
static int lastX, lastY;         /* zoom window image coordiates */
static int baseX, baseY;         /* base of region (rubber-band) box */
static int xarr[200], yarr[200]; /* Edit-Polygon-Region points */
static XPoint pnts[200];
static int ppoints=0;            /* number of points */
static int boxdrawn=False;
static int linedrawn=False;
static int draginc=0;

/*** HANDLE MOUSE EVENTS IN MAIN IMAGE AREA ***/

if (w == Global.imageArea)
   {
   switch (event->type)
      {
      case ButtonPress:
         x = event->xbutton.x;
         y = event->xbutton.y;
         if (x>Global.imageWidth || y>Global.imageHeight) break;
         if ( event->xbutton.button == Button1 )
            {
            /*** RESET CENTER OF ZOOM ***/
#           ifdef DEBUG
            printf("Reset zoom center\n");
#           endif

            /* zoomed image too narrow to fill zoom window */
            if (Global.imageWidth*Global.zoomMag < Global.zoomWidth)
               Global.zoomX = Global.imageWidth/2.0;

            /* new zoom center too far left */
            else if (x < (int)(zw/(2.0*Global.zoomMag)))
               Global.zoomX = (int)(zw/(2.0*Global.zoomMag));

            /* new zoom center too far right */
            else if (x > (Global.imageWidth - (int)(zw/(2.0*Global.zoomMag))))
               Global.zoomX = Global.imageWidth-(int)(zw/(2.0*Global.zoomMag));
            else Global.zoomX = x;

            /* zoomed image too short to fill zoom window */
            if (Global.imageHeight*Global.zoomMag < Global.zoomHeight)
               Global.zoomY = Global.imageHeight/2.0;

            /* new zoom center too far up */
            else if (y < (int)(zh/(2.0*Global.zoomMag)))
               Global.zoomY = (int)(zh/(2.0*Global.zoomMag));

            /* new zoom center too far down */
            else if (y > (Global.imageHeight - (int)(zh/(2.0*Global.zoomMag))))
               Global.zoomY = Global.imageHeight-(int)(zh/(2.0*Global.zoomMag));
            else Global.zoomY = y;
  
            zoomDraw();
            }
         else if ( event->xbutton.button == Button2 )
            {
            /*** QUERY RASTER ***/
            UpdatePositionText(x,y); 
            UpdateCategoryText(x,y); 
            }
         break;
   
      case MotionNotify:
         x = event->xmotion.x;
         y = event->xmotion.y;
         UpdatePositionText(x,y);
         UpdateCategoryText(x,y); 
         break;
      } 
   }

/*** HANDLE MOUSE EVENTS IN ZOOM AREA ***/

else if (w==Global.zoomArea && Global.zoomLoaded)
   {
   switch (event->type)
      {
      /**************************/
      /*** BUTTON PRESS EVENT ***/
      /**************************/

      case ButtonPress:
         x = event->xmotion.x; 
         y = event->xmotion.y;
         irow = Global.zoomRowOff + y/Global.zoomMag;
         icol = Global.zoomColOff + x/Global.zoomMag;
         if (icol>Global.imageWidth || irow>Global.imageHeight) break;

         /****************/
         /*** BUTTON 1 ***/
         /****************/
         if ( event->xbutton.button == Button1 )
            {
            if (Global.mode == XGRE_POLY_EDIT)
               {
               /* set polygon point */
               xarr[ppoints] = icol;
               yarr[ppoints] = irow;
               pnts[ppoints].x = x;
               pnts[ppoints].y = y;
               ppoints++;
               }
            else if (Global.mode == XGRE_BOX_EDIT)
               {
               /*** BEGIN REGION EDIT RUBBERBAND BOX ***/
               baseC = icol;
               baseR = irow;
               baseX = x; 
               baseY = y;
               }
            else if (Global.mode != XGRE_LOCKED)
               {
               /*** APPLY BRUSH ***/
               ApplyBrush(icol,irow);
               lastC = icol;
               lastR = irow;	
               lastX = x;
               lastY = y; 
               }
            }

         /****************/
         /*** BUTTON 3 ***/
         /****************/
         else if ( event->xbutton.button == Button3 )
            {
            if (ppoints > 1)
               {
               /* set last point */
               xarr[ppoints] = icol;
               yarr[ppoints] = irow;
               pnts[ppoints].x = x;
               pnts[ppoints].y = y;
               ppoints++;
   
               /* erase polygon */
               XSetFunction(Global.display,Global.imageGC,GXxor);
               XSetForeground(Global.display,Global.imageGC,
                  XgdGetVectColorPixelByName("red"));
               XDrawLines(Global.display,XtWindow(Global.zoomArea),
                  Global.imageGC,pnts,ppoints,CoordModeOrigin);
               XSetFunction(Global.display,Global.imageGC,GXcopy);
   
               /* process polygon edit */
               PolygonApplyBrush(xarr,yarr,ppoints);
               }
            /* reset polygon variables */
            linedrawn = False;
            SetEditMode(XGRE_NORMAL);
            ppoints = 0;
            }
         break;

      /********************/
      /*** MOTION EVENT ***/
      /********************/

      case MotionNotify:
         x = event->xmotion.x;
         y = event->xmotion.y;
         irow = Global.zoomRowOff+y/Global.zoomMag;
         icol = Global.zoomColOff+x/Global.zoomMag;
         if (icol>Global.imageWidth || irow>Global.imageHeight) break;
         UpdatePositionText(icol,irow);
         UpdateCategoryText(icol,irow);

         /**************************/
         /*** BUTTON 1 HELD DOWN ***/
         /**************************/
         if (event->xmotion.state & Button1Mask)
            {
            if (Global.mode == XGRE_BOX_EDIT)
               {
               /*** ADJUST REGION EDIT RUBBERBAND BOX ***/
               int X,Y,W,H;
               XSetFunction(Global.display,Global.imageGC,GXxor);
               XSetForeground(Global.display,Global.imageGC,
                  XgdGetVectColorPixelByName("red"));
               if (boxdrawn)
                  {
                  /* erase previous rubberband box */
                  X = (baseX < lastX) ? baseX : lastX;
                  Y = (baseY < lastY) ? baseY : lastY;
                  W = (baseX < lastX) ? (lastX-baseX) : (baseX-lastX);
                  H = (baseY < lastY) ? (lastY-baseY) : (baseY-lastY);
                  XDrawRectangle(Global.display,XtWindow(Global.zoomArea),
                     Global.imageGC,X,Y,W,H);
                  }
               /* draw new rubberband box */
               X = (baseX < x) ? baseX : x;
               Y = (baseY < y) ? baseY : y;
               W = (baseX < x) ? (x-baseX) : (baseX-x);
               H = (baseY < y) ? (y-baseY) : (baseY-y);
               XDrawRectangle(Global.display,XtWindow(Global.zoomArea),
                  Global.imageGC,X,Y,W,H);
               XSetFunction(Global.display,Global.imageGC,GXcopy);
               boxdrawn = True;
               lastC = icol;
               lastR = irow;
               lastX = x;
               lastY = y;
               }
            else if (Global.mode == XGRE_HORIZ_DRAG)
               {   
               /*** HORIZONTAL DRAG APPLY BRUSH ***/
               int ic;
               if (draginc==0 && icol>lastC) draginc = 1;
               else if (draginc==0 && icol<lastC) draginc = -1;
               else if ((draginc==1 && icol>lastC)||(draginc==-1 && icol<lastC))
                  {
                  for (ic = lastC; ic != icol; ic += draginc)
                     ApplyBrush(ic,lastR);
                  lastC = icol;
                  }
               }
            else if (Global.mode == XGRE_VERTI_DRAG)
               {
               /*** VERTICAL DRAG APPLY BRUSH ***/
               int ir;
               if (draginc==0 && irow>lastR) draginc = 1;
               else if (draginc==0 && irow<lastR) draginc = -1;
               else if ((draginc==1 && irow>lastR)||(draginc==-1 && irow<lastR))
                  {
                  for (ir = lastR; ir != irow; ir += draginc)
                     ApplyBrush(lastC,ir);
                  lastR = irow;
                  }
               }
            else if (Global.mode != XGRE_LOCKED)
               {
               if (!(icol==lastC && irow==lastR))
                  {
                  /*** DRAG APPLY BRUSH ***/
                  ApplyBrush(icol,irow);
                  lastC = icol;
                  lastR = irow;
                  lastX = x;
                  lastY = y;
                  }
               }
            }
         /***************************************/
         /*** MOTION WITH NO BUTTON HELD DOWN ***/
         /***************************************/
         else
            {
            if (Global.mode == XGRE_POLY_EDIT && ppoints > 0)
               {
               /*** ADJUST POLYGON-EDIT RUBBERBAND LINE */
               /* set line color=red and function=XOR */
               XSetFunction(Global.display,Global.imageGC,GXxor);
               XSetForeground(Global.display,Global.imageGC,
                  XgdGetVectColorPixelByName("red"));
               if (linedrawn)
                  {
                  /* erase previous line */
                  XDrawLine(Global.display,
                     XtWindow(Global.zoomArea),Global.imageGC,
                     pnts[ppoints-1].x,pnts[ppoints-1].y,lastX,lastY);
                  }
               /* draw new line */
               XDrawLine(Global.display,
                  XtWindow(Global.zoomArea),Global.imageGC,
                  pnts[ppoints-1].x,pnts[ppoints-1].y,x,y);
               linedrawn = True;
               lastX = x;
               lastY = y;
               /* set line function back to COPY */
               XSetFunction(Global.display,Global.imageGC,GXcopy);
               }
            else if (linedrawn)
              {
              /* polygon-mode was aborted, erase ongoing polygon */
              XSetFunction(Global.display,Global.imageGC,GXxor);
              XSetForeground(Global.display,Global.imageGC,
                 XgdGetVectColorPixelByName("red"));

              /* erase last line */
              XDrawLine(Global.display,XtWindow(Global.imageArea),
               Global.imageGC,pnts[ppoints-1].x,pnts[ppoints-1].y,lastX,lastY);

              /* erase polygon-points */
              XDrawLines(Global.display,XtWindow(Global.zoomArea),
                 Global.imageGC,pnts,ppoints,CoordModeOrigin);
              XSetFunction(Global.display,Global.imageGC,GXcopy);
              linedrawn = False;
              }
            }
         break;

      /****************************/
      /*** BUTTON RELEASE EVENT ***/
      /****************************/

      case ButtonRelease:
         x = event->xbutton.x;
         y = event->xbutton.y;
         irow = Global.zoomRowOff + y/Global.zoomMag;
         icol = Global.zoomColOff + x/Global.zoomMag;
         if ( event->xbutton.button == Button1 )
            {
            if (Global.mode == XGRE_BOX_EDIT)
               {
               /*** END REGION EDIT RUBBERBAND BOX ***/
               int x1,y1,x2,y2;
               if (boxdrawn)
                  {
                  /* erase previous rubberband box */
                  int X,Y,W,H;
                  XSetFunction(Global.display,Global.imageGC,GXxor);
                  XSetForeground(Global.display,Global.imageGC,
                     XgdGetVectColorPixelByName("red"));
                  X = (baseX < lastX) ? baseX : lastX;
                  Y = (baseY < lastY) ? baseY : lastY;
                  W = (baseX < lastX) ? (lastX-baseX) : (baseX-lastX);
                  H = (baseY < lastY) ? (lastY-baseY) : (baseY-lastY);
                  XDrawRectangle(Global.display,XtWindow(Global.zoomArea),
                     Global.imageGC,X,Y,W,H);
                  XSetFunction(Global.display,Global.imageGC,GXcopy);
                  boxdrawn = False;
                  }
               if (baseC < icol) { x1=baseC; x2=icol;  }
               else              { x1=icol;  x2=baseC; }
               if (baseR < irow) { y1=baseR; y2=irow;  }
               else              { y1=irow;  y2=baseR; }
#              ifdef DEBUG
               printf("Apply brush to (%d,%d) to (%d,%d)\n",x1,y1,x2,y2);
#              endif
               EditCellAddressRange(x1,y1,x2,y2);
               }
            else if (Global.mode==XGRE_VERTI_DRAG ||
                     Global.mode==XGRE_HORIZ_DRAG)
               draginc=0;
            }
         break;
      }
   }
}

/******************************/
/*** zoomHandleExposeEvents ***/
/******************************/

void
#ifdef _NO_PROTO
zoomHandleExposeEvents(w, cld, event, dispatch)
Widget    w;
XtPointer cld;
XEvent   *event;
Boolean  *dispatch;
#else
zoomHandleExposeEvents(Widget w,XtPointer cld,XEvent *event,Boolean *dispatch)
#endif
{
int x, y, width, height, loop, n, ix, iy;

x      = event->xexpose.x;
y      = event->xexpose.y;
width  = event->xexpose.width;
height = event->xexpose.height;

#ifdef DEBUG
printf("Zoom expose event (%d,%d) %dx%d\n",x,y,width,height);
#endif

if (Global.zoomLoaded)
   XPutImage(Global.display,XtWindow(Global.zoomArea),
      Global.imageGC,Global.zoomImage,x,y,x,y,width,height);
}
