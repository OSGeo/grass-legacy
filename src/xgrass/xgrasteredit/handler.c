
/* 
 * FILE: handler.c
 *
 * PROGRAMMER: David M. Johnson
 * 
 * FUNCTIONS:
 *
 * HandleMouseEvents()
 * -------------------
 * This event handler handles all mouse events that occur within
 * the main image display area (Global.imageArea).  It handles the 
 * four events below: 
 * 
 * 1) Button 1 pressed
 *    Mode XGRE_POLY_EDIT: accept point as polygon vertex 
 *    Mode XGRE_BOX_EDIT: set point as base of rubberband box 
 *    Mode not XGRE_LOCKED: apply brush to point by calling 
 *    ApplyBrush().
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
 *
 * HandleExposeEvents()
 * --------------------
 * This event handler handles expose events that occur within
 * the main image display area (Global.imageArea).  It redraws the 
 * exposed area by putting (XPutImage) the corresponding area 
 * of the global image structure (Global.image) into the main 
 * image display area. 
 *
 */

#include "xgre.h"

/*************************/
/*** HandleMouseEvents ***/
/*************************/

void
#ifdef _NO_PROTO
HandleMouseEvents(w, cld, event, dispatch)
     Widget    w;
     int     cld;
     XEvent   *event;
     Boolean  *dispatch;
#else
HandleMouseEvents(Widget w, int cld, XEvent *event, Boolean *dispatch)
#endif
{
int x, y;
static int lastX=0, lastY=0;
static int xarr[200], yarr[200];  /* Edit-Polygon-Region points */
static XPoint pnts[200];
static int ppoints=0;             /* number of points */
static int baseX=0, baseY=0;      /* base of Edit-Box-Region rectangle */
static boxdrawn = False;
static linedrawn = False;
static draginc = 0;

if (Global.mode==XGRE_UNLOADED) return;

switch (event->type)
   {
   /**************************/
   /*** BUTTON PRESS EVENT ***/
   /**************************/

   case ButtonPress:
      x = event->xbutton.x;
      y = event->xbutton.y;

      /****************/
      /*** BUTTON 1 ***/
      /****************/
      if ( event->xbutton.button == Button1 )
         {
         if (Global.mode == XGRE_POLY_EDIT)
            {
            /* set polygon point */
            xarr[ppoints] = x; 
            yarr[ppoints] = y;
            pnts[ppoints].x = x;
            pnts[ppoints].y = y;
            ppoints++;
            }
         else if (Global.mode == XGRE_BOX_EDIT)
            {
            /* set base of box-edit rubberband box */
            baseX = x;
            baseY = y;
            lastX = 0;
            lastY = 0;
            }
         else if (Global.mode != XGRE_LOCKED) 
            {
            ApplyBrush(x,y);
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
            xarr[ppoints] = x;
            yarr[ppoints] = y;
            pnts[ppoints].x = x;
            pnts[ppoints].y = y;
            ppoints++;
   
            /* erase polygon */
            XSetFunction(Global.display,Global.imageGC,GXxor);
            XSetForeground(Global.display,Global.imageGC,
               XgdGetVectColorPixelByName("red"));
            XDrawLines(Global.display,XtWindow(Global.imageArea),
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
      UpdatePositionText(x,y);
      UpdateCategoryText(x,y);

      /**************************/
      /*** BUTTON 1 HELD DOWN ***/
      /**************************/
      if (event->xmotion.state & Button1Mask)
         {
         if (Global.mode == XGRE_BOX_EDIT)
            {
            /*** ADJUST BOX-REGION EDIT RUBBERBAND BOX ***/
            int X,Y,W,H;
            /* set line color=red and function=XOR */ 
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
               XDrawRectangle(Global.display,XtWindow(Global.imageArea),
                  Global.imageGC,X,Y,W,H);
               }
            /* draw new rubberband box */
            X = (baseX < x) ? baseX : x;
            Y = (baseY < y) ? baseY : y;
            W = (baseX < x) ? (x-baseX) : (baseX-x);
            H = (baseY < y) ? (y-baseY) : (baseY-y);
            XDrawRectangle(Global.display,XtWindow(Global.imageArea),
               Global.imageGC,X,Y,W,H);
            boxdrawn = True;
            lastX = x;
            lastY = y;
            /* set line function back to COPY */ 
            XSetFunction(Global.display,Global.imageGC,GXcopy);
            }
         else if (Global.mode == XGRE_HORIZ_DRAG)
            {
            /*** HORIZONTAL DRAG APPLY BRUSH ***/
            int ix;
            if (draginc==0 && x>lastX) draginc = 1;
            else if (draginc==0 && x<lastX) draginc = -1;
            else if ((draginc==1 && x>lastX) || (draginc==-1 && x<lastX))
               {
               for (ix = lastX; ix != x; ix += draginc)
                  ApplyBrush(ix,lastY);
               lastX = x;
               }
            }
         else if (Global.mode == XGRE_VERTI_DRAG)
            {
            /*** VERTICAL DRAG APPLY BRUSH ***/
            int iy;
            if (draginc==0 && y>lastY) draginc = 1;
            else if (draginc==0 && y<lastY) draginc = -1;
            else if ((draginc==1 && y>lastY) || (draginc==-1 && y<lastY))
               {
               for (iy = lastY; iy != y; iy += draginc)
                  ApplyBrush(lastX,iy);
               lastY = y;
               }
            }
         else if (Global.mode != XGRE_LOCKED) 
            {
            if (!(x==lastX && y==lastY)) /* don't apply twice to one cell */
               {
               /*** DRAG APPLY BRUSH ***/
               ApplyBrush(x,y);
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
               XDrawLine(Global.display,XtWindow(Global.imageArea),
                  Global.imageGC,xarr[ppoints-1],yarr[ppoints-1],lastX,lastY);
               }
            /* draw new line */
            XDrawLine(Global.display,XtWindow(Global.imageArea),
               Global.imageGC,xarr[ppoints-1],yarr[ppoints-1],x,y);
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
               Global.imageGC,xarr[ppoints-1],yarr[ppoints-1],lastX,lastY);

            /* erase polygon-points */
            XDrawLines(Global.display,XtWindow(Global.imageArea),
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
      if ( event->xbutton.button == Button1 )
         {
         if (Global.mode == XGRE_BOX_EDIT)
            {
            /*** END BOX-REGION EDIT RUBBERBAND BOX ***/
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
               XDrawRectangle(Global.display,XtWindow(Global.imageArea),
                  Global.imageGC,X,Y,W,H);
               XSetFunction(Global.display,Global.imageGC,GXcopy);
               boxdrawn = False;
               }
            if (baseX < x) { x1=baseX; x2=x;     }
            else           { x1=x;     x2=baseX; }
            if (baseY < y) { y1=baseY; y2=y;     }
            else           { y1=y;     y2=baseY; }
#           ifdef DEBUG
            printf("Apply brush to (%d,%d) to (%d,%d)\n",x1,y1,x2,y2);
#           endif
            EditCellAddressRange(x1,y1,x2,y2);
            SetEditMode(XGRE_NORMAL);
            }
         else if (Global.mode==XGRE_VERTI_DRAG || 
                  Global.mode==XGRE_HORIZ_DRAG)
            draginc=0;
         }
      break;
   }
}

/**************************/
/*** HandleExposeEvents ***/
/**************************/

void
#ifdef _NO_PROTO
HandleExposeEvents(w, cld, event, dispatch)
Widget    w;
XtPointer cld;
XEvent   *event;
Boolean  *dispatch;
#else
HandleExposeEvents(Widget w,XtPointer cld,XEvent *event,Boolean *dispatch)
#endif
{
int x, y, width, height, loop, n, ix, iy;
static Boolean  imageInitial = True;

x      = event->xexpose.x;
y      = event->xexpose.y;
width  = event->xexpose.width;
height = event->xexpose.height;

#ifdef DEBUG
printf("Image expose event (%d,%d) %dx%d\n",x,y,width,height);
#endif

if (imageInitial) 
   {
   imageInitial = False;
   Global.imageHeight = height;
   Global.imageWidth = width;
   }
else if (Global.mode != XGRE_UNLOADED)
   XPutImage(Global.display,XtWindow(Global.imageArea),
      Global.imageGC,Global.image,x,y,x,y,width,height);
}

