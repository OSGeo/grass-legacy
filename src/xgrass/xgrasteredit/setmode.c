
/*
 * FILE: setmode.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTION:
 *
 * SetEditMode(mode)
 * -----------------
 * This function changes the raster editor's mode.
 * 
 */

#include "xgre.h"
#include "bmaps.h"

/*******************/
/*** SetEditMode ***/
/*******************/

SetEditMode(mode)
int mode;
{
XmString bxms;
Pixmap pixmap;
Pixel bg,fg;

/* turn off the old mode's indicators */

switch (Global.mode)
   {
   case XGRE_UNLOADED:
      break;
   case XGRE_NORMAL:
      break;
   case XGRE_LOCKED:
      if (mode == XGRE_NORMAL)
         {
         bxms = XmStringCreateSimple("Lock");
         XtVaSetValues(Global.mbtnLock,XmNlabelString,bxms,NULL);
         XmStringFree(bxms);
         XtVaGetValues(Global.btnLock,
            XmNforeground,&fg,
            XmNbackground,&bg,NULL);
         pixmap = XCreatePixmapFromBitmapData(Global.display,
            RootWindowOfScreen(Global.screenPtr),unlock_bits,
            unlock_width,unlock_height,fg,bg,
            DefaultDepthOfScreen(Global.screenPtr));
         XtVaSetValues(Global.btnLock,
            XmNlabelType,XmPIXMAP,
            XmNlabelPixmap,pixmap,NULL);
         }
      break;
   case XGRE_VERTI_DRAG:
      bxms = XmStringCreateSimple("Constrain-Vertical");
      XtVaSetValues(Global.mbtnVerti,XmNlabelString,bxms,NULL);
      XmStringFree(bxms);
      break;
   case XGRE_HORIZ_DRAG:
      bxms = XmStringCreateSimple("Constrain-Horizontal");
      XtVaSetValues(Global.mbtnHoriz,XmNlabelString,bxms,NULL);
      XmStringFree(bxms);
      break;
   case XGRE_BOX_EDIT:
      bxms = XmStringCreateSimple("Region-Box");
      XtVaSetValues(Global.mbtnBox,XmNlabelString,bxms,NULL);
      XmStringFree(bxms);
      break;
   case XGRE_POLY_EDIT: 
      bxms = XmStringCreateSimple("Region-Polygon");
      XtVaSetValues(Global.mbtnPoly,XmNlabelString,bxms,NULL);
      XmStringFree(bxms);
      break;
   }

/* turn on the new mode's indicators */

switch (mode)
   {
   case XGRE_UNLOADED:
      UpdateModeText("MODE: Edit-Unloaded");
      break;
   case XGRE_NORMAL:
      UpdateModeText("MODE: Edit-Normal");
      if (Global.zoomRunning)
         {
         DoBrushCursor(Global.zoomArea);
         DoZoomCursor(Global.imageArea);
         }
      else DoBrushCursor(Global.imageArea);
      break;
   case XGRE_LOCKED:
      UpdateModeText("MODE: Edit-Locked");
      if (Global.zoomRunning)
         {
         UndoCursor(Global.zoomArea);
         DoZoomCursor(Global.imageArea);
         }
      else UndoCursor(Global.imageArea);
      XtVaGetValues(Global.btnLock,
         XmNforeground,&fg,
         XmNbackground,&bg,NULL);
      pixmap = XCreatePixmapFromBitmapData(Global.display,
         RootWindowOfScreen(Global.screenPtr),lock_bits,
         lock_width,lock_height,fg,bg,
         DefaultDepthOfScreen(Global.screenPtr));
      XtVaSetValues(Global.btnLock,
         XmNlabelType,XmPIXMAP,
         XmNlabelPixmap,pixmap,NULL);
      bxms = XmStringCreateSimple("Unlock");
      XtVaSetValues(Global.mbtnLock,XmNlabelString,bxms,NULL);
      XmStringFree(bxms);
      break;
   case XGRE_VERTI_DRAG:
      UpdateModeText("MODE: Edit-Constrain-Vertical");
      bxms = XmStringCreateSimple("* Constrain-Vertical");
      XtVaSetValues(Global.mbtnVerti,XmNlabelString,bxms,NULL);
      XmStringFree(bxms);
      break;
   case XGRE_HORIZ_DRAG:
      UpdateModeText("MODE: Edit-Constrain-Horizontal");
      bxms = XmStringCreateSimple("* Constrain-Horizontal");
      XtVaSetValues(Global.mbtnHoriz,XmNlabelString,bxms,NULL);
      XmStringFree(bxms);
      break;
   case XGRE_BOX_EDIT:
      UpdateModeText("MODE: Edit-Box-Region");
      bxms = XmStringCreateSimple("* Box-Region");
      XtVaSetValues(Global.mbtnBox,XmNlabelString,bxms,NULL);
      XmStringFree(bxms);
      break;
   case XGRE_POLY_EDIT:
      UpdateModeText("MODE: Edit-Polygon-Region");
      bxms = XmStringCreateSimple("* Polygon-Region");
      XtVaSetValues(Global.mbtnPoly,XmNlabelString,bxms,NULL);
      XmStringFree(bxms); 
      break;
   }

Global.mode = mode;
}


