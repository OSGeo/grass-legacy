#include "xgdisp.h"
#define ABS(x) ((x) < 0 ? -(x):(x))
#define ISPOLYPOINT() (Global.drawType == XGD_POLYLINE || \
		       Global.drawType == XGD_POLYGON)
#define ISSPLINE() (Global.drawType == XGD_OPEN_INTERP_SPLINE || \
		    Global.drawType == XGD_CLOSED_INTERP_SPLINE || \
		    Global.drawType == XGD_OPEN_APPROX_SPLINE || \
		    Global.drawType == XGD_CLOSED_APPROX_SPLINE )
#define WITHINSLOP(x1, x2, s) (((x1)>((x2) - (s))) && ((x1)<((x2) + (s))))

#define ISBARSCALE() (Global.drawType == XGD_BARSCALE)

int     saveType;
Boolean doExposures = True;

void
#ifdef _NO_PROTO
HandleExposeEvents(w, cld, event, dispatch)
     Widget    w;
     XtPointer     cld;
     XEvent   *event;
     Boolean  *dispatch;
#else
HandleExposeEvents(Widget w, XtPointer cld, XEvent *event, Boolean *dispatch)
#endif
{
  int     x, y, width, height, n;
  static Boolean  initial = True;

  if (event->xexpose.send_event){
    doExposures = !doExposures;
    if (doExposures && Global.selectedObjects){
      if (Global.selectedObjects->object->type == XGD_GEOFRAME){
	ObjectList addList = NULL, excludeList = NULL, newList = NULL, tmp;
	GetBoxAffectedObjects(Global.selectedObjects->object->bbox,
			      &addList, &excludeList);
	XCopyArea(Global.display,
		  Global.selectedObjects->object->Obj.GeoFrame.pixmap,
		  XtWindow(Global.drawArea),
		  Global.selectedObjects->object->objgc,
		  0, 0,
		  Global.selectedObjects->object->width,
		  Global.selectedObjects->object->height,
		  Global.selectedObjects->object->x,
		  Global.selectedObjects->object->y);
	ReorderObjects(&addList);
	tmp = addList;
	while (tmp && tmp->object != Global.selectedObjects->object){
	  AddObjectToList(&newList, tmp->object);
	  tmp = tmp->next;
	}
	DrawObjectsInList(newList, NULL);
      }
      return;
    }
  }
  
  if (!doExposures)
    return;
  
  if (Global.hRuler == (Pixmap) NULL || Global.vRuler == (Pixmap) NULL) {
    CreateRulers();
    ClearRulerPixmaps();
    DrawRulerPixmaps();
  }
  x = event->xexpose.x;
  y = event->xexpose.y;
  width = event->xexpose.width;
  height = event->xexpose.height;

  if (event->xexpose.window == Global.hRulerWindow) {
    XCopyArea(Global.display, Global.hRuler, Global.hRulerWindow,
	      Global.rulerGC, x + Global.lastX, y, width, height, x, y);
  } else if (event->xexpose.window == Global.vRulerWindow) {
    XCopyArea(Global.display, Global.vRuler, Global.vRulerWindow,
	      Global.rulerGC, x, y + Global.lastY, width, height, x, y);
  } else {
    ObjectList addlist = NULL;
    ObjectList exclude = NULL;
    XgdBox *bb;
    /*
     * This is the first expose event, handle some preliminary
     * stuff
     */

    if (initial) {
      CreateXorGc();
      InitGridAttributes(Global.display,Global.screenNo,Global.xorGC);
      InitBarscaleAttributes(Global.display,Global.screenNo,Global.xorGC);
      InitStdSiteAttributes(Global.display,Global.screenNo,Global.xorGC);
      SetScrollBars(width - 1, height - 1);
      initial = False;
    }

    bb = XgdSetBox(y, y + height, x, x + width);
    GetBoxAffectedObjects(bb, &addlist, &exclude);

    if ( addlist && Global.mode != XGD_MODE_POLYRESHAPE) {
      XRectangle rects[4];

/*
 set up clipping rectangles to handle expose events.  Any resize handles
 outside the expose area need to be redrawn in xor mode to erase them, then
 all resize handles are redrawn.
 */
      n = 0;

      if (y != 0){
	rects[n].x = 0; rects[n].y = 0;
	rects[n].width = (short) Global.hWidth; rects[n].height=(short)(y - 1);
	++n;
      }
      if (x != 0){
	rects[n].x = 0; rects[n].y = (short) (y);
	rects[n].width = (short) (x - 1); rects[n].height = (short) (height);
	++n;
      }
      if (Global.hWidth - x - width != 0){
	rects[n].x = (short) (x + width + 1); rects[n].y = (short) (y);
	rects[n].width = (short) (Global.hWidth - x - width);
	rects[n].height = (short) (height);
	++n;
      }
      if (Global.vHeight - y - height != 0){
	rects[n].x = 0; rects[n].y = (short) (y + height + 1);
	rects[n].width = (short) Global.hWidth;
	rects[n].height = (short) (Global.vHeight - y - height);
	++n;
      }
      XSetClipRectangles(XtDisplay(w), Global.xorGC, 0, 0,
			 rects, n, Unsorted);

      UnDrawObjectsInList(addlist, True);
      ReorderObjects(&addlist);
      XSetClipMask(XtDisplay(w), Global.xorGC, None);
      DrawObjectsInList(addlist, NULL);
    } else if (Global.mode == XGD_MODE_POLYRESHAPE) {
      DrawPolyPointHandles(Global.selectedObjects->object,
			   XgdGetGCOfObject(Global.selectedObjects->object));
      XgdDrawObject(XgdGetGCOfObject(Global.selectedObjects->object),
		    Global.selectedObjects->object, False, NULL);
      DrawPolyPointHandles(Global.selectedObjects->object,
			   XgdGetGCOfObject(Global.selectedObjects->object));
    }
  } 
}

void
#ifdef _NO_PROTO
HandleMouseEvents(w, cld, event, dispatch)
     Widget    w;
     int     cld;
     XEvent   *event;
     Boolean  *dispatch;
#else
HandleMouseEvents(Widget w, int cld, XEvent * event, Boolean * dispatch)
#endif
{
  int     x, y;
  int     retX, retY;
  static int     selectX, selectY;
  static int     lastX, lastY;
  static int which;
  static XgdBox *bbox;
  static XgdBox *savedBBox;
  static XgdPointList *point = NULL;
  
  switch (event->type) {
/*
 ***************************************************************************
 *  ButtonPress
 ***************************************************************************
 */
  case ButtonPress:
    x = event->xbutton.x;
    y = event->xbutton.y;
/*
 ***************************************************************************
 *           Button1
 ***************************************************************************
 */
    if ( event->xbutton.button == Button1 ) {
      if (Global.mode == XGD_MODE_QUERY_RASTER) {
	queryraster(x, y, False);
	return;
      }	else if (Global.mode == XGD_MODE_HIGHLIGHT) {
	Pixel p = queryraster(x, y, True);
	  
	  if (p){
	    EndFlash();
	    StartFlash();
	    HighlightFlash(p, NULL);
	  }
/*
 ***************************************************************************
 *                XGD_MODE_DRAW
 ***************************************************************************
 */
      }	else if (Global.mode == XGD_MODE_DRAW) {
        if ( ISPOLYPOINT() ) {
          if ( Global.currentObject == NULL ) {
            if ( Global.selectedObjects )  {
              ObjectList list = Global.selectedObjects;
	      
              for ( ; list; list = list->next) {
	        XgdDrawResizeHandles(list->object, Global.xorGC);
              }
            }
            Global.currentObject = XgdCreateObject(Global.drawType);
	    XgdInitObject(Global.display, XtWindow(Global.drawArea),
			  Global.currentObject, Global.fillPattern,
			  Global.linePattern, Global.foreground,
			  Global.background, Global.lineWidth);
	    
            HandleDrawModeButtonPress(&x, &y);
            XgdAddPointToPointList(Global.currentObject, x, y);
	    
            XgdDrawObject(Global.xorGC,Global.currentObject, False, NULL);
          } else {
            HandleDrawModeButtonRelease(&x, &y);
            XgdDrawObject(Global.xorGC,Global.currentObject, False, NULL);
            HandleDrawModeButtonPress(&x, &y);
            XgdAddPointToPointList(Global.currentObject, x, y);
	    
            XgdDrawObject(Global.xorGC,Global.currentObject, False, NULL);
          }
        } else if ( ISSPLINE() ) {
          if ( Global.currentObject == NULL ) {
            if ( Global.selectedObjects )  {
              ObjectList list = Global.selectedObjects;
	      
              for ( ; list; list = list->next) {
	        XgdDrawResizeHandles(list->object, Global.xorGC);
              }
            }
            Global.currentObject = XgdCreateObject(Global.drawType);
            XgdInitObject(Global.display, XtWindow(Global.drawArea),
			  Global.currentObject, Global.fillPattern,
			  Global.linePattern, Global.foreground,
			  Global.background, Global.lineWidth);
            HandleDrawModeButtonPress(&x, &y);
            XgdAddPointToPointList(Global.currentObject, x, y);
	    saveType = Global.currentObject->type;
	    Global.currentObject->type = XGD_POLYLINE;
            XgdDrawObject(Global.xorGC,Global.currentObject, False, NULL);
	    Global.currentObject->type = saveType;
          } else {
            HandleDrawModeButtonRelease(&x, &y);
	    saveType = Global.currentObject->type;
	    Global.currentObject->type = XGD_POLYLINE;
            XgdDrawObject(Global.xorGC,Global.currentObject, -100, NULL);
	    Global.currentObject->type = saveType;
            HandleDrawModeButtonPress(&x, &y);
            XgdAddPointToPointList(Global.currentObject, x, y);
	    
	    saveType = Global.currentObject->type;
	    Global.currentObject->type = XGD_POLYLINE;
            XgdDrawObject(Global.xorGC,Global.currentObject, -100, NULL);
	    Global.currentObject->type = saveType;
          }
        } else {
          if ( Global.selectedObjects ) {
            ObjectList list = Global.selectedObjects;
	    
            for ( ; list; list = list->next) {
              XgdDrawResizeHandles(list->object, Global.xorGC);
            }
          }
          HandleDrawModeButtonPress(&x, &y);
          Global.currentObject = XgdCreateObject(Global.drawType);
	  if (Global.drawType == XGD_LEGEND){
	    XgdInitObject(Global.display, XtWindow(Global.drawArea),
			  Global.currentObject, Global.fillPattern,
			  Global.linePattern, Global.foreground,
			  Global.background, Global.lineWidth);
	    XgdInitLegend(Global.currentObject,Global.selectedObjects->object);
	  } else if (Global.drawType == XGD_LABEL)
	    XgdInitObject(Global.display, XtWindow(Global.drawArea),
			  Global.currentObject, Global.fillPattern,
			  Global.linePattern, Global.vectColors[1],
			  Global.background, Global.lineWidth);
	  else
	    XgdInitObject(Global.display, XtWindow(Global.drawArea),
			  Global.currentObject, Global.fillPattern,
			  Global.linePattern, Global.foreground,
			  Global.background, Global.lineWidth);
	  
          XgdSetObjectPosition(Global.currentObject, x, y);
        }
/*
 ***************************************************************************
 *         XGD_MODE_POLYRESHAPE
 ***************************************************************************
 */
      } else if ( Global.mode == XGD_MODE_POLYRESHAPE){
	if (SelectedObjectCount() == 1 &&
	    ((point = InPolyPointMoveHandle(Global.selectedObjects->object,
					    x, y)) != NULL)){
	  Global.redrawList = NULL;
	  
	  GetAffectedObjects(Global.selectedObjects->object,
			     &Global.redrawList, &Global.excludeList);
	  
	  ReorderObjects(&Global.redrawList);
	  XgdUnDrawObject(Global.selectedObjects->object,
			  Global.selectedObjects->object->bg, True);
	  
	  if (ISSPLINE()){
	    saveType = Global.selectedObjects->object->type;
	    Global.selectedObjects->object->type = XGD_FAKE_POLYLINE;
	    XgdDrawObject(XgdGetGCOfObject(Global.selectedObjects->object),
			  Global.selectedObjects->object, True, NULL);
	  }
	}
/*
 ***************************************************************************
 *             XGD_MODE_SELECT
 ***************************************************************************
 */
      } else if ( Global.mode == XGD_MODE_SELECT ) {
        ObjectList list = NULL;
        /*
         * check resize handles on selected objects.
         * if we get a hit, set to resize mode.
         * else check to see if we are inside a selected object.
         * if we are, set to move mode.
         * else just start banding select process.
         */
        selectX = x;
        selectY = y;
	/* undraw resize handles */
	for ( list = Global.selectedObjects; list; list = list->next) {
	  XgdDrawResizeHandles(list->object, Global.xorGC);
	}
	
        if ( SelectedObjectCount() == 1 &&
	    (which = XgdInResizeHandle(Global.selectedObjects->object, x, y)) != XGD_OUTSIDE_HANDLE  &&
	    Global.selectedObjects->object->type != XGD_BARSCALE) {
	  
	  bbox = XgdGetBBoxOfObject(Global.selectedObjects->object);
	  
	  /* Figure out which objects are affected by this UnDraw */
	  Global.excludeList = NULL;
	  AddObjectToList(&Global.excludeList, Global.selectedObjects->object);
	  
	  Global.redrawList = NULL;
	  GetAffectedObjects(Global.selectedObjects->object,
			     &Global.redrawList, &Global.excludeList);
	  
	  XgdUnDrawObject(Global.selectedObjects->object,
			  WhitePixel(Global.display, Global.screenNo), True);
	  SetMode(XGD_MODE_RESIZE);
	  HandleResizeButtonPress(x, y, bbox, which,
		  (Global.selectedObjects->object->type == XGD_SQUARE ||
		   Global.selectedObjects->object->type == XGD_CIRCLE));
	} else {
	  GetPointSelectedObjects(Global.selectedObjects, x, y, &list);
	  
	  Global.excludeList = NULL;
	  if ( list ) {
	    
	    bbox = GetBBoxOfSelectedObjects();
	    savedBBox = GetBBoxOfSelectedObjects();
	    Global.redrawList = NULL;
	    for ( list = Global.selectedObjects; list; list = list->next) {
	      AddObjectToList(&Global.excludeList, list->object);
	    }
	    for ( list = Global.selectedObjects; list; list = list->next) {
	      /* Figure out which objects are affected by this UnDraw */
	      GetAffectedObjects(list->object, &Global.redrawList,
				 &Global.excludeList);
	      XgdUnDrawObject(list->object,
			      WhitePixel(Global.display, Global.screenNo),
			      True);
	    }

	    ReorderObjects(&Global.redrawList);
	    SetMode(XGD_MODE_MOVE);
	    HandleMoveButtonPress(bbox);
	    lastX = x;
	    lastY = y;
	  } else {
	    BandRect(&x, &y, XGD_BAND_INIT, False);
	  }
	}
      }
/*
 ***************************************************************************
 *                Button2
 ***************************************************************************
 */
    }else if ( event->xbutton.button == Button2 ) {
      if (Global.mode == XGD_MODE_DRAW && ISPOLYPOINT() &&
	  Global.currentObject ) {
	HandleDrawModeButtonRelease(&x, &y);
	XgdDrawObject(Global.xorGC,Global.currentObject, False, NULL);
	XgdGetObjectPosition(Global.currentObject,
			     &retX, &retY);
	XgdConfigureObject( XgdGetGCOfObject(Global.currentObject),
			   Global.currentObject, retX, retY, ABS(retX - x),
			   ABS(retY - y), True);
	XgdDrawObject(XgdGetGCOfObject(Global.currentObject),
		      Global.currentObject, True, NULL);
	XgdDrawResizeHandles(Global.currentObject, Global.xorGC);
	AddObjectToList(&Global.objectList, Global.currentObject);
	
	Global.selectedObjects = NULL;
	AddObjectToList(&Global.selectedObjects, Global.currentObject);

	Global.currentObject = NULL;
	SetMode(XGD_MODE_SELECT);
      } else if (Global.mode == XGD_MODE_DRAW && ISSPLINE() &&
		 Global.currentObject ) {
	HandleDrawModeButtonRelease(&x, &y);
	saveType = Global.currentObject->type;
	Global.currentObject->type = XGD_POLYLINE;
	XgdDrawObject(Global.xorGC,Global.currentObject, -100, NULL);
	Global.currentObject->type = saveType;
	XgdGetObjectPosition(Global.currentObject,
			     &retX, &retY);
	XgdConfigureObject( XgdGetGCOfObject(Global.currentObject),
			   Global.currentObject, retX, retY, ABS(retX - x),
			   ABS(retY - y), True);
	XgdDrawObject(XgdGetGCOfObject(Global.currentObject),
		      Global.currentObject, True, NULL);
	XgdDrawResizeHandles(Global.currentObject, Global.xorGC);
	AddObjectToList(&Global.objectList, Global.currentObject);
	Global.selectedObjects = NULL;
	AddObjectToList(&Global.selectedObjects, Global.currentObject);
	
	Global.currentObject = NULL;
	SetMode(XGD_MODE_SELECT);
      }
/*
 ***************************************************************************
 *                Button3
 ***************************************************************************
 */
    } else if  ( event->xbutton.button == Button3 ) {
	XmMenuPosition(Global.popup, event);
        XtManageChild(Global.popup);
    }
    break;
/*
 ***************************************************************************
 *             ButtonRelease:
 ***************************************************************************
 */
  case ButtonRelease:
    x = event->xbutton.x;
    y = event->xbutton.y;
    /* if button 1 is released */
    if ( event->xbutton.button == Button1 ) {
      /* and we're off the drawing area */
      if (x > Global.hWidth || x < 0 || y > Global.vHeight || y < 0) {
	/* and we're in draw mode, then destroy the object */
	if (Global.mode == XGD_MODE_DRAW) {
	  XgdDestroyObject(Global.currentObject);
	  /*          FreeObjectList(Global.selectedObjects);*/
	  Global.selectedObjects = NULL;
	  Global.currentObject = NULL;
	}
	/* and we got a valid x,y pair */
      } else {
	/* and we're in draw mode */
/*
 ***************************************************************************
 *       XGD_MODE_DRAW
 ***************************************************************************
 */
	if (Global.mode == XGD_MODE_DRAW) {
	  if ( ISBARSCALE() ) {
	    if ( Global.currentObject ) {
	      if ( SelectedObjectCount() == 1 &&
		  Global.selectedObjects->object->type == XGD_GEOFRAME) {
		Global.currentObject = XgdCreateObject(Global.drawType);
		XgdInitObject(Global.display, XtWindow(Global.drawArea),
			      Global.currentObject, Global.fillPattern,
			      Global.linePattern, Global.foreground,
			      Global.background, Global.lineWidth);
		todrawbarscale(Global.currentObject, x,y);
		XgdConfigureObject(XgdGetGCOfObject(Global.currentObject),
				   Global.currentObject,
				   Global.currentObject->x,
				   Global.currentObject->y,
				   Global.currentObject->width,
				   Global.currentObject->height, False);
		XgdDrawObject(XgdGetGCOfObject(Global.currentObject),
			      Global.currentObject, True, NULL);
		XgdDrawResizeHandles(Global.currentObject, Global.xorGC);
		AddObjectToList(&Global.objectList, Global.currentObject);
		Global.selectedObjects = NULL;
		AddObjectToList(&Global.selectedObjects, Global.currentObject);
		Global.currentObject = NULL;
		SetMode(XGD_MODE_SELECT);
	      }
	      else
		{
		  Global.currentObject = NULL;
		  SetMode(XGD_MODE_SELECT);
		}
	    }
	  } else
	    /* and we're not drawing a polyline or polygon */
	    if ( !ISPOLYPOINT() && !ISSPLINE() && Global.currentObject) {
	      HandleDrawModeButtonRelease(&x, &y);
	      XgdGetObjectPosition(Global.currentObject,
				   &retX, &retY);
	      /* we may have to swap start location */
	      if ( retX > x ) {
		int tmp;
		
		tmp = retX; retX = x; x = tmp;
	      }
	      if ( retY > y ) {
		int tmp;
		
		tmp = retY; retY = y; y = tmp;
	      }
	      if (Global.drawType == XGD_LABEL){
		XgdConfigureObject(XgdGetGCOfObject(Global.currentObject),
				   Global.currentObject,
				   retX, retY,
				   10, 10);
		XgdDrawObject(XgdGetGCOfObject(Global.currentObject),
			      Global.currentObject, True, NULL);
	      } else if (Global.drawType == XGD_LEGEND){
		G_read_cats(Global.selectedObjects->object->Obj.GeoFrame.rname,
			  Global.selectedObjects->object->Obj.GeoFrame.rmapset,
			  &Global.currentObject->Obj.Legend.cats);
		XgdDrawLegend(Global.currentObject, NULL);
	      } else {
		XgdConfigureObject(XgdGetGCOfObject(Global.currentObject),
				   Global.currentObject,
				   retX, retY,
				   ABS(retX - x), ABS(retY - y), True);
		XgdDrawObject(XgdGetGCOfObject(Global.currentObject),
			      Global.currentObject, True, NULL);
	      }
	      XgdDrawResizeHandles(Global.currentObject, Global.xorGC);
	      AddObjectToList(&Global.objectList, Global.currentObject);
	      Global.selectedObjects = NULL;
	      AddObjectToList(&Global.selectedObjects, Global.currentObject);
	      if (Global.currentObject->type == XGD_LABEL && IsLabelAttUp())
		ChangeLabelAttObject(Global.selectedObjects->object);
	      else if (Global.currentObject->type == XGD_LEGEND)
		SetLegendObject(Global.selectedObjects->object);
	      Global.currentObject = NULL;
	      SetMode(XGD_MODE_SELECT);
	    }
/*
 ***************************************************************************
 *            XGD_MODE_POLYRESHAPE
 ***************************************************************************
 */
	} else if ( Global.mode == XGD_MODE_POLYRESHAPE ) {
	  if (point){
	    XgdObject *obj = Global.selectedObjects->object;
	    
	    MovePolyPoint(obj, point, x, y);
	    DrawPolyPointHandles(obj, Global.xorGC);
	    XgdUnDrawObject(obj, obj->bg, True);
	    XgdConfigureObject(XgdGetGCOfObject(obj), obj, obj->x, obj->y,
			       obj->width, obj->height, True);
	    XgdDrawObject(XgdGetGCOfObject(obj),
			  obj, True, NULL);
	    DrawPolyPointHandles(obj, Global.xorGC);
	    point = NULL;
	  }
/*
 ***************************************************************************
 *               XGD_MODE_SELECT
 ***************************************************************************
 */
	} else if ( Global.mode == XGD_MODE_SELECT ) {
	  ObjectList list = NULL;
	  ObjectList newList = NULL;
	  XgdBox *box;
	  
	  /*
	   * undraw the banding box
	   * find the list of objects contained in the box
	   * if we got a hit, draw their resize handles.
	   */
	  BandRect(&x, &y, XGD_BAND_END, False);
	  
	  if ( WITHINSLOP(y, selectY, 2) && WITHINSLOP(x, selectX, 2) ) {
	    GetPointSelectedObjects(Global.objectList, x, y, &newList);
	  } else {
	    if ( y < selectY ) {
	      int tmp = selectY;
	      selectY = y;
	      y = tmp;
	    }
	    if ( x < selectX ) {
	      int tmp = selectX;
	      selectX = x;
	      x = tmp;
	    }
	    box = XgdSetBox(selectY, y, selectX, x);

	    GetBoxSelectedObjects(box, &newList);
	  }
	  /* draw new resize handles */
	  Global.selectedObjects = newList;
	  for ( list = Global.selectedObjects; list; list = list->next) {
	    XgdDrawResizeHandles(list->object, Global.xorGC);
	  }

	  if ( SelectedObjectCount() == 1 &&
	      Global.selectedObjects->object->type == XGD_BARSCALE ){
	    if (Global.barbox != NULL && XtIsManaged(Global.barbox) )
	      UpdateBarBox();
	  } else if ( SelectedObjectCount() == 1 &&
		     Global.selectedObjects->object->type == XGD_GEOFRAME ){
	    if (Global.gridbox != NULL && XtIsManaged(Global.gridbox) )
	      UpdateGridBox();
	  } else if ( SelectedObjectCount() == 1 &&
		     Global.selectedObjects->object->type == XGD_LABEL){
	    if (IsLabelAttUp())
	      ChangeLabelAttObject(Global.selectedObjects->object);
	  } else if ( SelectedObjectCount() == 1 &&
		     Global.selectedObjects->object->type == XGD_LEGEND){
	    SetLegendObject(Global.selectedObjects->object);
	  }
	  
/*
 ***************************************************************************
 *           XGD_MODE_RESIZE
 ***************************************************************************
 */
	} else if ( Global.mode == XGD_MODE_RESIZE ) {
	  ObjectList list = NULL;
	  XgdBox    *bb;
	  ObjectList add = NULL, exclude = NULL;
	  HandleResizeButtonRelease(x, y, bbox, which,
		    (Global.selectedObjects->object->type == XGD_SQUARE ||
		     Global.selectedObjects->object->type == XGD_CIRCLE));

	  if (Global.selectedObjects->object->type == XGD_GEOFRAME){
	    int loop;

	    for (loop = 0; loop < Global.selectedObjects->object->Obj.GeoFrame.numbarscales; loop++)
	      XgdUnDrawObject(Global.selectedObjects->object->Obj.GeoFrame.barscales[loop], (Global.selectedObjects->object->Obj.GeoFrame.barscales[loop])->bg, True);
	  }
	      
	  XgdConfigureObject(XgdGetGCOfObject(Global.selectedObjects->object),
			     Global.selectedObjects->object, bbox->l, bbox->t,
			     bbox->r - bbox->l, bbox->b - bbox->t, True);
	  
	  bb = GetBBoxOfSelectedObjects();
	  for(list = Global.redrawList; list; list = list->next)
	    AddObjectToList(&exclude, list->object);
	  
	  GetBoxAffectedObjects(bb, &add, &exclude);

	  for(list = Global.redrawList; list; list = list->next)
	    AddObjectToList(&add, list->object);

	  UnDrawObjectsInList(add, False);
	  if (Global.selectedObjects->object->type == XGD_GEOFRAME){
	    int loop;

	    for (loop = 0; loop < Global.selectedObjects->object->Obj.GeoFrame.numbarscales; loop++)
	      AddObjectToList(&add, Global.selectedObjects->object->Obj.GeoFrame.barscales[loop]);
	  }
	  
	  ReorderObjects(&add);
	  DrawObjectsInList(add, NULL);
	  SetMode(XGD_MODE_SELECT);
/*
 ***************************************************************************
 *       XGD_MODE_MOVE
 ***************************************************************************
 */
	} else if ( Global.mode == XGD_MODE_MOVE ) {
	  ObjectList newList = NULL;
	  ObjectList tmp;
	  
	  HandleMoveButtonRelease(lastX - selectX, lastY - selectY, bbox);
	  /*
	   * if we haven't really moved, we've selected an already
	   * selected object
	   */
	  if ( WITHINSLOP(savedBBox->l, bbox->l - selectX + lastX, 2) &&
	      WITHINSLOP(savedBBox->t, bbox->t - selectY + lastY, 2) ) {
	    GetPointSelectedObjects(Global.selectedObjects,
				    savedBBox->l, savedBBox->t, &newList);
	    for (tmp = Global.selectedObjects; tmp; tmp = tmp->next){
	      AddObjectToList(&Global.redrawList, tmp->object);
	    }
	    Global.selectedObjects = newList;
	    UnDrawObjectsInList(Global.redrawList, False);
	    ReorderObjects(&Global.redrawList);
	    DrawObjectsInList(Global.redrawList, NULL);
	  } else {
	    ObjectList exclude = NULL, add = NULL, tmp;
	    XgdBox *box;
	    
	    for ( newList = Global.selectedObjects; newList; newList = newList->next) {
	      XgdBox *bb = XgdGetBBoxOfObject(newList->object);

	      XgdConfigureObject(XgdGetGCOfObject(newList->object),
				 newList->object, bb->l - selectX + lastX,
				 bb->t - selectY + lastY,
				 bb->r - bb->l, bb->b - bb->t, True);
	      AddObjectToList(&Global.redrawList, newList->object);
	    }
	    for (tmp = Global.redrawList; tmp; tmp = tmp->next)
	      AddObjectToList(&exclude, tmp->object);
	    box = GetBBoxOfSelectedObjects();
	    GetBoxAffectedObjects(box, &add, &exclude);
	    for(; add ; add = add->next)
	      AddObjectToList(&Global.redrawList, add->object);
	    UnDrawObjectsInList(Global.redrawList, False);
	    ReorderObjects(&Global.redrawList);
	    DrawObjectsInList(Global.redrawList, NULL);
	    Global.redrawList = NULL;
	  }
/*	  for (newList=Global.selectedObjects;newList;newList=newList->next){
	    XgdDrawResizeHandles(newList->object, Global.xorGC);
	  }*/
	  SetMode(XGD_MODE_SELECT);
	}
      }
    }
    break;
/*
 ***************************************************************************
 *        MotionNotify
 ***************************************************************************
 */
  case MotionNotify:
    x = event->xmotion.x;
    y = event->xmotion.y;
    /* no modifiers, pure mouse motion */
    if (event->xmotion.state & Button1Mask) {
      if (Global.mode == XGD_MODE_DRAW) {
	if ( !ISPOLYPOINT() && !ISSPLINE() ) {
	  HandleDrawModeMotion(&x, &y);
	}
      } else if ( Global.mode == XGD_MODE_SELECT ) {
	/* update banding box */
	BandRect(&x, &y, XGD_BAND, False);
      } else if ( Global.mode == XGD_MODE_POLYRESHAPE) {
	if (point){
/*	  DrawPolyPointHandles(Global.selectedObjects->object, Global.xorGC);*/
	  MovePolyPoint(Global.selectedObjects->object, point, x, y, NULL);
/*	  DrawPolyPointHandles(Global.selectedObjects->object, Global.xorGC);*/
	}
      } else if ( Global.mode == XGD_MODE_RESIZE ) {
	HandleResizeMotion(x, y, bbox, which,
			(Global.selectedObjects->object->type == XGD_SQUARE ||
			 Global.selectedObjects->object->type == XGD_CIRCLE));
      } else if ( Global.mode == XGD_MODE_MOVE ) {
	HandleMoveMotion(x - selectX, y - selectY,
			 lastX - selectX, lastY - selectY, bbox);
	lastX = x;
	lastY = y;
      }
      UpdatePositionReadout(w, event->xmotion.x, event->xmotion.y);
    } else if (event->xmotion.state) {
      UpdatePositionReadout(w, event->xmotion.x, event->xmotion.y);
    } else {
      if (Global.mode == XGD_MODE_DRAW) {
	if ( (ISPOLYPOINT() || ISSPLINE()) && Global.currentObject ) {
	  HandleDrawModeMotion(&x, &y);
	}
      }
      UpdatePositionReadout(w, event->xmotion.x, event->xmotion.y);
    }
    break;
  }
}

void
#ifdef _NO_PROTO
UpdatePositionReadout(w, x, y)
     Widget    w;
     int     x, y;
#else
UpdatePositionReadout(Widget w, int x, int y)
#endif
{
  char    buffer[1024];
  int     ival;
  double    dval;

  switch (Global.units) {
  case XGD_UNITS_PIXELS:
    sprintf(buffer, "%4d", x);
    XmTextFieldSetString(Global.xPosArea, buffer);
    sprintf(buffer, "%4d", y);
    XmTextFieldSetString(Global.yPosArea, buffer);
    break;
  case XGD_UNITS_INCHES:
    ival = XmConvertUnits(w, XmHORIZONTAL, XmPIXELS,
			  x, Xm1000TH_INCHES);
    dval = ((double) ival) / 1000.0;
    sprintf(buffer, "%8.2f", dval);
    XmTextFieldSetString(Global.xPosArea, buffer);
    ival = XmConvertUnits(w, XmHORIZONTAL, XmPIXELS,
			  y, Xm1000TH_INCHES);
    dval = ((double) ival) / 1000.0;
    sprintf(buffer, "%8.2f", dval);
    XmTextFieldSetString(Global.yPosArea, buffer);
    break;
  case XGD_UNITS_MILLI:
    ival = XmConvertUnits(w, XmHORIZONTAL, XmPIXELS,
			  x, Xm100TH_MILLIMETERS);
    dval = ((double) ival) / 100.0;
    sprintf(buffer, "%8.2f", dval);
    XmTextFieldSetString(Global.xPosArea, buffer);
    ival = XmConvertUnits(w, XmHORIZONTAL, XmPIXELS,
			  y, Xm100TH_MILLIMETERS);
    dval = ((double) ival) / 100.0;
    sprintf(buffer, "%8.2f", dval);
    XmTextFieldSetString(Global.yPosArea, buffer);
    break;
  }
}

void
#ifdef _NO_PROTO
HandleResizeEvents(w, cld, event, dispatch)
     Widget    w;
     XtPointer     cld;
     XEvent   *event;
     Boolean  *dispatch;
#else
HandleResizeEvents(Widget w, XtPointer cld, XEvent * event, Boolean * dispatch)
#endif
{

  switch (event->type) {
  case ConfigureNotify:
    SetScrollBars(event->xconfigure.width - 16,
		  event->xconfigure.height - 16);
    break;
  }
}

void
#ifdef _NO_PROTO
HandleDrawModeMotion(x, y)
     int *x;
     int *y;
#else
HandleDrawModeMotion(int x, int y)
#endif
{
  switch (Global.drawType) {
    /* These kind we just band a rectangle */
  case XGD_SQUARE:
  case XGD_RECTANGLE:
  case XGD_CIRCLE:
  case XGD_ELLIPSE:
  case XGD_GEOFRAME:
  case XGD_LEGEND:
  case XGD_HISTOGRAM:
    if ( Global.drawType == XGD_SQUARE ||
	Global.drawType == XGD_CIRCLE ) {
      BandRect(x, y, XGD_BAND, True);
    } else {
      BandRect(x, y, XGD_BAND, False);
    }
    break;
    /* These kind we just band a line */
  case XGD_POLYLINE:
  case XGD_POLYGON:
  case XGD_OPEN_INTERP_SPLINE:
  case XGD_CLOSED_INTERP_SPLINE:
  case XGD_OPEN_APPROX_SPLINE:
  case XGD_CLOSED_APPROX_SPLINE:
    BandLine(*x, *y, XGD_BAND);
    break;
    /* These kind we just drag the bbox around */
  case XGD_LABEL:
  case XGD_GRID:
  case XGD_BARSCALE:
    break;
  }
}

void
#ifdef _NO_PROTO
HandleDrawModeButtonRelease(x, y)
     int *x;
     int *y;
#else
HandleDrawModeButtonRelease(int x, int y)
#endif
{
  switch (Global.drawType) {
    /* These kind we just band a rectangle */
  case XGD_SQUARE:
  case XGD_RECTANGLE:
  case XGD_CIRCLE:
  case XGD_ELLIPSE:
  case XGD_GEOFRAME:
  case XGD_LEGEND:
  case XGD_HISTOGRAM:
    if ( Global.drawType == XGD_SQUARE ||
	Global.drawType == XGD_CIRCLE ) {
      BandRect(x, y, XGD_BAND_END, True);
    } else {
      BandRect(x, y, XGD_BAND_END, False);
    }
    break;
    /* These kind we just band a line */
  case XGD_POLYLINE:
  case XGD_POLYGON:
  case XGD_OPEN_INTERP_SPLINE:
  case XGD_CLOSED_INTERP_SPLINE:
  case XGD_OPEN_APPROX_SPLINE:
  case XGD_CLOSED_APPROX_SPLINE:
    BandLine(*x, *y, XGD_BAND_END);
    break;
    /* These kind we just drag the bbox around */
  case XGD_LABEL:
  case XGD_GRID:
  case XGD_BARSCALE:
    break;
  }
}

void
#ifdef _NO_PROTO
HandleDrawModeButtonPress(x, y)
     int *x;
     int *y;
#else
HandleDrawModeButtonPress(int *x, int *y)
#endif
{
  switch (Global.drawType) {
    /* These kind we just band a rectangle */
  case XGD_SQUARE:
  case XGD_RECTANGLE:
  case XGD_CIRCLE:
  case XGD_ELLIPSE:
  case XGD_GEOFRAME:
  case XGD_LEGEND:
  case XGD_HISTOGRAM:
    BandRect(x, y, XGD_BAND_INIT, False);
    break;
    /* These kind we just band a line */
  case XGD_POLYLINE:
  case XGD_POLYGON:
  case XGD_OPEN_INTERP_SPLINE:
  case XGD_CLOSED_INTERP_SPLINE:
  case XGD_OPEN_APPROX_SPLINE:
  case XGD_CLOSED_APPROX_SPLINE:
    BandLine(*x, *y, XGD_BAND_INIT);
    break;
    /* These kind we just drag the bbox around */
  case XGD_LABEL:
  case XGD_GRID:
  case XGD_BARSCALE:
    /* XorBox(*x, *y); */
    break;
  }
}

void
#ifdef _NO_PROTO
HandleResizeButtonPress(x, y, bbox, which, equal)
     int x;
     int y;
     XgdBox *bbox;
     int which;
     Boolean equal;
#else
HandleResizeButtonPress( int x, int y, XgdBox *bbox, int which, Boolean equal)
#endif
{
  XDrawRectangle(Global.display, XtWindow(Global.drawArea), Global.xorGC,
		 bbox->l, bbox->t, bbox->r - bbox->l, bbox->b - bbox->t);
}

void
#ifdef _NO_PROTO
HandleResizeButtonRelease(x, y, bbox, which, equal)
     int x;
     int y;
     XgdBox *bbox;
     int which;
     Boolean equal;
#else
HandleResizeButtonRelease(int x, int y, XgdBox *bbox, int which, Boolean equal)
#endif
{
  XDrawRectangle(Global.display, XtWindow(Global.drawArea), Global.xorGC,
		 bbox->l, bbox->t, bbox->r - bbox->l, bbox->b - bbox->t);

  UpdateBBox(x, y, bbox, which, equal);
}

void
#ifdef _NO_PROTO
HandleResizeMotion(x, y, bbox, which, equal)
     int x;
     int y;
     XgdBox *bbox;
     int which;
     Boolean equal;
#else
HandleResizeMotion( int x, int y, XgdBox *bbox, int which, Boolean equal)
#endif
{
  XDrawRectangle(Global.display, XtWindow(Global.drawArea), Global.xorGC,
		 bbox->l, bbox->t, bbox->r - bbox->l, bbox->b - bbox->t);

  UpdateBBox(x, y, bbox, which, equal);

  XDrawRectangle(Global.display, XtWindow(Global.drawArea), Global.xorGC,
		 bbox->l, bbox->t, bbox->r - bbox->l, bbox->b - bbox->t);
}

void
#ifdef _NO_PROTO
HandleMoveButtonPress(bbox)
     XgdBox *bbox;
#else
HandleMoveButtonPress(XgdBox *bbox)
#endif
{
  XDrawRectangle(Global.display, XtWindow(Global.drawArea), Global.xorGC,
		 bbox->l, bbox->t,
		 bbox->r - bbox->l, bbox->b - bbox->t);
}

void
#ifdef _NO_PROTO
HandleMoveMotion(xoff, yoff, lxoff, lyoff, bbox)
     int xoff;
     int yoff;
     int lxoff;
     int lyoff;
     XgdBox *bbox;
#else
HandleMoveMotion(int xoff, int yoff, int lxoff, int lyoff, XgdBox *bbox)
#endif
{
  XDrawRectangle(Global.display, XtWindow(Global.drawArea), Global.xorGC,
		 bbox->l + lxoff, bbox->t + lyoff,
		 bbox->r - bbox->l, bbox->b - bbox->t);
  XDrawRectangle(Global.display, XtWindow(Global.drawArea), Global.xorGC,
		 bbox->l + xoff, bbox->t + yoff,
		 bbox->r - bbox->l, bbox->b - bbox->t);
}

void
#ifdef _NO_PROTO
HandleMoveButtonRelease(lxoff, lyoff, bbox)
     int lxoff;
     int lyoff;
     XgdBox *bbox;
#else
HandleMoveButtonRelease(int lxoff, int lyoff, XgdBox *bbox)
#endif
{
  XDrawRectangle(Global.display, XtWindow(Global.drawArea), Global.xorGC,
		 bbox->l + lxoff, bbox->t + lyoff,
		 bbox->r - bbox->l, bbox->b - bbox->t);
}

void
#ifdef _NO_PROTO
UpdateBBox(x, y, bbox, which, equal)
     int x;
     int y;
     XgdBox *bbox;
     int which;
     Boolean equal;
#else
UpdateBBox( int x, int y, XgdBox *bbox, int which, Boolean equal)
#endif
{
  switch(which) {
  case XGD_UPPER_LEFT:
    if ( x < bbox->r - 3  && y < bbox->b - 3 ) {
      bbox->l = x;
      bbox->t = y;
    }
    if ( equal && ((bbox->r - bbox->l) != (bbox->b - bbox->t)) ) {
      int gap = ABS((bbox->r - bbox->l) - (bbox->b - bbox->t));

      if ( (bbox->r - bbox->l) > (bbox->b - bbox->t) ) {
	bbox->t -= gap;
      } else {
	bbox->l -= gap;
      }
    }
    break;
  case XGD_TOP:
    if ( !equal && y < bbox->b - 3 ) {
      bbox->t = y;
    }
    break;
  case XGD_UPPER_RIGHT:
    if ( x > bbox->l + 3  && y < bbox->b - 3 ) {
      bbox->r = x;
      bbox->t = y;
    }
    if ( equal && ((bbox->r - bbox->l) != (bbox->b - bbox->t)) ) {
      int gap = ABS((bbox->r - bbox->l) - (bbox->b - bbox->t));

      if ( (bbox->r - bbox->l) > (bbox->b - bbox->t) ) {
	bbox->t -= gap;
      } else {
	bbox->r += gap;
      }
    }
    break;
  case XGD_RIGHT:
    if ( !equal && x > bbox->l + 3 ) {
      bbox->r = x;
    }
    break;
  case XGD_BOTTOM_RIGHT:
    if ( x > bbox->l + 3 && y > bbox->t + 3) {
      bbox->r = x;
      bbox->b = y;
    }
    if ( equal && ((bbox->r - bbox->l) != (bbox->b - bbox->t)) ) {
      int gap = ABS((bbox->r - bbox->l) - (bbox->b - bbox->t));

      if ( (bbox->r - bbox->l) > (bbox->b - bbox->t) ) {
	bbox->b += gap;
      } else {
	bbox->r += gap;
      }
    }
    break;
  case XGD_BOTTOM:
    if ( !equal && y > bbox->t + 3 ) {
      bbox->b = y;
    }
    break;
  case XGD_BOTTOM_LEFT:
    if ( x < bbox->r - 3  && y > bbox->t + 3) {
      bbox->l = x;
      bbox->b = y;
    }
    if ( equal && ((bbox->r - bbox->l) != (bbox->b - bbox->t)) ) {
      int gap = ABS((bbox->r - bbox->l) - (bbox->b - bbox->t));

      if ( (bbox->r - bbox->l) > (bbox->b - bbox->t) ) {
	bbox->b += gap;
      } else {
	bbox->l -= gap;
      }
    }
    break;
  case XGD_LEFT:
    if ( !equal && x < bbox->r - 3 ) {
      bbox->l = x;
    }
    break;
  }
}

void
#ifdef _NO_PROTO
MovePolyPoint(obj, p, x, y)
     XgdObject    *obj;
     XgdPointList *p;
     int          x;
     int          y;
#else
MovePolyPoint(XgdObject *obj, XgdPointList *p, int x, int y)
#endif
{
  DrawPolyPointHandles(obj, Global.xorGC);
  XgdUnDrawObject(obj, obj->bg, True);
  p->x = x - obj->x;
  p->y = y - obj->y;
  p->box->t = p->y - 3;
  p->box->b = p->y + 3;
  p->box->l = p->x - 3;
  p->box->r = p->x + 3;
  XgdDrawObject(XgdGetGCOfObject(obj), obj, False, NULL);
  DrawPolyPointHandles(obj, Global.xorGC);
}

XgdPointList *
#ifdef _NO_PROTO
InPolyPointMoveHandle(obj, x, y)
     XgdObject *obj;
     int        x;
     int        y;
#else
InPolyPointResizeHandle(XgdObject *obj, int x, int y)
#endif
{
  XgdPointList *tmp;
  XgdBox       box;

  if (obj->type == XGD_POLYGON || obj->type == XGD_POLYLINE)
    tmp =  obj->Obj.Polyline.pts;
  else
    tmp = obj->Obj.Spline.points;

  while (tmp != NULL){
    box = *(tmp->box);
    box.t += obj->bbox->t;
    box.b += obj->bbox->t;
    box.l += obj->bbox->l;
    box.r += obj->bbox->l;
    if (XgdPointInBox(box, x, y))
      return(tmp);
    tmp = tmp->next;
  }
  return(NULL);
}

void
#ifdef _NO_PROTO
DrawPolyPointHandles(obj, gc)
     XgdObject *obj;
     GC        gc;
#else
DrawPolyPointHandles(XgdObject *obj, GC gc)
#endif
{
  XgdPointList *tmp;

  if (obj->type == XGD_POLYGON || obj->type == XGD_POLYLINE)
    tmp =  obj->Obj.Polyline.pts;
  else
    tmp = obj->Obj.Spline.points;

  while (tmp) {
    XDrawRectangle(obj->display, obj->window, gc,
		   tmp->box->l + obj->x, tmp->box->t + obj->y, 6, 6);
    tmp = tmp->next;
  }
}
