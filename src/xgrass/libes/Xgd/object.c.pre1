#include "xgrass_dlib.h"
#include "Vect.h"

/*
 ***************************************************************************
 * XgdCreateObject - create an object, zero it, set it's type and return it
 ***************************************************************************
 */
XgdObject *
#ifdef _NO_PROTO
XgdCreateObject(type)
     int type;
#else
XgdCreateObject(int type)
#endif
{
  XgdObject *obj;

  obj = (XgdObject *) XtMalloc(sizeof(XgdObject));

  bzero((char *) obj, sizeof(XgdObject));
  
  obj->type = type;

  return(obj);
}

/*
 ***************************************************************************
 * XgdDestroyObject - destory the given object, freeing the pixmap if the
 *     type is a GeoFrame.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdDestroyObject(obj)
     XgdObject *obj;
#else
XgdDestroyObject(XgdObject *obj)
#endif
{
  if (obj->type == XGD_GEOFRAME)
    XFreePixmap(obj->display, obj->Obj.GeoFrame.pixmap);

  XtFree(obj);
}

/*
 ***************************************************************************
 * XgdGetGCOfObject - Return the GC associated with the given object.
 *   This function may seem trivial, but is done to hide the implementation
 *   of the object data type from the library user.  The user should never
 *   access the data structure directly.
 ***************************************************************************
 */
GC
#ifdef _NO_PROTO
XgdGetGCOfObject(obj)
     XgdObject *obj;
#else
XgdGetGCOfObject(XgdObject *obj)
#endif
{
  if (obj == NULL)
    XgdError("Object in XgdGetGCOfObject was NULL.");

  return(obj->objgc);
}

/*
 ***************************************************************************
 * XgdSetObjectPosition - Set the object's x and y postion in the window
 *    associated with that object.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdSetObjectPosition(obj, x, y)
     XgdObject *obj;
     int       x;
     int       y;
#else
XgdSetObjectPosition(XgdObject *obj, int x, int y)
#endif
{
  obj->x = x;
  obj->y = y;
}

/*
 ***************************************************************************
 * XgdGetObjectPosition - return the x and y postion of the object.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdGetObjectPosition(obj, x, y)
     XgdObject *obj;
     int       *x;
     int       *y;
#else
XgdGetObjectPosition(XgdObject *obj, int *x, int *y)
#endif
{
  *x = obj->x;
  *y = obj->y;
}

/*
 ***************************************************************************
 * XgdInitObject - Initialize an object:
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdInitObject(dpy, win, obj, fp, lp, fg, bg, lw)
     Display   *dpy;     /* the display */
     Window    win;      /* Window that obj belong to */
     XgdObject *obj;     /* the object to initialize */
     int       fp;       /* Fill Pattern */
     int       lp;       /* line pattern */
     Pixel     fg;       /* foreground color */
     Pixel     bg;       /* background color */
     int       lw;       /* line width */
#else
XgdInitObject(Display *dpy, Window win, XgdObject *obj, int fp, int lp, Pixel fg, Pixel bg, int lw)
#endif
{
  XGCValues gcval;
  unsigned long vmask = GCForeground|GCBackground|GCPlaneMask|GCFillRule|GCCapStyle;
  
  if (obj == NULL)
    XgdError("Object passed to XgdInitObject is NULL");

  obj->display = dpy;
  obj->window = win;
    
  if (lw == 1)
    lw = 0;

  /* Fill in the data structure */
  obj->x = 0;
  obj->y = 0;
  obj->width = 0;
  obj->height = 0;
  obj->lw = lw;
  obj->lp = lp;
  obj->fg = fg;
  obj->bg = bg;
  obj->bbox = NULL;

  /* Fill in the GC structure with fg & bg information */
  gcval.foreground = fg;
  gcval.background = bg;
  gcval.plane_mask = AllPlanes;
  gcval.fill_rule = WindingRule;
  gcval.cap_style = CapRound;

  obj->objgc = XCreateGC(dpy, win, vmask, &gcval);
  if (obj->type == XGD_POLYLINE || obj->type == XGD_POLYGON)
    obj->Obj.Polyline.pts = NULL;
    
  XgdSetObjectFillPattern(obj, fg, bg, fp);
  XgdSetObjectLinePattern(obj, lw, lp);

  if (obj->type == XGD_GEOFRAME){
    G_get_set_window(&obj->Obj.GeoFrame.region);
    obj->Obj.GeoFrame.pixmap = NULL;
  }
}

void
#ifdef _NO_PROTO
XgdSetLegendFontName(obj, name)
     XgdObject *obj;
     char *name;
#else
XgdSetLegendFontName(XgdObject *obj, char *name)
#endif
{
    if ( obj->Obj.Legend.fontname ) XtFree(obj->Obj.Legend.fontname);
    obj->Obj.Legend.fontname = XtNewString(name);
}

void
#ifdef _NO_PROTO
XgdSetLegendFont(obj, font)
     XgdObject *obj;
     XFontStruct *font;
#else
XgdSetLegendFont(XgdObject *obj, XFontStruct *font)
#endif
{
  int               newheight, oldheight;
  struct Categories cat;

  newheight = font->ascent + font->descent;
  oldheight = obj->Obj.Legend.font->ascent + obj->Obj.Legend.font->descent;

  G_read_cats(obj->Obj.Legend.geoFrame->Obj.GeoFrame.rname,
	      obj->Obj.Legend.geoFrame->Obj.GeoFrame.rmapset,
	      &cat);

  if (newheight < oldheight){
    obj->height += (newheight - oldheight) * cat.num;
  }

  obj->Obj.Legend.font = font;
  XSetFont(obj->display, obj->objgc, font->fid);
}

void
#ifdef _NO_PROTO
XgdSetLegendsDisplayList(obj, list, nums)
     XgdObject  *obj;
     char      **list;
     int        *nums;
#else
XgdSetLegendsDisplayList(XgdObject *obj, char **list, int *nums)
#endif     
{
  int i;
  int size;
  
  if (!list){
    obj->Obj.Legend.catsToList = NULL;
    return;
  }
  
  for (size = 0; list[size]; size++);

  obj->Obj.Legend.catsToList = (char **) XtMalloc (sizeof(char *) * size);
  obj->Obj.Legend.catNums = (int *) XtMalloc(sizeof(int) * size);
  
  for (i = 0; i < size; i++){
    obj->Obj.Legend.catsToList[i] = (char *) XtMalloc (strlen(list[i]) + 1);
    strcpy(obj->Obj.Legend.catsToList[i], list[i]);
    obj->Obj.Legend.catNums[i] = nums[i];
  }
  obj->Obj.Legend.catsToList[i] = NULL;
}

void
#ifdef _NO_PROTO
XgdInitLegend(legend, geoframe)
     XgdObject *legend;
     XgdObject *geoframe;
#else
XgdInitLegend(XgdObject *legend, XgdObject *geoframe)
#endif
{
  G_read_cats(geoframe->Obj.GeoFrame.rname, geoframe->Obj.GeoFrame.rmapset,
	      &legend->Obj.Legend.cats);
  legend->Obj.Legend.geoFrame = geoframe;
  geoframe->Obj.GeoFrame.legend = legend;
  legend->Obj.Legend.toDisplay = XGD_DISPLAY_CAT_NAMES;
  legend->Obj.Legend.numCols = 1;
}

/*
 ***************************************************************************
 * XgdSetObjectFillPattern - Set the fill pattern for the given object
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdSetObjectFillPattern(obj, fg, bg, fp)
     XgdObject *obj;
     Pixel fg;
     Pixel bg;
     int fp;
#else
XgdSetObjectFillPattern(XgdObject *obj, Pixel fg, Pixel bg, int fp)
#endif
{
  obj->fp = fp;
  if (fp != XGD_FILL_PATTERN_NONE && fp != XGD_FILL_PATTERN_SOLID) {
    XSetTile(obj->display, obj->objgc, XgdCreateFillPixmap(obj, fg, bg, fp));
    XSetFillStyle(obj->display, obj->objgc, FillTiled);
  }
  else 
    XSetFillStyle(obj->display, obj->objgc, FillSolid);
}

/*
 ***************************************************************************
 * XgdSetObjectBackground - Set the background pixel color for the given
 *          object.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdSetObjectBackground(obj, bg)
     XgdObject *obj;
     Pixel     bg;
#else
XgdSetObjectBackground(XgdObject *obj, Pixel bg)
#endif
{
  obj->bg = bg;
  XSetBackground(obj->display, obj->objgc, bg);
}

/*
 ***************************************************************************
 * XgdSetObjectForeground - Set the foreground pixel color for the given
 *    object.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdSetObjectForeground(obj, fg)
     XgdObject *obj;
     Pixel     fg;
#else
XgdSetObjectForeground(XgdObject *obj, Pixel fg)
#endif
{
  obj->fg = fg;
  XSetForeground(obj->display, obj->objgc, fg);
}

/*
 ***************************************************************************
 * XgdSetObjectLinePattern - Set the line pattern for the given object using
 *   the table of dash patterns referenced by the integer index lp.  lw is
 *   the line width.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdSetObjectLinePattern(obj, lw, lp)
     XgdObject *obj;
     int lw;
     int lp;
#else
XgdSetObjectLinePattern(XgdObject *obj, int lw, int lp)
#endif
{
  int lstyle;

  obj->lw = lw;
  obj->lp = lp;
  if (__XGDlinePatternButtonTable[lp].mode == XGD_DASHED_NONE) return;
  
  if (__XGDlinePatternButtonTable[lp].mode == XGD_DASHED_SOLID)
    lstyle = LineSolid;
  else
    lstyle = LineDoubleDash;
  
  XSetLineAttributes(obj->display, obj->objgc, lw, lstyle, CapButt, JoinMiter);
  if (lp != XGD_LINE_PATTERN_NONE && lp != XGD_LINE_PATTERN_SOLID)
    XSetDashes(obj->display, obj->objgc, 0,
	       __XGDlinePatternButtonTable[lp].dashes,
	       __XGDlinePatternButtonTable[lp].n);
}

void
#ifdef _NO_PROTO
XgdConfigureResizeHandles(obj)
     XgdObject *obj;
#else
XgdConfigureResizeHandles(XgdObject *obj)
#endif     
{
  /* Set up the resize handles ... resize boxes are 6x6 boxes. */
  obj->handles[XGD_UPPER_LEFT] = *(XgdSetBox(obj->y - 3, obj->y + 3,
					     obj->x - 3, obj->x + 3));

  obj->handles[XGD_TOP] = *(XgdSetBox(obj->y - 3, obj->y + 3,
				      obj->x + obj->width/2 - 3,
				      obj->x + obj->width/2 + 3));
  obj->handles[XGD_UPPER_RIGHT] = *(XgdSetBox(obj->y - 3, obj->y + 3,
					      obj->x + obj->width - 3,
					      obj->x + obj->width + 3));
  obj->handles[XGD_RIGHT] = *(XgdSetBox(obj->y + obj->height/2 - 3,
					obj->y + obj->height/2 + 3,
					obj->x + obj->width - 3,
					obj->x  + obj->width + 3));
  obj->handles[XGD_BOTTOM_RIGHT] = *(XgdSetBox(obj->y + obj->height - 3,
					       obj->y + obj->height + 3,
					       obj->x + obj->width - 3,
					       obj->x + obj->width + 3));
  obj->handles[XGD_BOTTOM] = *(XgdSetBox(obj->y + obj->height - 3,
					 obj->y + obj->height + 3,
					 obj->x + obj->width/2 - 3,
					 obj->x + obj->width/2 + 3));
  obj->handles[XGD_BOTTOM_LEFT] = *(XgdSetBox(obj->y + obj->height - 3,
					      obj->y + obj->height + 3,
					      obj->x - 3, obj->x + 3));
  obj->handles[XGD_LEFT] = *(XgdSetBox(obj->y + obj->height/2 - 3,
				       obj->y + obj->height/2 + 3,
				       obj->x - 3, obj->x + 3));
}  

/*
 ***************************************************************************
 * XgdConfigureObject - Set the objects (x,y) position in the window, it's
 *    width and height.  Also configure the objects resize handle.  If the
 *    object is a GeoFrame, create it's new pixmap if needed.  If the object
 *    has been resized and is a XGD_POLY* or a spline, the points must be
 *    reconfigured.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdConfigureObject(gc, obj, x, y, width, height, fill)
     GC        gc;
     XgdObject *obj;
     int       x, y;
     int       width, height;
     int       fill;
#else
XgdConfigureObject(GC gc, XgdObject *obj, int x, int y, int width, int height, int fill)
#endif
{
  short changed;
  double aratio;
  int    minx, maxx, miny, maxy;
  XgdPointList *tmp;
  int    oldw, oldh;
  double  dx = 0.0, dy = 0.0;

  if (obj == NULL)
    XgdError("Object in XgdConfigureObject is NULL.");

  if (obj->lw == 0)
    obj->lw = 1;

  if (x < 0)
    x = 0;
  if (y < 0)
    y = 0;
  oldw = obj->width;
  oldh = obj->height;
  
  obj->x = x;
  obj->y = y;
  obj->width = width;
  obj->height = height;
  if (obj->height < 10)
    obj->height = 10;
  if (obj->width < 10)
    obj->width = 10;
      

  switch(obj->type)
    {
    case XGD_LABEL:
      {
	int y;
	int loop;
	int width = 0;
	int text = 0;
	
	if (obj->Obj.Label.lblstr){
	  y = obj->y;
	  for (loop = 0; loop < obj->Obj.Label.numlines; loop++){
	    y += obj->Obj.Label.font->ascent;
	    y += obj->Obj.Label.font->descent;
	    text = XTextWidth(obj->Obj.Label.font, obj->Obj.Label.lblstr[loop],
			     strlen(obj->Obj.Label.lblstr[loop]));
	    if (width < text)
	      width = text;

	  }
	  obj->height = y - obj->y;
	  obj->width = width;
	}
	if ( obj->bbox )
	  XgdUpdateBoxForObject(obj, obj->y, obj->y + obj->height, 
				obj->x, obj->x + obj->width);
	else
	  XgdCreateBoxForObject(obj);
	
      }
      break;
      
    case XGD_GEOFRAME:
      {
	G_set_window(&obj->Obj.GeoFrame.region);

	/* The aspect ratio must be maintained with respect to the  */
	/* region associated with the Geographic frame.             */
	aratio = (obj->Obj.GeoFrame.region.east - obj->Obj.GeoFrame.region.west)/(obj->Obj.GeoFrame.region.north - obj->Obj.GeoFrame.region.south);
	
	/* Re calcuate the larger of the 2 dimension to make them conform */
	/*  to the aspect ratio.                                          */
	if (width > height)
	  obj->width = (double)height * aratio;
	else
	  obj->height = (double)width / aratio;

	/* If the dimensions have changed, the old pixmap needs to be */
	/* destroyed and a new one created.                           */
	changed = (obj->width != oldw || obj->height != oldh);
	if (obj->Obj.GeoFrame.pixmap != NULL && changed)
	  XFreePixmap(obj->display, obj->Obj.GeoFrame.pixmap);

	if (changed || obj->Obj.GeoFrame.pixmap == NULL){
	  XgdSiteInfo *tmp;
	  
	  obj->Obj.GeoFrame.pixmap = XCreatePixmap(obj->display, obj->window,
						   obj->width + 2 * obj->lw,
						   obj->height + 2 * obj->lw,
						   DefaultDepth(obj->display,
						  DefaultScreen(obj->display)));

    
	  tmp = obj->Obj.GeoFrame.sites.site;
	  while (tmp){
	    /* re-proportion the size of the site to fit the size of */
	    /* the geoframe.  Why 40?  Cause it looked good.         */

	    if (tmp->type == XGD_SITE_STANDARD){
	      tmp->Site.def.size =  obj->width / 40;
	      
	      if (tmp->Site.def.size < 1)
		tmp->Site.def.size = 1;
	      if (tmp->Site.def.size > 10)
		tmp->Site.def.size = 10;
	    }
	    tmp = tmp->next;
	  }
	  XgdRedrawGeoframe(obj, True, True, NULL);
	}
      }
      
      _xgdDrawPixmapBorder(obj, gc);
      
      if ( obj->bbox )
	XgdUpdateBoxForObject(obj, obj->y, obj->y + obj->height, 
			      obj->x, obj->x + obj->width);
      else
	XgdCreateBoxForObject(obj);
      
      break;
    case XGD_POLYLINE:
    case XGD_POLYGON:
      tmp = obj->Obj.Polyline.pts;

      /* If the dimesions have changed, the points need to be resized */
      /* accordingly.  Points are stored in bounding box coordinates  */
      /* not window coordiantes so this will actually work :-)        */
      if ((oldw != obj->width || oldh != obj->height) && oldw != 0){
	dx = ((double)obj->width / (double)oldw);
	dy = ((double)obj->height / (double)oldh);
	while (tmp){
	  tmp->x = (int) (dx * tmp->x);
	  tmp->y = (int) (dy * tmp->y);
	  tmp->box->t = tmp->y - 3;
	  tmp->box->b = tmp->y + 3;
	  tmp->box->l = tmp->x - 3;
	  tmp->box->r = tmp->x + 3;
	  tmp = tmp->next;
	}
      }
      
      tmp = obj->Obj.Polyline.pts;

      maxx = minx = tmp->x;
      maxy = miny = tmp->y;

      tmp = tmp->next;
      
      /* Find the max/min x and y points to make sure the bounding box */
      /* is defined properly.                                          */
      while (tmp != NULL){
	if (tmp->x < minx)
	  minx = tmp->x;
	if (tmp->x > maxx)
	  maxx = tmp->x;
	if (tmp->y > maxy)
	  maxy = tmp->y;
	if (tmp->y < miny)
	  miny = tmp->y;

	tmp = tmp->next;
      }
      
      if (abs(maxx - minx) != oldw || abs(maxy - miny) != oldh){
	if (minx > 0 && miny > 0){
	  obj->x = minx;
	  obj->y = miny;
	} else {
	  obj->x += minx;
	  obj->y += miny;
	  tmp = obj->Obj.Polyline.pts;
	  while(tmp){
	    tmp->x -= minx;
	    tmp->y -= miny;
	    tmp->box->t = tmp->y - 3;
	    tmp->box->b = tmp->y + 3;
	    tmp->box->l = tmp->x - 3;
	    tmp->box->r = tmp->x + 3;
	    tmp = tmp->next;
	  }
	}
	obj->width = abs(maxx - minx);
	obj->height = abs(maxy - miny);
      }
      
      if ( obj->bbox )
	XgdUpdateBoxForObject(obj, obj->y, obj->y + obj->height, 
			      obj->x, obj->x + obj->width);
      else{
	XgdCreateBoxForObject(obj);
	/* If the bbox is Null, this is the first time the object has */
	/* been drawn, so the points must be converted from the       */
	/* window coordinate system to the bounding box coordinate    */
	/* system.                                                    */
	_xgdWindowPointsToBoxPoints(obj);
      }

      break;
      
    case XGD_OPEN_INTERP_SPLINE:
    case XGD_CLOSED_INTERP_SPLINE:
    case XGD_OPEN_APPROX_SPLINE:
    case XGD_CLOSED_APPROX_SPLINE:
      tmp = obj->Obj.Spline.points;

      /* See above - recalculate points if dimesions change */
      if ((oldw != obj->width || oldh != obj->height) && oldw != 0){
	dx = ((double)obj->width / (double)oldw);
	dy = ((double)obj->height / (double)oldh);
	while (tmp){
	  tmp->x = (int) (dx * tmp->x);
	  tmp->y = (int) (dy * tmp->y);
	  tmp->box->t = tmp->y - 3;
	  tmp->box->b = tmp->y + 3;
	  tmp->box->l = tmp->x - 3;
	  tmp->box->r = tmp->x + 3;
	  tmp = tmp->next;
	}
      }

      /* if bbox is null, this is the first time that the points are */
      /* being draw, so we need to convert the points from window    */
      /* coordinates to bounding box coordinates.                    */
      if (!obj->bbox){
	if (obj->type == XGD_CLOSED_INTERP_SPLINE ||
	                               obj->type == XGD_CLOSED_APPROX_SPLINE)
	  XgdAddPointToPointList(obj, obj->Obj.Spline.points->x,
				 obj->Obj.Spline.points->y);
	
	tmp = obj->Obj.Spline.points;
	
	minx = tmp->x;
	miny = tmp->y;
	
	while (tmp != NULL){
	  if (tmp->x < minx)
	    minx = tmp->x;
	  if (tmp->y < miny)
	    miny = tmp->y;
	  
	  tmp = tmp->next;
	}
	
	
	tmp = obj->Obj.Spline.points;

	for (; tmp != NULL; tmp = tmp->next) {
	  tmp->x -= minx;
	  tmp->y -= miny;
	  tmp->box->t = tmp->y - 3;
	  tmp->box->b = tmp->y + 3;
	  tmp->box->l = tmp->x - 3;
	  tmp->box->r = tmp->x + 3;
	}
	
	obj->x = minx;
	obj->y = miny;
      }

      break;

    case XGD_LEGEND:
      obj->Obj.Legend.fit = True;
      return;
      
    case XGD_BARSCALE:
	
	if (obj->bbox)
	XgdUpdateBoxForObject(obj, obj->y, obj->y + obj->height, 
			      obj->x, obj->x + obj->width);
      else
	XgdCreateBoxForObject(obj);
	break;
      
    default:
      if ( obj->bbox )
	XgdUpdateBoxForObject(obj, obj->y, obj->y + obj->height, 
			      obj->x, obj->x + obj->width);
      else
	XgdCreateBoxForObject(obj);
      
      break;
    }

  XgdConfigureResizeHandles(obj);
  
  if (obj->lw == 1)
    obj->lw = 0;
}

/*
 ***************************************************************************
 * _xgdDrawPixmap - Private Library function that draws a border around
 *    the object's pixmap.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
_xgdDrawPixmapBorder(obj, gc)
     XgdObject *obj;
     GC        gc;
#else     
_xgdDrawPixmapBorder(XgdObject *obj, GC gc)
#endif
{
  XDrawRectangle(obj->display, obj->Obj.GeoFrame.pixmap, gc, 0, 0,
		 obj->width - 1, obj->height - 1);
  
/*  XDrawLine(obj->display, obj->Obj.GeoFrame.pixmap, gc, 0, 0,
	    obj->width, 0);
  XDrawLine(obj->display, obj->Obj.GeoFrame.pixmap, gc, 0, 0,
	    0, obj->height);
  XDrawLine(obj->display, obj->Obj.GeoFrame.pixmap, gc,
	    obj->width - 1, 0,
	    obj->width - 1, obj->height - 1);
  XDrawLine(obj->display, obj->Obj.GeoFrame.pixmap, gc,
	    0, obj->height - 1,
	    obj->width - 1, obj->height - 1);*/
}


#ifdef _NO_PROTO
_xgdWindowPointsToBoxPoints(obj)
     XgdObject *obj;
#else
_xgdWindowPointsToBoxPoints(XgdObject *obj)
#endif
{
  XgdPointList *tmp = obj->Obj.Polyline.pts;;

  while (tmp != NULL){
    tmp->x -= obj->bbox->l;
    tmp->y -= obj->bbox->t;
    tmp->box->t -= obj->bbox->t;
    tmp->box->b -= obj->bbox->t;
    tmp->box->l -= obj->bbox->l;
    tmp->box->r -= obj->bbox->l;
    tmp = tmp->next;
  }
}

/*
 ***************************************************************************
 * XgdCreateFillPixmap - Create the FIll Pixmap specified by the integer
 *   index (fp) into the array of fill pixmaps.
 ***************************************************************************
 */
Pixmap
#ifdef _NO_PROTO
XgdCreateFillPixmap(obj, fg, bg, fp)
     XgdObject *obj;
     Pixel     fg, bg;
     int    fp;
#else
XgdCreateFillPixmap(XgdObject *obj, Pixel fg, Pixel bg, int fp)
#endif
{
  int screen;
  
  if ((fp < 0) || (fp > XtNumber(__XGDfillPatternButtonTable))){
    XgdWarning("Fill Pixmap number out of range.  Using 0.");
    fp = 0;
  }

  screen = DefaultScreen(obj->display);
  if (__XGDfillPatternButtonTable[fp].bitmap != NULL)
    return(XCreatePixmapFromBitmapData(obj->display, obj->window, 
				       __XGDfillPatternButtonTable[fp].bitmap,
				       __XGDfillPatternButtonTable[fp].width,
				       __XGDfillPatternButtonTable[fp].height,
				       fg, bg,
				       DefaultDepth(obj->display, screen)));
  else
    return NULL;
}

/*
 ***************************************************************************
 * XgdAddPointToList - Add the given point into the object's pointlist.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdAddPointToPointList(obj, x, y)
     XgdObject *obj;
     int       x;
     int       y;
#else
XgdAddPointToPointList(XgdObject *obj, int x, int y)
#endif
{
  if (obj == NULL)
    XgdError("Object in XgdAddPointToPolyline was NULL.");
  
  if (obj->type == XGD_POLYGON || obj->type == XGD_POLYLINE){
    if (obj->Obj.Polyline.pts == NULL){
      obj->Obj.Polyline.pts = (XgdPointList *) XtMalloc(sizeof(XgdPointList));
      obj->Obj.Polyline.Tail = obj->Obj.Polyline.pts;
    }
    else{
      obj->Obj.Polyline.Tail->next = (XgdPointList *)
	XtMalloc(sizeof(XgdPointList));
      obj->Obj.Polyline.Tail = obj->Obj.Polyline.Tail->next;
    }
    
    obj->Obj.Polyline.Tail->x = x;
    obj->Obj.Polyline.Tail->y = y;
    obj->Obj.Polyline.Tail->box = XgdSetBox(y - 3, y + 3,
					    x - 3, x + 3);
    obj->Obj.Polyline.Tail->next = NULL;
  }
  else{
    if (obj->Obj.Spline.points == NULL){
      obj->Obj.Spline.points = (XgdPointList *) XtMalloc(sizeof(XgdPointList));
      obj->Obj.Spline.ptail = obj->Obj.Spline.points;
    }
    else{
      obj->Obj.Spline.ptail->next = (XgdPointList *)
	XtMalloc(sizeof(XgdPointList));
      obj->Obj.Spline.ptail = obj->Obj.Spline.ptail->next;
    }
    
    obj->Obj.Spline.ptail->x = x ;
    obj->Obj.Spline.ptail->y = y ;
    obj->Obj.Spline.ptail->box = XgdSetBox(y - 3, y + 3, x - 3, x + 3);
    obj->Obj.Spline.ptail->next = NULL;
  }
}

/*
 ***************************************************************************
 * _xgdGetBitmapPad - private library function that returns the bitmap
 *   pad info.
 ***************************************************************************
 */
int
#ifdef _NO_PROTO
_XgdGetBitmapPad(vinfo)
        XVisualInfo     vinfo;
#else
_XgdGetBitmapPad(XVisualInfo vinfo)
#endif
{

        if (vinfo.depth > 16)
                return (32);

        else if (vinfo.depth > 8)
                return (16);

        else
                return (8);

}

void
#ifdef _NO_PROTO
XgdResizePixmap(obj, nwidth, nheight, xoff, yoff, enlarge)
     XgdObject *obj;
     int       nwidth;
     int       nheight;
     int       xoff;
     int       yoff;
     Boolean   enlarge;
#else
XgdResizePixmap(XgdObject *obj, int nwidth, int nheight, int xoff, int yoff,
		Boolean enlarge)
#endif
{
  Pixmap npix;

  npix = XCreatePixmap(obj->display, obj->window, nwidth, nheight,
		       DefaultDepth(obj->display,DefaultScreen(obj->display)));
		       
  XSetForeground(obj->display, obj->objgc, obj->bg);
  XFillRectangle(obj->display, npix, obj->objgc, 0, 0, nwidth, nheight);
  XSetForeground(obj->display, obj->objgc, obj->fg);
  
  if (enlarge)
    XCopyArea(obj->display, obj->Obj.GeoFrame.pixmap, npix,
	      obj->objgc, 0, 0, obj->width, obj->height,xoff,yoff);
  else
    XCopyArea(obj->display, obj->Obj.GeoFrame.pixmap, npix, obj->objgc,
	      xoff, yoff, nwidth, nheight, 0, 0);
  
  XFreePixmap(obj->display, obj->Obj.GeoFrame.pixmap);
  obj->Obj.GeoFrame.pixmap = npix;
  
  if (enlarge){
    obj->x -= xoff;
    obj->y -= yoff;
    obj->width = nwidth;
    obj->height = nheight;
  
    XgdUpdateBoxForObject(obj, obj->y, obj->y + nheight,
			  obj->x, obj->x + nwidth);
  }
  else{
    obj->x += xoff;
    obj->y += yoff;
    obj->width = nwidth;
    obj->height = nheight;

    XgdUpdateBoxForObject(obj, obj->y, obj->y + nheight,
			  obj->x, obj->x + nwidth);
  }    
  
}

void
#ifdef _NO_PROTO
XgdDeleteRaster(obj)
     XgdObject *obj;
#else
XgdDeleteRaster(XgdObject *obj)
#endif
{
  if (obj == NULL)
    XgdError("Object in XgdDeleteRaster is NULL");
  
  if (obj->type != XGD_GEOFRAME){
    XgdWarning("Object in XgdDeleteRaster is not a GeoFrame");
    return;
  }
  obj->Obj.GeoFrame.colorsExist = False;
  _XgdFreeColors(obj);
  if (obj->Obj.GeoFrame.rname){
    obj->Obj.GeoFrame.rname = NULL;
    obj->Obj.GeoFrame.rmapset = NULL;    
  }
}

void
#ifdef _NO_PROTO
XgdDeleteMap(obj, map, type)
     XgdObject *obj;
     char      *map;
     int        type;
#else
XgdDeleteMap(XgdObject *obj, char *map, int type)
#endif
{
  XgdVectInfo *vtmp, *vprev = NULL;
  XgdSiteInfo *stmp, *sprev = NULL;
  
  if (type == XGD_VECTOR){
    for (vtmp = obj->Obj.GeoFrame.vects.vect; vtmp; vprev = vtmp, vtmp = vtmp->next){
      if (!strcmp(map, vtmp->vname)){
	if (vprev){
	  if (!vtmp->next)
	    obj->Obj.GeoFrame.vects.Tail = vprev;
	  vprev->next = vtmp->next;
	  return;
	} else {
	  obj->Obj.GeoFrame.vects.vect = vtmp->next;
	  if (!vtmp->next)
	    obj->Obj.GeoFrame.vects.Tail = NULL;
	}
      }
    }
  } else { 
    for (stmp = obj->Obj.GeoFrame.sites.site; stmp; sprev = stmp, stmp = stmp->next){
      if (!strcmp(map, stmp->sname)){
	if (stmp->type == XGD_SITE_PIXMAP){
	  int i;

	  for (i = 0; i < stmp->Site.pixdef.count; i++)
	    XDestroyWindow(obj->display, stmp->Site.pixdef.pwin[i]);
	}
	if (sprev){
	  if (!stmp->next)
	    obj->Obj.GeoFrame.sites.Tail = sprev;
	  sprev->next = stmp->next;
	  return;
	} else {
	  obj->Obj.GeoFrame.sites.site = stmp->next;
	  if (!stmp->next)
	    obj->Obj.GeoFrame.sites.Tail = NULL;
	}
      }
    }
  }
}
     
