#include "xgrass_dlib.h"
#include "Vect.h"
#include "ptlines.h"

/*
 ***************************************************************************
 * 
 * XgdDrawVector - Draw the given vector map with name 'name' and in mapset
 * 'mapset', putting this information in the GeoFrame data structure.  The
 * plot functions draw into the Obj.GeoFrame.pixmap instead of drawing
 * directly to the window. 
 **************************************************************************
 * 
 */
void
#ifdef _NO_PROTO
XgdInitVector(obj, name, mapset, color, lp, lw)
  XgdObject      *obj;
  char     *name;
  char     *mapset;
  Pixel     color;
  int       lp, lw;
#else
XgdInitVector(XgdObject * obj, char *name, char *mapset, Pixel color, int lp, int lw)
#endif
{
  if (obj->type != XGD_GEOFRAME) {
    XgdWarning("Object in XgdInitVector is not a GeoFrame.");
    return;
  }
  /* create space in the list of vector maps associated with a geoframe */
  if (obj->Obj.GeoFrame.vects.vect == NULL) {
    obj->Obj.GeoFrame.vects.vect = (XgdVectInfo *) XtMalloc(sizeof(XgdVectInfo));
    obj->Obj.GeoFrame.vects.Tail = obj->Obj.GeoFrame.vects.vect;
  } else {
    obj->Obj.GeoFrame.vects.Tail->next = (XgdVectInfo *)
      XtMalloc(sizeof(XgdVectInfo));
    obj->Obj.GeoFrame.vects.Tail = obj->Obj.GeoFrame.vects.Tail->next;
  }
  obj->Obj.GeoFrame.vects.Tail->next = NULL;

  /* create room and stick the name and mapset in the structure */
  obj->Obj.GeoFrame.vects.Tail->vname = (char *)
    XtMalloc(sizeof(char) * strlen(name) +1);
  obj->Obj.GeoFrame.vects.Tail->vmapset = (char *)
    XtMalloc(sizeof(char) * strlen(mapset) +1);

  strcpy(obj->Obj.GeoFrame.vects.Tail->vname, name);
  strcpy(obj->Obj.GeoFrame.vects.Tail->vmapset, mapset);

  /* stash away the current fg, linepattern, and width for later */
  obj->Obj.GeoFrame.vects.Tail->color = color;
  obj->Obj.GeoFrame.vects.Tail->linewidth = lw;
  obj->Obj.GeoFrame.vects.Tail->linepattern = lp;
}

void
#ifdef _NO_PROTO
XgdDrawVector(obj, name, mapset, pix)
     XgdObject *obj;
     char      *name;
     char      *mapset;
     Pixmap    pix;
#else
XgdDrawVector(XgdObject *obj, char *name, char *mapset, Pixmap pix)
#endif
{
  Pixel     fg;
  int       lp, lw;
  XgdVectInfo    *v;

  if (obj->type != XGD_GEOFRAME) {
    XgdWarning("Object in XgdDrawVect is not a GeoFrame.");
    return;
  }
  fg = obj->fg;
  lp = obj->lp;
  lw = obj->lw;
  v = obj->Obj.GeoFrame.vects.vect;
  while (v) {
    if (!strcmp(v->vname, name) && !strcmp(v->vmapset, mapset)) {
      XgdSetObjectForeground(obj, v->color);
      XgdSetObjectLinePattern(obj, v->linewidth, v->linepattern);
      _xgdDrawVector(obj, v->vname, v->vmapset, pix);
      XgdSetObjectForeground(obj, fg);
      XgdSetObjectLinePattern(obj, lw, lp);
    }
    v = v->next;
  }

  /* Copy the pixmap onto the window associated with the object */
  XCopyArea(obj->display, obj->Obj.GeoFrame.pixmap, pix?pix:obj->window,
	    obj->objgc, 0, 0, obj->width, obj->height, obj->x, obj->y);
}

/*
 ***************************************************************************
 * _xgdDrawVector - Private function for library calls Draw the vector into the
 * give objects pixmap, not storing the name and mapset information.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
_xgdDrawVector(obj, name, mapset, pix)
     XgdObject *obj;
     char      *name;
     char      *mapset;
     Pixmap    pix;
#else
_xgdDrawVector(XgdObject *obj, char *name, char *mapset, Pixmap pix)
#endif
{
  struct Map_info Map;
  struct line_pnts *points;
  int       type;
  int       i;
  int       oldfp;
  double   *x, *y;

  oldfp = obj->fp;

  XgdSetObjectFillPattern(obj, obj->fg, obj->bg, XGD_FILL_PATTERN_NONE);
  if (obj->Obj.GeoFrame.gridOn && obj->Obj.GeoFrame.grid.labelon) {
    XRectangle      rect[1];
    rect[0].x = obj->Obj.GeoFrame.grid.xoff;
    rect[0].y = obj->Obj.GeoFrame.grid.yoff;
    rect[0].width = obj->width;
    rect[0].height = obj->height;

    XSetClipRectangles(obj->display, obj->objgc, 0, 0, rect, 1, Unsorted);
    XgdSetupPlot(obj, (double) obj->Obj.GeoFrame.grid.yoff,
     (double) obj->height, (double) obj->Obj.GeoFrame.grid.xoff,
           (double) obj->width);
  } else
    XgdSetupPlot(obj, 0.0, (double) obj->height, 0.0, (double) obj->width);

  Vect_set_open_level(1);
  if (Vect_open_old(&Map, name, mapset) < 0)
    XgdError("Couldn't open Vector map.");

  points = Vect_new_line_struct();

  /*
   * loop through the map one line at a time.  -2 indicates end of
   * mapfile
   */
  while ((type = Vect_read_next_line(&Map, points)) >= 0) {
    x = (double *) XtMalloc(sizeof(double) * points->n_points);
    y = (double *) XtMalloc(sizeof(double) * points->n_points);
    for (i = 0; i < points->n_points; i++) {
      x[i] = points->x[i];
      y[i] = points->y[i];
    }
    switch (type) {
    case LINE:
      for (i = 1; i < points->n_points; i++)
        XgdPlotLine(obj, x[i - 1], y[i - 1], x[i], y[i], pix);
      break;
    case DOT:
      XgdPlotPoint(obj, x[0], y[0], pix);
      break;
    case AREA:
      for (i = 1; i < points->n_points; i++)
        XgdPlotLine(obj, x[i - 1], y[i - 1], x[i], y[i], pix);
      break;
    default:
      break;
    }
    XtFree(x);
    XtFree(y);
  }
  Vect_destroy_line_struct(points);
  Vect_close(&Map);


  XgdSetObjectFillPattern(obj, obj->fg, obj->bg, oldfp);

  XSetClipMask(obj->display, obj->objgc, None);

}


/*
 ***************************************************************************
 * 
 * XgdDrawRaster - Draw the specified raster map.  The map is draw into the
 * pixmap associated with obj, not to the screen.
 *************************************************************************** 
 */
void
#ifdef _NO_PROTO
XgdDrawRaster(obj, name, mapset, pix)
     XgdObject *obj;
     char      *name;
     char      *mapset;
     Pixmap     pix;
#else
XgdDrawRaster(XgdObject *obj, char *name, char *mapset, Pixmap pix)
#endif
{
  if (obj == NULL)
    XgdError("Object in XgdDrawRaster was NULL.");

  if (obj->type != XGD_GEOFRAME) {
    XgdWarning("Object type in XgdDrawRaster was not XGD_GEOFRAME.");
    return;
  }
  obj->Obj.GeoFrame.rname = (char *) XtMalloc(sizeof(char) * strlen(name) +1);
  strcpy(obj->Obj.GeoFrame.rname, name);
  obj->Obj.GeoFrame.rmapset = (char *) XtMalloc(sizeof(char) * strlen(mapset) +1);
  strcpy(obj->Obj.GeoFrame.rmapset, mapset);
  if (obj->Obj.GeoFrame.lookup_tbl) {
    obj->Obj.GeoFrame.colorsExist = False;
    _XgdFreeColors(obj);
  }
  if (obj->Obj.GeoFrame.legend) {
    XgdUnDrawObject(obj->Obj.GeoFrame.legend, obj->bg, True);
    XgdSetLegendsDisplayList(obj->Obj.GeoFrame.legend, NULL);
    G_read_cats(name, mapset, &obj->Obj.GeoFrame.legend->Obj.Legend.cats);
  }
  XgdRedrawGeoframe(obj, True, True, pix);
}

_xgdDrawRaster(obj, name, mapset, pix)
  XgdObject      *obj;
  char     *name, *mapset;
{
  XgdCreateImage(obj, DefaultScreen(obj->display), name, mapset, 1, pix);
}

/*
 ***************************************************************************
 * 
 * XgdDrawObject - draw the given object with the gc.  Fill != 0 if the object
 * is to be filled.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdDrawObject(gc, obj, fill, pix)
     GC        gc;
     XgdObject *obj;
     int       fill;
     Pixmap    pix;
#else
XgdDrawObject(GC gc, XgdObject *obj, int fill, Pixmap pix)
#endif
{
  int       i, loop;
  XgdPointList   *p1, *p2;
  XPoint   *p;
  int       oldfp;
  int       x1, y1;

  if (obj == NULL)
    XgdError("Object in XgdDrawObject was NULL.");

  switch (obj->type) {
  case XGD_LEGEND:
    XgdDrawLegend(obj, pix);
    break;
  case XGD_SQUARE:
    if (fill && obj->fp != XGD_FILL_PATTERN_NONE)
      XFillRectangle(obj->display, pix?pix:obj->window, gc, obj->x,
               obj->y, obj->width,
               obj->height);

    if (obj->lp != XGD_LINE_PATTERN_NONE) {
      oldfp = obj->fp;
      XgdSetObjectFillPattern(obj, obj->fg, obj->bg, XGD_FILL_PATTERN_NONE);
      XDrawRectangle(obj->display, pix?pix:obj->window, gc, obj->x,
               obj->y, obj->width,
               obj->height);
      XgdSetObjectFillPattern(obj, obj->fg, obj->bg, oldfp);
    }
    break;
  case XGD_RECTANGLE:
    if (fill && obj->fp != XGD_FILL_PATTERN_NONE)
      XFillRectangle(obj->display, pix?pix:obj->window, gc, obj->x,
               obj->y, obj->width,
               obj->height);

    if (obj->lp != XGD_LINE_PATTERN_NONE) {
      oldfp = obj->fp;
      XgdSetObjectFillPattern(obj, obj->fg, obj->bg, XGD_FILL_PATTERN_NONE);
      XDrawRectangle(obj->display, pix?pix:obj->window, gc, obj->x,
               obj->y, obj->width,
               obj->height);
      XgdSetObjectFillPattern(obj, obj->fg, obj->bg, oldfp);
    }
    break;
  case XGD_CIRCLE:
    if (fill && obj->fp != XGD_FILL_PATTERN_NONE)
      XFillArc(obj->display, pix?pix:obj->window, gc, obj->x,
         obj->y, obj->width,
         obj->height, 0, 360 * 64);

    if (obj->lp != XGD_LINE_PATTERN_NONE) {
      oldfp = obj->fp;
      XgdSetObjectFillPattern(obj, obj->fg, obj->bg, XGD_FILL_PATTERN_NONE);
      XDrawArc(obj->display, pix?pix:obj->window, gc, obj->x,
         obj->y, obj->width,
         obj->height, 0, 360 * 64);
      XgdSetObjectFillPattern(obj, obj->fg, obj->bg, oldfp);
    }
    break;
  case XGD_ELLIPSE:
    if (fill && obj->fp != XGD_FILL_PATTERN_NONE)
      XFillArc(obj->display, pix?pix:obj->window, gc, obj->x,
         obj->y, obj->width,
         obj->height, 0, 360 * 64);

    if (obj->lp != XGD_LINE_PATTERN_NONE) {
      oldfp = obj->fp;
      XgdSetObjectFillPattern(obj, obj->fg, obj->bg, XGD_FILL_PATTERN_NONE);
      XDrawArc(obj->display, pix?pix:obj->window, gc, obj->x,
         obj->y, obj->width,
         obj->height, 0, 360 * 64);
      XgdSetObjectFillPattern(obj, obj->fg, obj->bg, oldfp);
    }
    break;
  case XGD_GEOFRAME:
    {
      XgdSiteInfo    *s;
      XCopyArea(obj->display, obj->Obj.GeoFrame.pixmap, pix?pix:obj->window,
		obj->objgc,
		0, 0, obj->width, obj->height, obj->x, obj->y);
      s = obj->Obj.GeoFrame.sites.site;
      while (s != NULL) {
        switch (s->type) {
	case XGD_SITE_PIXMAP:
	  _xgdReDrawSitePixmap(obj, s);
	  break;
        }
        s = s->next;
      }
    }
    break;
  case XGD_POLYLINE:
  case XGD_FAKE_POLYLINE:
    oldfp = obj->fp;
    XgdSetObjectFillPattern(obj, obj->fg, obj->bg, XGD_FILL_PATTERN_NONE);
    if (obj->lp != XGD_LINE_PATTERN_NONE) {
      if (fill == -100 || obj->type == XGD_FAKE_POLYLINE)
        p1 = obj->Obj.Spline.points;
      else
        p1 = obj->Obj.Polyline.pts;

      if (p1 == NULL)
        return;
      p2 = p1->next;
      if (p2 == NULL)
        return;
      while (p2 != NULL) {
        XDrawLine(obj->display, pix?pix:obj->window, gc,
            p1->x + obj->x, p1->y + obj->y,
            p2->x + obj->x, p2->y + obj->y);
        p1 = p2;
        p2 = p2->next;
      }
      XgdSetObjectFillPattern(obj, obj->fg, obj->bg, oldfp);
    }
    break;

  case XGD_OPEN_INTERP_SPLINE:
  case XGD_CLOSED_INTERP_SPLINE:
    _xgdCreateSplineobject(obj, INTERP, pix);
    break;
  case XGD_OPEN_APPROX_SPLINE:
  case XGD_CLOSED_APPROX_SPLINE:
    _xgdCreateSplineobject(obj, APPROX, pix);
    break;
  case XGD_POLYGON:
    p1 = obj->Obj.Polyline.pts;
    if (p1 == NULL)
      return;
    for (i = 0; p1 != NULL; i++, p1 = p1->next);

    p = (XPoint *) XtMalloc(sizeof(XPoint) * i);
    p1 = obj->Obj.Polyline.pts;
    for (loop = 0; loop < i; loop++, p1 = p1->next) {
      _xgdBoxPointsToWindowPoints(obj, p1, &x1, &y1);
      p[loop].x = x1;
      p[loop].y = y1;
    }

    if (fill && obj->fp != XGD_FILL_PATTERN_NONE)
      XFillPolygon(obj->display, pix?pix:obj->window, gc, p, i, Complex,
             CoordModeOrigin);

    if (obj->lp != XGD_LINE_PATTERN_NONE) {
      oldfp = obj->fp;
      XgdSetObjectFillPattern(obj, obj->fg, obj->bg, XGD_FILL_PATTERN_NONE);
      XDrawLines(obj->display, pix?pix:obj->window, gc, p, i, CoordModeOrigin);
      XDrawLine(obj->display, pix?pix:obj->window, gc,
          p[i - 1].x, p[i - 1].y, p[0].x, p[0].y);
      XgdSetObjectFillPattern(obj, obj->fg, obj->bg, oldfp);
    }
    break;

  case XGD_LABEL:
    XgdDrawLabel(obj, pix);
    break;

  case XGD_BARSCALE:
    XgdDrawBarscale(obj, False, pix);
    break;

  default:
    break;
  }
}

extern double   floor();

void
#ifdef _NO_PROTO
XgdDrawGrid(obj, gfnorth, gfsouth, gfeast, gfwest, t, b, l, r, gridgap, pix)
     XgdObject      *obj;
     double    gfnorth, gfsouth, gfeast, gfwest;
     double    t, b, l, r;
     double    gridgap;
     Pixmap    pix;
#else
XgdDrawGrid(XgdObject * obj,
      double gfnorth, double gfsouth, double gfeast, double gfwest,
      double t, double b, double l, double r, double gridgap, Pixmap pix)
#endif
{
  double    east, west, incr;
  double    g;
  int       i;
  GC        geoframeGC = obj->objgc;

  obj->objgc = obj->Obj.GeoFrame.grid.gc;

  XgdSetupPlot(obj, t, b, l, r);
  XSetForeground(obj->display, obj->objgc, obj->Obj.GeoFrame.grid.color);

  west = gfwest;
  incr = (gfeast - gfwest) / 3;

  for (i = 0; i < 3; i++) {
    east = west + incr;
    g = floor(gfnorth / gridgap) * gridgap;

    for (; g > gfsouth; g -= gridgap) {
      XgdPlotLine(obj, west, g, east, g, pix);
    }
    west = east;
  }

  g = floor(gfeast / gridgap) * gridgap;

  for (; g > gfwest; g -= gridgap) {
    XgdPlotLine(obj, g, gfnorth, g, gfsouth, pix);
  }

  obj->objgc = geoframeGC;
}


#define XGD_METERS_TO_INCHES    ((double)39.37)
#define XGD_MILES_TO_KILOMETERS ((double)1.609344)

void
#ifdef _NO_PROTO
XgdDrawBarscale(obj, updtemode, pix)
     XgdObject      *obj;
     Boolean   updtemode;
     Pixmap    pix;
#else
XgdDrawBarscale(XgdObject * obj, Boolean updtemode, Pixmap pix)
#endif
{
  int       x, y;
  double    interval;
  int       intervalLength, scaleLength;
  int       gap;
  int       ox, oy, ow, oh;
  int       unitTextLength;
  int       startTextLength;
  int       endTextLength;
  int       textAscent, textDescent;
  char      endText[30];
  char      unitText[30];
  int       xstart, ystart;
  int       bScaleLength;

  x = obj->x;
  y = obj->y;

  intervalLength = 0;
  scaleLength = 0;

  XSetForeground(obj->display, obj->objgc, obj->Obj.Barscale.color);
  interval = (double) obj->Obj.Barscale.length /
    (double) obj->Obj.Barscale.intervals;

  startTextLength = _XgdGetTextLength(obj->display, obj->objgc, "0");
  sprintf(endText, "%8.2f", obj->Obj.Barscale.length);

  endTextLength = _XgdGetTextLength(obj->display, obj->objgc, endText);
  _XgdGetTextAscentDescent(obj->display, obj->objgc, endText,
       &textAscent, &textDescent);

  xstart = x + startTextLength / 2;
  ystart = y + 5 + textAscent;

  switch (obj->Obj.Barscale.unit) {
  case XGD_KILOMETERS:
    intervalLength = interval * 1000;
    scaleLength = obj->Obj.Barscale.length * 1000;
    strcpy(unitText, " km");
    break;

  case XGD_METERS:
    intervalLength = interval;
    scaleLength = obj->Obj.Barscale.length;
    strcpy(unitText, " m");
    break;

  case XGD_MILES:
    intervalLength = ((double) interval *
         XGD_MILES_TO_KILOMETERS) * 1000;
    scaleLength = ((double) obj->Obj.Barscale.length *
         XGD_MILES_TO_KILOMETERS) * 1000;
    strcpy(unitText, " mi");
    break;

  case XGD_FEET:
    intervalLength = ((float) interval /
         XGD_METERS_TO_INCHES) * 12;
    scaleLength = ((float) obj->Obj.Barscale.length /
         XGD_METERS_TO_INCHES) * 12;
    strcpy(unitText, " ft");
    break;

  default:
    return;
  }
  unitTextLength = _XgdGetTextLength(obj->display, obj->objgc, unitText);

  XSetForeground(obj->display, obj->objgc, obj->Obj.Barscale.color);
  XSetLineAttributes(obj->display, obj->objgc, obj->Obj.Barscale.linewidth,
         LineSolid, JoinRound, FillSolid);
  XSetFont(obj->display, obj->objgc, obj->Obj.Barscale.fid);

  if (obj->Obj.Barscale.style == DASHED)
    gap = 0;
  else
    gap = obj->Obj.Barscale.linewidth / 2;

  _XgdGetBarscaleLength(&bScaleLength,
       obj->Obj.Barscale.winwidth,
       scaleLength);
  intervalLength = (int)(((double)bScaleLength - 
                           ((double)(obj->Obj.Barscale.intervals + 1) * 
                            (double)gap)) / 
                         (double)obj->Obj.Barscale.intervals);

  switch (obj->Obj.Barscale.style) {
  case DASHED:
    {
      _XgdDrawDashedBarscale(obj->display, pix?pix:obj->window, obj->objgc,
        xstart, ystart, obj->Obj.Barscale.linewidth,
             intervalLength, gap,
             obj->Obj.Barscale.intervals,
             &ox, &oy, &ow, &oh, pix);
    }

    break;

  case TICKED:
    {
      _XgdDrawTickedBarscale(obj->display, pix?pix:obj->window, obj->objgc,
             xstart, ystart, obj->Obj.Barscale.linewidth, intervalLength, gap,
             obj->Obj.Barscale.intervals,
             &ox, &oy, &ow, &oh, pix);
    }
    break;
  }

  XSetLineAttributes(obj->display, obj->objgc, 0,
         LineSolid, JoinRound,
         FillSolid);


  XSetForeground(obj->display, obj->objgc, obj->Obj.Barscale.textcolor);
  XDrawString(obj->display, pix?pix:obj->window, obj->objgc,
        x, y + textAscent, "0", strlen("0"));
  XDrawString(obj->display, pix?pix:obj->window, obj->objgc,
        ox + ow - endTextLength / 2, y + textAscent,
        endText, strlen(endText));

  XDrawString(obj->display, pix?pix:obj->window, obj->objgc,
        ox + ow + endTextLength / 2, y + textAscent,
        unitText, strlen(unitText));

  XSetForeground(obj->display, obj->objgc,
         BlackPixel(obj->display, DefaultScreen(obj->display)));


  if (!updtemode)
    return;

  obj->x = x;
  obj->y = y;
  obj->width = ow + endTextLength / 2 + unitTextLength + 4;
  obj->height = oh + 5 + textAscent;

}



void
#ifdef _NO_PROTO
_XgdDrawDashedBarscale(dpy, win, gc, x, y, thick,
       dlen, gap, num, ox, oy, ow, oh, pix)
     Display  *dpy;
     Window    win;
     GC        gc;
     int       x, y;
     int       thick;
     int       dlen, gap, num;
     int      *ox, *oy, *ow, *oh;
     Pixmap   pix;
#else
_XgdDrawDashedBarscale(
       Display * dpy,
       Window win,
       GC gc,
       int x, int y, int thick,
       int dlen, int gap, int num,
       int *ox, int *oy, int *ow, int *oh, Pixmap pix)
#endif
{
  int       i;
  *ox = x;
  *oy = y;

  XSetLineAttributes(dpy, gc, 0, LineSolid,
         JoinRound, FillSolid);

  for (i = 0; i < num; i++) {
    if (i % 2 == 0) {
      XDrawRectangle(dpy, pix?pix:win, gc, x, y, dlen - 1, thick);
      XFillRectangle(dpy, pix?pix:win, gc, x, y + thick, dlen, thick + 1);
    } else {
      XFillRectangle(dpy, pix?pix:win, gc, x, y, dlen, thick + 1);
      XDrawRectangle(dpy, pix?pix:win, gc, x, y + thick, dlen - 1, thick);
    }
    x = x + dlen;
  }

  *ow = dlen * num;
  *oh = thick + thick + thick / 2;
}


void
#ifdef _NO_PROTO
_XgdDrawTickedBarscale(dpy, win, gc, x, y, thick,
       dlen, gap, num, ox, oy, ow, oh, pix)
  Display  *dpy;
  Window    win;
  GC        gc;
  int       x, y;
  int       thick;
  int       dlen, gap, num;
  int      *ox, *oy, *ow, *oh;
     Pixmap pix;
#else
_XgdDrawTickedBarscale(
       Display * dpy,
       Window win,
       GC gc,
       int x, int y, int thick,
       int dlen, int gap, int num,
       int *ox, int *oy, int *ow, int *oh, Pixmap pix)
#endif
{
  int       i;

  *ox = x;
  *oy = y;


  XSetLineAttributes(dpy, gc, thick / 2, LineSolid,
         JoinRound, FillSolid);

  XDrawLine(dpy, pix?pix:win, gc, x, y,
      x, y + thick + thick + 20);


  for (i = 0; i < num; i++) {
    if (i == 0) {
      XDrawLine(dpy, pix?pix:win, gc, x, y + thick + 10,
        x + dlen * num + gap * num, y + thick + 10);
    }
    x = x + dlen + gap;

    if (i < num - 1) {
      XSetLineAttributes(dpy, gc, thick / 2,
             LineSolid, JoinRound, FillSolid);
      XDrawLine(dpy, pix?pix:win, gc, x, y + 5,
          x, y + thick + thick + 15);
    }
  }

  XSetLineAttributes(dpy, gc, thick / 2,
         LineSolid, JoinRound, FillSolid);

  XDrawLine(dpy, pix?pix:win, gc, *ox + dlen * num + gap * num, y,
      *ox + dlen * num + gap * num, y + thick + thick + 20);

  *ow = x - gap - *ox;
  *oh = thick + thick + 22;

}



void
#ifdef _NO_PROTO  
_XgdGetBarscaleLength(dlen, wwidth, length)
  int      *dlen;
  int       wwidth, length;
#else
_XgdGetBarscaleLength(int *dlen, int wwidth, int length)
#endif
{
  struct Cell_head window;

  G_get_set_window(&window);
  *dlen = (int) (length * wwidth) /
    (window.east - window.west);
}
#ifdef _NO_PROTO
_xgdBoxPointsToWindowPoints(obj, p, x, y)
  XgdObject      *obj;
  XgdPointList   *p;
  int      *x;
  int      *y;
#else
_xgdBoxPointsToWindowPoints(XgdObject * obj, XgdPointList *p, int *x, int *y)
#endif
{
  *x = p->x + obj->x;
  *y = p->y + obj->y;
}

/*
 ***************************************************************************
 * 
 * XgdUnDrawObject - draw the given object with the gc. Fill != 0 if the object
 * is to be filled. bg is the background color of the window where object was
 * drawn.
 **************************************************************************
 * 
 */
void
#ifdef _NO_PROTO
XgdUnDrawObject(obj, bg, fill)
  XgdObject      *obj;
  Pixel     bg;
  int       fill;
#else
XgdUnDrawObject(XgdObject * obj, Pixel bg, int fill)
#endif
{
  int       oldFp;
  Pixel     oldFg, oldBg;

  if (obj == NULL)
    XgdError("Object in XgdUnDrawObject was NULL.");

  /* save old fg and bg colors */
  oldFg = obj->fg;
  oldBg = obj->bg;
  oldFp = obj->fp;

  if (obj->type == XGD_GEOFRAME || obj->type == XGD_LEGEND) {
    int i;
    XgdSiteInfo *s;
    
    XClearArea(obj->display, obj->window, obj->x, obj->y,
         obj->width, obj->height, False);

    return;
  }
  /* set both background and foreground to the given background */
  XgdSetObjectBackground(obj, bg);
  XgdSetObjectForeground(obj, bg);
  XgdSetObjectFillPattern(obj, bg, bg, XGD_FILL_PATTERN_SOLID);
  if (obj->type == XGD_FAKE_POLYLINE)
    XgdDrawObject(obj->objgc, obj, -100, NULL);
  else
    XgdDrawObject(obj->objgc, obj, fill, NULL);
  XgdSetObjectBackground(obj, oldBg);
  XgdSetObjectForeground(obj, oldFg);
  XgdSetObjectFillPattern(obj, oldFg, oldBg, oldFp);

  if (obj->type == XGD_BARSCALE)
    XClearArea(obj->display, obj->window, obj->x, obj->y, obj->width,
         obj->height, False);
}

void
#ifdef _NO_PROTO
XgdRedrawGeoframe(obj, drawRaster, drawIT, pix)
     XgdObject      *obj;
     Boolean   drawRaster; /* Redraw the reaster? */
     Boolean   drawIT;  /* actually draw to the window? */
     Pixmap    pix;
#else
XgdRedrawGeoframe(XgdObject *obj, Boolean drawRaster,
		  Boolean drawIT, Pixmap pix)
#endif
{
  struct Cell_head oldwin;
  XgdVectInfo    *v;
  XgdSiteInfo    *s;

  if (obj == NULL)
    XgdError("Object in XgdRedrawGeoframe was NULL.");

  if (obj->type != XGD_GEOFRAME) {
    XgdWarning("Object type != XGD_GEOFRAME in XgdRedrawGeoframe");
    return;
  }
  G_get_set_window(&oldwin);
  G_set_window(&obj->Obj.GeoFrame.region);

  /* If there was a raster map in the Pixmap, redraw it */
  if (drawRaster) {
    XSetForeground(obj->display, obj->objgc, obj->bg);

    XFillRectangle(obj->display, obj->Obj.GeoFrame.pixmap,
             obj->objgc, 0, 0, obj->width, obj->height);

    XSetForeground(obj->display, obj->objgc, obj->fg);


    if (obj->Obj.GeoFrame.rname) {
      _xgdDrawRaster(obj, obj->Obj.GeoFrame.rname,
		     obj->Obj.GeoFrame.rmapset, pix);
    }
    /* If any vector maps were present, redraw them as well */
    v = obj->Obj.GeoFrame.vects.vect;
    while (v) {
      Pixel     fg;
      int       lp, lw;

      fg = obj->fg;
      lp = obj->lp;
      lw = obj->lw;
      XgdSetObjectForeground(obj, v->color);
      XgdSetObjectLinePattern(obj, v->linewidth, v->linepattern);
      _xgdDrawVector(obj, v->vname, v->vmapset, pix);
      XgdSetObjectForeground(obj, fg);
      XgdSetObjectLinePattern(obj, lw, lp);
      v = v->next;
    }

  }
  s = obj->Obj.GeoFrame.sites.site;
  while (s != NULL) {
    switch (s->type) {
    case XGD_SITE_STANDARD:
      _xgdDrawSiteStd(obj, s);
      break;
    case XGD_SITE_PIXMAP:
      _xgdReDrawSitePixmap(obj, s);
      break;
    }
    s = s->next;
  }

  if (obj->Obj.GeoFrame.numbarscales != 0) {
    int i;
    for (i = 0; i < obj->Obj.GeoFrame.numbarscales; i++) {
      if  ( XgdIsGridOn(obj) ) {
	XgdSetBarscaleWinWidth(obj->Obj.GeoFrame.barscales[i],
			       obj->width - obj->Obj.GeoFrame.grid.xoff);
      } else {
	XgdSetBarscaleWinWidth(obj->Obj.GeoFrame.barscales[i], 
			       obj->width);
      }
      XgdUnDrawObject(obj->Obj.GeoFrame.barscales[i],
		      (obj->Obj.GeoFrame.barscales[i])->bg, True);

      XgdDrawBarscale(obj->Obj.GeoFrame.barscales[i], True, NULL);
      
      if ( (obj->Obj.GeoFrame.barscales[i])->bbox ) {
	XgdUpdateBoxForObject( obj->Obj.GeoFrame.barscales[i],
			      (obj->Obj.GeoFrame.barscales[i])->y, 
			      (obj->Obj.GeoFrame.barscales[i])->y + 
			      (obj->Obj.GeoFrame.barscales[i])->height,
			      (obj->Obj.GeoFrame.barscales[i])->x,
			      (obj->Obj.GeoFrame.barscales[i])->x + 
			      (obj->Obj.GeoFrame.barscales[i])->width);

      } else {
	XgdCreateBoxForObject(obj);
      }
    }
  }
	  

  if (obj->Obj.GeoFrame.gridOn) {
    XgdDrawGrid(obj, obj->Obj.GeoFrame.region.north,
		obj->Obj.GeoFrame.region.south, obj->Obj.GeoFrame.region.east,
		obj->Obj.GeoFrame.region.west,
		(double) obj->Obj.GeoFrame.grid.yoff,
		(double) obj->height, (double) obj->Obj.GeoFrame.grid.xoff,
		(double) obj->width,
		(double) obj->Obj.GeoFrame.grid.gridgap, pix);
    
    if (obj->Obj.GeoFrame.grid.labelon)
      DoGridLabel(obj, 2, &obj->Obj.GeoFrame.grid.xoff,
		  &obj->Obj.GeoFrame.grid.yoff);
  }
  G_set_window(&oldwin);
  _xgdDrawPixmapBorder(obj, obj->objgc);
  
  
  if (drawIT)
    XCopyArea(obj->display, obj->Obj.GeoFrame.pixmap, pix?pix:obj->window,
	      obj->objgc,0, 0, obj->width, obj->height, obj->x, obj->y);
}

void
#ifdef _NO_PROTO
XgdSetLegendBorderColor(obj, color)
  XgdObject      *obj;
  Pixel     color;
#else
XgdSetLgendBorderColor(XgdObject *obj, Pixel color)
#endif
{
  obj->Obj.Legend.brdrcolor = color;
}

Boolean
#ifdef _NO_PROTO
IsCatInList(obj, name)
  XgdObject      *obj;
  char     *name;
#else
IsCatInList(XgdObject * obj, char *name)
#endif
{
  int       i;

  if (!obj->Obj.Legend.catsToList)
    return True;

  for (i = 0; obj->Obj.Legend.catsToList[i]; i++)
    if (!strcmp(obj->Obj.Legend.catsToList[i], name))
      return True;

  return False;
}

/*
 ***************************************************************************
 * 
 * XgdDrawLegend - Draw the legend - See comments in code as to how this is
 * actually done
 **************************************************************************
 * 
 */
void
#ifdef _NO_PROTO
XgdDrawLegend(obj, pix)
     XgdObject *obj;
     Pixmap    pix;
#else
XgdDrawLegend(XgdObject *obj, Pixmap pix)
#endif
{
  /*
   ***************************************************************************
   * 
   * x - the x position to start drawing the color boxes y - the y postion
   * the draw the box and text height - the maximum height that each
   * entry in the column will have. width - the width of the text
   * string being printed to the screen totalwidth - the total width of
   * the object after drawing it - this is calculated on the fly
   * totalheight - the total height of the object after the legend is
   * drawn. this is also calcuated as the object is drawn. loop - the
   * inner loop to draw a column of catagories oloop - the outer loop
   * that controls the number of columns to be drawn name - the current
   * category's name pad - the amount of space to pad between category
   * entries to make it fill the entire height of the object. sub -
   * variable used to keep track of the number of categories that are
   * not to be displayed to keep the spacing even in the object.
   * numcats - the number of categories to display per column.
   **************************************************************************
   * 
   */
  int       x, y, width = 0, height, totalwidth = 20, totalheight = 0;
  int       loop;
  char     *name;
  double    pad = 1.0;
  int       sub = 0;
  int       numcats, oloop, numincol;
  int       count = 0, twidth = 0;
  Boolean   colFull = False;

  x = obj->x;
  y = 0;

  /*
   * if the objects font doesn't exist, get the info on the default
   * font
   */
  /* in the object's gc and store it for future use.         */
  if (!obj->Obj.Legend.font)
    obj->Obj.Legend.font = XQueryFont(obj->display,
                 XGContextFromGC(obj->objgc));

  height = obj->Obj.Legend.font->ascent + obj->Obj.Legend.font->descent;

  /*
   * if the object's height exists, force the categories being
   * displayed
   */
  /* to fit in that height by padding the space between categories       */
  if (obj->height) {
    if (!obj->Obj.Legend.catsToList) {
      numcats = obj->Obj.Legend.cats.num + 1;
    } else {
      for (numcats = 0; obj->Obj.Legend.catsToList[numcats]; numcats++);
    }

  } else {
    numcats = obj->Obj.Legend.cats.num + 1;
  }

  /*
   * doesn't make sense to have more colums than the number of
   * categories
   */
  /*
   * if (obj->Obj.Legend.numCols > numcats) obj->Obj.Legend.numCols =
   * numcats;
   */

  /* numCols stores the number of colums that the user wants displayed   */
  /* so, we divide the number of actual categories to display by the     */
  /* number of colums, round up, and that gives us the number of   */
  /* categories per column                 */

  /* outer loop goes from 0 to the number of columns */
  loop = 0;
  for (oloop = 0; oloop < obj->Obj.Legend.numCols; oloop++) {
    /*
     * inner loop goes from 0 -> the number of categories per
     * column
     */
    totalwidth = 0;
    numincol = (int) ((double) (numcats) /
      ((double) (obj->Obj.Legend.numCols - oloop)) + 0.9);

    pad = (double) obj->height / (double) (height * numincol);
    if (pad < 1.0){
      pad = 1.0;
    }
    for (; loop <= obj->Obj.Legend.cats.num; loop++) {
      /*
       * the equation loop + oloop * numcats will give you
       * the current category
       */
      /* name/position that should be used.             */
      name = G_get_cat(loop, &obj->Obj.Legend.cats);
      if (!colFull) {
        if (IsCatInList(obj, name)) {
          ++count;
	  XSetForeground(obj->display, obj->objgc,
		 obj->Obj.Legend.geoFrame->Obj.GeoFrame.lookup_tbl[loop]);
	  
	  XFillRectangle(obj->display, pix?pix:obj->window, obj->objgc, x,
			 obj->y + y, 10, height);
	  
	  XSetForeground(obj->display, obj->objgc, obj->Obj.Legend.brdrcolor);
	  XDrawRectangle(obj->display, pix?pix:obj->window, obj->objgc, x,
			 obj->y + y, 10, height);
	  
	  XSetForeground(obj->display, obj->objgc, obj->fg);

	  if (obj->Obj.Legend.toDisplay == XGD_DISPLAY_CAT_NAMES) {
	    width = XTextWidth(obj->Obj.Legend.font, name, strlen(name));
	    XDrawString(obj->display, pix?pix:obj->window, obj->objgc, x + 20,
		       obj->y + y + height - 2 - obj->Obj.Legend.font->descent,
		       name, strlen(name));
	  } else if (obj->Obj.Legend.toDisplay == XGD_DISPLAY_CAT_NUMS) {
	    char      str[10];
	    
	    sprintf(str, "%d", loop);
	    width = XTextWidth(obj->Obj.Legend.font, str, strlen(str));
	    XDrawString(obj->display, pix?pix:obj->window, obj->objgc, x + 20,
			obj->y + y + height - obj->Obj.Legend.font->descent,
			str, strlen(str));
	  }
	  y = (int) (height * count * pad);
	  
	  if (totalwidth < width + 20)
	    totalwidth = width + 20;
	  if (count == numincol)
	    colFull = True;
	}
	++sub;
      }
    }
    colFull = False;
    if (totalheight < y)
      totalheight = y;
    y = 0;
    twidth += totalwidth + 10;
    x += totalwidth + 10;
    totalwidth = 0;
    loop = sub;
    numcats -= count;
    count = 0;
  }

  if (!obj->height || totalheight > obj->height)
    obj->height = totalheight + 2;
  obj->width = twidth;

  if (obj->bbox)
    XgdUpdateBoxForObject(obj, obj->y, obj->y + obj->height,
              obj->x, obj->x + obj->width);
  else
    XgdCreateBoxForObject(obj);

  XgdConfigureResizeHandles(obj);
  
  obj->Obj.Legend.fit = False;
}
