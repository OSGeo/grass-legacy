#include "xgrass_dlib.h"

void
#ifdef _NO_PROTO
XgdUpdateGrid(obj, onOff, labelon,gap, spacing,
	      color, lw, lp, fid, textcolor, xoff, yoff)
     XgdObject *obj;
     Boolean onOff;
     int labelon;
     double gap;
     int spacing;
     Pixel color;
     int lw;
     int lp;
     Font fid;
     Pixel textcolor;
#else
XgdUpdateGrid(XgdObject *obj,
	      Boolean onOff,
	      int labelon,
	      double gap,
	      int spacing, 
	      Pixel color,
	      int lw,
	      int lp,
	      Font fid,
	      Pixel textcolor,
	      int xoff,
	      int yoff)
#endif
{
  if ( obj->Obj.GeoFrame.gridOn && !onOff ) {
    obj->Obj.GeoFrame.gridOn = onOff;
    XgdUnDrawObject(obj, obj->bg, True);
    if ( obj->Obj.GeoFrame.pixmap ) {
      XFreePixmap(obj->display, obj->Obj.GeoFrame.pixmap);
      obj->Obj.GeoFrame.pixmap = NULL;
    }
    XgdConfigureObject(obj->objgc, obj, obj->x, obj->y, 
		       obj->width, obj->height, True);
    XgdRedrawGeoframe(obj, False, True, NULL);
    return;
  }
  
  obj->Obj.GeoFrame.gridOn = onOff;
  if ( obj->Obj.GeoFrame.grid.gc == NULL ) {
    XGCValues gcv;
    
    gcv.background = obj->bg;
    obj->Obj.GeoFrame.grid.gc = 
      XCreateGC(obj->display, obj->window, 0, 0);
  }
  XSetBackground(obj->display, obj->Obj.GeoFrame.grid.gc, obj->bg);
  XgdSetGridLabelOn(obj, labelon);
  XgdSetGridGap(obj, gap);
  XgdSetGridSpacing(obj, spacing);
  XgdSetGridColor(obj, color);
  XgdSetGridLinePattern(obj, lw, lp);
  XgdSetGridFont(obj, fid);
  XgdSetGridTextColor(obj, textcolor);
  XgdSetGridOffset(obj, xoff, yoff);
  
  
  if (obj->Obj.GeoFrame.gridOn){
    XgdUnDrawObject(obj, obj->bg, True);
    XgdDrawGrid(obj, obj->Obj.GeoFrame.region.north,
		obj->Obj.GeoFrame.region.south, obj->Obj.GeoFrame.region.east,
		obj->Obj.GeoFrame.region.west, 
		(double)yoff, (double)obj->height, (double)xoff,
		(double)obj->width,
		(double)obj->Obj.GeoFrame.grid.gridgap, NULL);
    XgdDrawObject(obj->objgc, obj, True, NULL);
  }
}


void 
#ifdef _NO_PROTO
XgdSetGridLabelOn(obj, labelon)
    XgdObject *obj;
    int labelon;
#else
XgdSetGridLabelOn(XgdObject *obj, int labelon)
#endif
{
    obj->Obj.GeoFrame.grid.labelon = labelon;
}

void
#ifdef _NO_PROTO
XgdSetGridGap(obj, gap)
    XgdObject *obj;
    double gap;
#else
XgdSetGridGap( XgdObject *obj, double gap)
#endif
{
    obj->Obj.GeoFrame.grid.gridgap = gap;
}

void
#ifdef _NO_PROTO
XgdSetGridSpacing(obj, spacing)
    XgdObject *obj;
    int spacing;
#else
XgdSetGridSpacing( XgdObject *obj, int spacing)
#endif
{
    obj->Obj.GeoFrame.grid.spacing = spacing;
}

void
#ifdef _NO_PROTO
XgdSetGridColor(obj, fg)
    XgdObject *obj;
    Pixel fg;
#else
XgdSetGridColor( XgdObject *obj, Pixel fg)
#endif
{
    if ( obj->Obj.GeoFrame.grid.color == fg ) return;
    obj->Obj.GeoFrame.grid.color = fg;
    XSetForeground(obj->display, obj->Obj.GeoFrame.grid.gc, fg);
}

void
#ifdef _NO_PROTO
XgdSetGridLinePattern(obj, lw, lp)
    XgdObject *obj;
    int lw;
    int lp;
#else
XgdSetGridLinePattern( XgdObject *obj, int lw, int lp)
#endif
{
    int lstyle;

    if ( obj->Obj.GeoFrame.grid.linewidth == lw && 
         obj->Obj.GeoFrame.grid.linepattern == lp ) return;
    obj->Obj.GeoFrame.grid.linewidth = lw;
    obj->Obj.GeoFrame.grid.linepattern = lp;
    if (__XGDlinePatternButtonTable[lp].mode == XGD_DASHED_NONE) return;

    if (__XGDlinePatternButtonTable[lp].mode == XGD_DASHED_SOLID)
      lstyle = LineSolid;
    else
      lstyle = LineDoubleDash;
  
    XSetLineAttributes(obj->display, obj->Obj.GeoFrame.grid.gc, lw, lstyle, 
        CapButt, JoinMiter);
    if (lp != XGD_LINE_PATTERN_NONE && lp != XGD_LINE_PATTERN_SOLID)
      XSetDashes(obj->display, obj->objgc, 0,
                 __XGDlinePatternButtonTable[lp].dashes,
                 __XGDlinePatternButtonTable[lp].n);
}

void
#ifdef _NO_PROTO
XgdSetGridFontName(obj, name)
     XgdObject *obj;
     char *name;
#else
XgdSetGridFontName(XgdObject *obj, char *name)
#endif
{
    if ( obj->Obj.GeoFrame.grid.fontname ) 
        XtFree(obj->Obj.GeoFrame.grid.fontname);
    obj->Obj.GeoFrame.grid.fontname = XtNewString(name);
}

void
#ifdef _NO_PROTO
XgdSetGridFont(obj, fid)
    XgdObject *obj;
    Font fid;
#else
XgdSetGridFont( XgdObject *obj, Font fid)
#endif
{
    if ( obj->Obj.GeoFrame.grid.fid == fid ) return;
    obj->Obj.GeoFrame.grid.fid = fid;
    XSetFont(obj->display, obj->Obj.GeoFrame.grid.gc, fid);
}

void
#ifdef _NO_PROTO
XgdSetGridTextColor(obj, textcolor)
    XgdObject *obj;
    Pixel textcolor;
#else
XgdSetGridTextColor( XgdObject *obj, Pixel textcolor)
#endif
{
    obj->Obj.GeoFrame.grid.textcolor = textcolor;
}


void 
#ifdef _NO_PROTO
XgdSetGridOffset(obj, xoff, yoff)
    XgdObject *obj;
    int xoff, yoff;
#else
XgdSetGridOffset(XgdObject *obj, int xoff, int yoff)
#endif
{
    obj->Obj.GeoFrame.grid.xoff = xoff;
    obj->Obj.GeoFrame.grid.yoff = yoff;
}


Boolean
#ifdef _NO_PROTO
XgdIsGridOn(obj)
    XgdObject *obj;
#else
XgdIsGridOn( XgdObject *obj)
#endif
{
    return obj->Obj.GeoFrame.gridOn;
}

extern double floor() ;
#define _XGD_EASTING 0
#define _XGD_NORTHING 1

char *
#ifdef _NO_PROTO
_Xgd_format_coord(window, coord, round, nOrE)
    struct Cell_head window;
    double coord;
    int round;
    int nOrE;
#else
_Xgd_format_coord( struct Cell_head window, double coord, int round, int nOrE)
#endif
{
    char text[50];

    if (window.proj == PROJECTION_LL) {
        if ( nOrE == _XGD_EASTING ) {
	    G_format_easting (coord, text, window.proj);
        } else {
	    G_format_northing (coord, text, window.proj);
        }
    } else {
        coord = floor (coord / round);
        sprintf (text,"%.0lf", coord);
    }
    return XtNewString(text);
}

void
#ifdef _NO_PROTO
XgdCalculateGridOffset(obj, xoff, yoff)
    XgdObject *obj;
    int *xoff;
    int *yoff;
#else
XgdCalculateGridOffset( XgdObject *obj, int *xoff, int *yoff)
#endif
{
    double g;
    char num_text[50];
    int grid;
    int len;
    int rounded_grid;
    int xlenh; 
    int ascent,descent;

    XSetFont(obj->display,obj->Obj.GeoFrame.grid.gc,obj->Obj.GeoFrame.grid.fid);
        
    grid = obj->Obj.GeoFrame.grid.gridgap * 
	   obj->Obj.GeoFrame.grid.spacing;

/* round grid to multiple of 10 */
    rounded_grid = 1;
    if (obj->Obj.GeoFrame.region.proj != PROJECTION_LL)
    {
	sprintf (num_text, "%d", (int)obj->Obj.GeoFrame.grid.gridgap);
	len = strlen (num_text);
	while (len-- && num_text[len] == '0')
	    rounded_grid *= 10;
	if (rounded_grid == 10)
	    rounded_grid = 1;
    }

    g = floor (obj->Obj.GeoFrame.region.north/grid) * grid ;
    sprintf (num_text, "%s", _Xgd_format_coord(obj->Obj.GeoFrame.region,
        g, rounded_grid,_XGD_NORTHING));

    xlenh = _XgdGetTextLength(obj->display,
	obj->Obj.GeoFrame.grid.gc, num_text);

    _XgdGetTextAscentDescent(obj->display,
	    obj->Obj.GeoFrame.grid.gc,
	    num_text,&ascent,&descent);

    *xoff= xlenh + 4;
    *yoff= ascent + descent;
}

