/******************************************************************
 *  This file was blatently stolen from the gis library and
 *  converted for use by the Xgd library. :-)
 *
 *
 * Plot lines and filled polygons. Input space is database window.
 * Output space and output functions are user defined.
 * Converts input east,north lines and polygons to output x,y
 * and calls user supplied line drawing routines to do the plotting.
 *
 * Handles global wrap-around for lat-lon databases.
 *
 * Does not perform window clipping.
 * Clipping must be done by the line draw routines supplied by the user.
 *
 * Note:
 *  Hopefully, cartographic style projection plotting will be added later.
 *******************************************************************/
#include "xgrass_dlib.h"
#include "gis.h"

static double __xgdxconv, __xgdyconv;
static double __xgdleft, __xgdright, __xgdtop, _xgdbottom;
static int __xgdymin, __xgdymax;
static struct Cell_head __xgdwindow;

/******************************/
/* static function prototypes */
/******************************/

static double _xgdFabs(
#ifndef _NO_PROTO
    double x
#endif
);

static void _xgdFastLine(
#ifndef _NO_PROTO
     XgdObject *obj,
     double x1, double y1, double x2, double y2, Pixmap pix
#endif
);

static double _xgdNearest(
#ifndef _NO_PROTO
    double e0, double e1
#endif
);

static int _xgdEdge(
#ifndef _NO_PROTO
    double x0, double y0, double x1, double y1
#endif
);

static int _xgdEdgePoint(
#ifndef _NO_PROTO
    double x,
    register int y
#endif
);

static int _xgdEdgeOrder(
#ifndef _NO_PROTO
    struct point *a, struct poiny *b
#endif
);

static void _xgdRowFill(
#ifndef _NO_PROTO
     XgdObject *obj,
     int     y,
     double x1, double x2
#endif
);

static int _xgdIFloor(
#ifndef _NO_PROTO
     double x
#endif
);

static int _xgdICiel(
#ifndef _NO_PROTO
     double x
#endif
);

double XgdAdjustEastLongitude (
#ifndef _NO_PROTO			       
    double east,
    double west
#endif
);


double XgdAdjustEasting (
#ifndef _NO_PROTO
    double east,
    struct Cell_head *window
#endif
);

/*************************/
/*   end of prototypes   */
/*************************/

/*
 ***************************************************************************
 * _xgdFabs - reinvention of the wheel
 ***************************************************************************
 */
static double
#ifdef _NO_PROTO
_xgdFabs(x)
    double x;
#else
_xgdFabs(double x)
#endif
{
    return x>0?x:-x;
}

double XgdAdjustEastLongitude (east, west)
    double east, west;
{
    while (east > west + 360.0)
	east -=360.0;
    while (east <= west)
	east += 360.0;
    return east;
}

double XgdAdjustEasting (east, window)
    double east;
    struct Cell_head *window;
{
    if (window->proj == PROJECTION_LL)
    {
	east = XgdAdjustEastLongitude(east, window->west);
	if (east > window->east && east == window->west + 360)
	    east = window->west;
    }
    return east;
}


/*
 ***************************************************************************
 * G_setup_plot (t, b, l, r)
 *     double t, b, l, r;
 *     int (*Move)(), (*Cont)();
 *
 * initialize the plotting capability.
 *    t,b,l,r:   __xgdtop, _xgdbottom, __xgdleft, __xgdright of the
 *                                   output x,y coordinate space.
 *    Move,Cont: subroutines that will draw lines in x,y space.
 *       Move(x,y)   move to x,y (no draw)
 *       Cont(x,y)   draw from previous position to x,y
 * Notes:
 *   Cont() is responsible for clipping.
 *   The t,b,l,r are only used to compute coordinate transformations.
 *   The input space is assumed to be the current GRASS window.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdSetupPlot (obj, t, b, l, r)
     XgdObject *obj;
     double t, b, l, r;
#else
XgdSetupPlot (XgdObject *obj, double t, double b, double l, double r)
#endif
{
  __xgdwindow = obj->Obj.GeoFrame.region;

  __xgdleft = l;
  __xgdright = r;
  __xgdtop = t;
  _xgdbottom = b;
  
  __xgdxconv = (__xgdright-__xgdleft)/(__xgdwindow.east-__xgdwindow.west);
  __xgdyconv = (_xgdbottom-__xgdtop)/(__xgdwindow.north-__xgdwindow.south);
  
  if (__xgdtop < _xgdbottom)
    {
      __xgdymin = _xgdICiel(__xgdtop);
      __xgdymax = _xgdIFloor(_xgdbottom);
    }
  else
    {
      __xgdymin = _xgdICiel(_xgdbottom);
      __xgdymax = _xgdIFloor(__xgdtop);
    }
}

#define X(e) (__xgdleft + __xgdxconv * ((e) - __xgdwindow.west))
#define Y(n) (__xgdtop + __xgdyconv * (__xgdwindow.north - (n)))

#define EAST(x) (__xgdwindow.west + ((x)-__xgdleft)/__xgdxconv)
#define NORTH(y) (__xgdwindow.north - ((y)-__xgdtop)/__xgdxconv)

/*
 ***************************************************************************
 * XgdPlotWhereXY
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdPlotWhereXY (east, north, x, y)
    double east, north;
    int *x, *y;
#else
XgdPlotWhereXY (double east, double north, int *x, int *y)
#endif
{
    *x = _xgdIFloor(X(XgdAdjustEasting(east,&__xgdwindow))+0.5);
    *y = _xgdIFloor(Y(north)+0.5);
}

/*
 ***************************************************************************
 * XgdPlotWhereEn - Brother of XY?
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdPlotWhereEN (x, y, east, north)
     int x, y;
     double *east, *north;
#else
XgdPlotWhereEN (int x, int y, double *east, double *north)
#endif
{
    *east = XgdAdjustEasting(EAST(x),&__xgdwindow);
    *north = NORTH(y);
}

/*
 ***************************************************************************
 * XgdPlotPoint - plot the given point
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdPlotPoint(obj, east, north, pix)
     XgdObject *obj;
     double east, north;
#else
XgdPlotPoint(XgdObject *obj, double east, double north, Pixmap pix)
#endif
{
  int x,y;

  XgdPlotWhereXY(east,north,&x,&y);
  XDrawPoint(obj->display, pix?pix:obj->Obj.GeoFrame.pixmap, obj->objgc, x, y);
}

/*
 ***************************************************************************
 *_xgdFastLine converts double rows/cols to ints then plots
 * this is ok for graphics, but not the best for vector to raster
 ***************************************************************************
 */

static void
#ifdef _NO_PROTO
_xgdFastLine(obj, x1,y1,x2,y2, pix)
     XgdObject *obj;
     double x1,y1,x2,y2;
     Pixmap pix;
#else
_xgdFastLine(XgdObject *obj, double x1, double y1,
	     double x2, double y2, Pixmap pix)
#endif
{
  XDrawLine(obj->display, pix?pix:obj->Obj.GeoFrame.pixmap, obj->objgc,
	    _xgdIFloor(x1+0.5),_xgdIFloor(y1+0.5),
	    _xgdIFloor(x2+0.5),_xgdIFloor(y2+0.5));
}

/*
 ***************************************************************************
 * XgdPlotLine - Draw the line given by (east1, north1) - (east2, north2)
 *    into the object's pixmap.
 ***************************************************************************
 */
void 
#ifdef _NO_PROTO
XgdPlotLine (obj, east1, north1, east2, north2, pix)
     XgdObject *obj;
     double east1, north1, east2, north2;
     Pixmap pix;
#else
XgdPlotLine (XgdObject *obj, double east1, double north1,
	       double east2, double north2, Pixmap pix)
#endif
{
    double x1,x2,y1,y2;

    y1 = Y(north1);
    y2 = Y(north2);

    if (__xgdwindow.proj == PROJECTION_LL)
    {
	if (east1 > east2)
	    while ((east1-east2) > 180)
		east2 += 360;
	else if (east2 > east1)
	    while ((east2-east1) > 180)
		east1 += 360;
	while (east1 > __xgdwindow.east)
	{
	    east1 -= 360.0;
	    east2 -= 360.0;
	}
	while (east1 < __xgdwindow.west)
	{
	    east1 += 360.0;
	    east2 += 360.0;
	}
	x1 = X(east1);
	x2 = X(east2);

	_xgdFastLine(obj, x1, y1, x2, y2, pix);

	if (east2 > __xgdwindow.east || east2 < __xgdwindow.west)
	{
	    while (east2 > __xgdwindow.east)
	    {
		east1 -= 360.0;
		east2 -= 360.0;
	    }
	    while (east2 < __xgdwindow.west)
	    {
		east1 += 360.0;
		east2 += 360.0;
	    }
	    x1 = X(east1);
	    x2 = X(east2);
	    _xgdFastLine(obj, x1, y1, x2, y2, pix);
	}
    }
    else
    {
	x1 = X(east1);
	x2 = X(east2);
	_xgdFastLine(obj, x1, y1, x2, y2, pix);
    }
}
/*
 ***************************************************************************
 * G_plot_polygon (x, y, n)
 * 
 *    double *x       x coordinates of vertices
 *    double *y       y coordinates of vertices
 *    int n           number of verticies
 *
 * polygon fill from map coordinate space to plot x,y space.
 *     for lat-lon, handles global wrap-around as well as polar polygons.
 *
 * returns 0 ok, 2 n<3, -1 weird internal error, 1 no memory
 ***************************************************************************
 */

#define POINT struct point
POINT
{
    double x;
    int y;
};

static POINT *P;
static int np;
static int npalloc = 0;

#define OK 0
#define TOO_FEW__XGDEDGES 2
#define NO_MEMORY 1
#define OUT_OF_SYNC -1

/*
 ***************************************************************************
 * _xgdNearest
 ***************************************************************************
 */
static double
#ifdef _NO_PROTO
_xgdNearest(e0,e1)
    double e0, e1;
#else
_xgdNearest(double e0, double e1)
#endif
{
    while (e0 - e1 > 180)
	e1 += 360.0;
    while (e1 - e0 > 180)
	e1 -= 360.0;
    
    return e1;
}


int
#ifdef _NO_PROTO
XgdPlotPolygon (obj, x, y, n)
     XgdObject *obj;
     double *x, *y;
     int    n;
#else
XgdPlotPolygon (XgdObject *obj, double *x, double *y, int n)
#endif
{
    int i;
    int pole;
    double x0,x1;
    double y0,y1;
    double shift,E,W;
    double e0,e1;
    int shift1, shift2;

    if (n < 3)
        return TOO_FEW__XGDEDGES;

/* traverse the perimeter */

    np = 0;
    shift1 = 0;

/* global wrap-around for lat-lon, part1 */
    if (__xgdwindow.proj == PROJECTION_LL)
    {
	/*
	pole = G_pole_in_polygon(x,y,n);
	*/
pole = 0;

	e0 = x[n-1];
	E = W = e0;

	x0 = X(e0);
	y0 = Y(y[n-1]);

	if (pole &&!_xgdEdge (x0, y0, x0, Y(90.0*pole)))
		return NO_MEMORY;

	for (i = 0; i < n; i++)
	{
	    e1 = _xgdNearest (e0, x[i]);
	    if (e1 > E) E = e1;
	    if (e1 < W) W = e1;

	    x1 = X(e1);
	    y1 = Y(y[i]);

	    if(!_xgdEdge (x0, y0, x1, y1))
		return NO_MEMORY;

	    x0 = x1;
	    y0 = y1;
	    e0 = e1;
	}
	if (pole &&!_xgdEdge (x0, y0, x0, Y(90.0*pole)))
		return NO_MEMORY;

	shift = 0;        /* shift into window */
	while (E+shift > __xgdwindow.east)
	    shift -= 360.0;
	while (E+shift < __xgdwindow.west)
	    shift += 360.0;
	shift1 = X(x[n-1]+shift) - X(x[n-1]);
    }
    else
    {
	x0 = X(x[n-1]);
	y0 = Y(y[n-1]);

	for (i = 0; i < n; i++)
	{
	    x1 = X(x[i]);
	    y1 = Y(y[i]);
	    if(!_xgdEdge (x0, y0, x1, y1))
		return NO_MEMORY;
	    x0 = x1;
	    y0 = y1;
	}
    }

/* check if perimeter has odd number of points */
    if (np%2)
        return OUT_OF_SYNC;

/* sort the _xgdEdge points by col(x) and then by row(y) */
    qsort (P, np, sizeof(POINT), _xgdEdgeOrder);

/* plot */
    for (i = 1; i < np; i += 2)
    {
        if (P[i].y != P[i-1].y)
	    return OUT_OF_SYNC;
	_xgdRowFill (obj, P[i].y, P[i-1].x+shift1, P[i].x+shift1);
    }
    if (__xgdwindow.proj == PROJECTION_LL)	/* now do wrap-around, part 2 */
    {
	shift = 0;
	while (W+shift < __xgdwindow.west)
	    shift += 360.0;
	while (W+shift > __xgdwindow.east)
	    shift -= 360.0;
	shift2 = X(x[n-1]+shift) - X(x[n-1]);
	if (shift2 != shift1)
	{
	    for (i = 1; i < np; i += 2)
	    {
		_xgdRowFill (obj, P[i].y, P[i-1].x+shift2, P[i].x+shift2);
	    }
	}
    }
    return OK;
}

/*
 ***************************************************************************
 * _xgdEdge
 ***************************************************************************
 */
static int
#ifdef _NO_PROTO
_xgdEdge (x0, y0, x1, y1)
    double x0, y0, x1, y1;
#else
_xgdEdge (double x0, double y0, double x1, double y1)
#endif
{
    register double m;
    double dy, x;
    int ystart, ys__xgdtop;


/* tolerance to avoid FPE */
    dy = y0 - y1;
    if (_xgdFabs(dy) < 1e-10)
	return 1;

    m = (x0 - x1) / dy;

    if (y0 < y1)
    {
        ystart = _xgdICiel  (y0);
	ys__xgdtop  = _xgdIFloor (y1);
	if (ys__xgdtop == y1) ys__xgdtop--; /* if line s__xgdtops at row center, don't include point */
    }
    else
    {
        ystart = _xgdICiel  (y1);
	ys__xgdtop  = _xgdIFloor (y0);
	if (ys__xgdtop == y0) ys__xgdtop--; /* if line s__xgdtops at row center, don't include point */
    }
    if (ystart > ys__xgdtop)
	return 1;	/* does not cross center line of row */

    x = m * (ystart - y0) + x0;
    while (ystart <= ys__xgdtop)
    {
	if(!_xgdEdgePoint (x, ystart++))
	    return 0;
	x += m;
    }
    return 1;
}

/*
 ***************************************************************************
 * _xgdEdgePoint
 ***************************************************************************
 */
static int
#ifdef _NO_PROTO
_xgdEdgePoint (x, y)
    double x;
    register int y;
#else
_xgdEdgePoint (double x, register int y)
#endif
{
    if (y < __xgdymin || y > __xgdymax)
	return 1;
    if (np >= npalloc)
    {
	if (npalloc > 0)
	{
	    npalloc *= 2;
	    P = (POINT *) realloc (P, npalloc * sizeof (POINT));
	}
	else
	{
	    npalloc = 32;
	    P = (POINT *) malloc (npalloc * sizeof (POINT));
	}
	if (P == NULL)
	{
	    npalloc = 0;
	    return 0;
	}
    }
    P[np].x   = x;
    P[np++].y = y;
    return 1;
}

/*
 ***************************************************************************
 * _xgdEdgeOrder
 ***************************************************************************
 */
static int
#ifdef _NO_PROTO
_xgdEdgeOrder (a, b)
    struct point *a, *b;
#else
_xgdEdgeOrder (struct point *a, struct point *b)
#endif
{
    if (a->y < b->y) return (-1);
    if (a->y > b->y) return (1);

    if (a->x < b->x) return (-1);
    if (a->x > b->x) return (1);

    return (0);
}

/*
 ***************************************************************************
 * _xgdRowFill
 ***************************************************************************
 */
static void
#ifdef _NO_PROTO
_xgdRowFill (obj, y, x1, x2)
     XgdObject *obj;
     int     y;
     double x1,x2;
#else
_xgdRowFill (XgdObject *obj, int y, double x1, double x2)
#endif
{
  int i1,i2;

  i1 = _xgdICiel(x1);
  i2 = _xgdIFloor(x2);
  if (i1 <= i2)
    {
      XDrawLine(obj->display, obj->Obj.GeoFrame.pixmap, obj->objgc,
		i1, y, i2, y);
    }
}

/*
 ***************************************************************************
 * _xgdFloor
 ***************************************************************************
 */
static int
#ifdef _NO_PROTO
_xgdIFloor(x)
     double x;
#else
_xgdIFloor(double x)
#endif
{
    int i;
    i = (int) x;
    if (i > x) i--;
    return i;
}

/*
 ***************************************************************************
 * _xgdCiel
 ***************************************************************************
 */
static int
#ifdef _NO_PROTO
_xgdICiel(x)
     double x;
#else
_xgdICiel(double x)
#endif
{
    int i;

    i = (int) x;
    if (i < x) i++;
    return i;
}

