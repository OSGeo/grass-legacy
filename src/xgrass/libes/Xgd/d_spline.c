#include "xgrass_dlib.h"
#include "ptlines.h"

/***************************************************************************
 * All the functions in this file are private functions to the Xgd library
 *  and should not be called by the programmer.
 ***************************************************************************/

short   __xgdptxmin, __xgdptxmax ,__xgdptymin, __xgdptymax;

#define MAXNUMPTS       4000
#define round(a)    (int)((a)+0.5)

static int      __xgdnpoints;
static int      __xgdmaxpoints;
static XPoint  *__xgdpoints;
static int      __xgdallocstep;
static int      __xgdstartx, __xgdstarty, __xgdendx, __xgdendy;

/*
 ***************************************************************************
 * _xgdInitPointArray - Create space for the points array
 ***************************************************************************
 */
int
#ifdef _NO_PROTO
_xgdInitPointArray(init_size, step_size)
     int             init_size, step_size;
#else
_xgdInitPointArray(int init_size, int step_size)
#endif
{
  __xgdnpoints = 0;
  __xgdmaxpoints = init_size;
  __xgdallocstep = step_size;
  if (__xgdmaxpoints > MAXNUMPTS)
    __xgdmaxpoints = MAXNUMPTS;
  if ((__xgdpoints = (XPoint *) XtMalloc(__xgdmaxpoints*sizeof(XPoint))) == 0){
    return 0;
  }
  return 1;
}

/*
 ***************************************************************************
 * _xgdAddPoint - Add a point into the points array
 ***************************************************************************
 */
int
#ifdef _NO_PROTO
_xgdAddPoint(x, y)
int x, y;
#else
_xgdAddPoint(int x, int y)
#endif
{
  if (__xgdnpoints >= (__xgdmaxpoints - 1)) {
    XPoint         *tmp_pt;
    
    __xgdmaxpoints += __xgdallocstep;
    if (__xgdmaxpoints > MAXNUMPTS)
      return 0;
    if ((tmp_pt = (XPoint *)
	 realloc(__xgdpoints, __xgdmaxpoints * sizeof(XPoint))) == 0) {
      fprintf(stderr, " insufficient memory to reallocate point array\n");
      return 0;
    }
    free(__xgdpoints);
    __xgdpoints = tmp_pt;
  }
  __xgdpoints[__xgdnpoints].x = (short) x;
  __xgdpoints[__xgdnpoints].y = (short) y;
  __xgdstartx = (short) x;
  __xgdstarty = (short) y;
  __xgdendx = (short) x;
  __xgdendy = (short) y;
  __xgdnpoints++;
  
  if (__xgdnpoints == 1){
    __xgdptxmin = __xgdptxmax = x;
    __xgdptymin = __xgdptymax = y;
  }
  
  if (__xgdptxmin > x)
    __xgdptxmin = x;
  if (__xgdptxmax < x)
    __xgdptxmax = x;
  if (__xgdptymin > y)
    __xgdptymin = y;
  if (__xgdptymax < y)
    __xgdptymax = y;
  
  return 1;
  
}

/*
 ***************************************************************************
 * _xgdDrawSpline - figure out what type of spline to draw, then draw it
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
_xgdDrawSpline(display, window, gc, fp, lp, spline, x, y, type)
     Display  *display;
     Drawable  window;
     GC        gc;
     int       fp, lp;
     XgdSpline *spline;
     int       x, y;
     int       type;
#else
_xgdDrawSpline(Display *display,
	       Drawable window,
	       GC gc,
	       int fp,
	       int lp,
	       XgdSpline *spline,
	       int x,
	       int y,
	       int type)
#endif
{
  if (type == XGD_OPEN_INTERP_SPLINE || type == XGD_OPEN_APPROX_SPLINE){
    _xgdDrawOpenSpline(display, window, gc, fp, lp, spline, x, y);
  } else {
    _xgdDrawClosedSpline(display, window, gc, fp, lp, spline, x, y);
  }
}

/*
 ***************************************************************************
 * _xgdDrawIntSpline - Draw an interpolated spline.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
_xgdDrawIntSpline(display, window, gc, fp, lp, spline, x, y,  op)
     Display   *display;
     Window     window;
     GC         gc;
     int        fp, lp;
     XgdSpline *spline;
     int        x;
     int        y;
     int        op;
#else
_xgdDrawIntSpline(Display *display,
		  Drawable window,
		  GC gc,
		  int fp,
		  int lp,
		  XgdSpline *spline,
		  int x,
		  int y,
		  int op)
#endif
{
  XgdPointList        *p1, *p2;
  XgdControlList      *cp1, *cp2;
  
  p1 = spline->points;
  cp1 = spline->controls;
  cp2 = cp1->next;
  
  
  if (!_xgdInitPointArray(300, 200))
    return;
  
  for (p2 = p1->next, cp2 = cp1->next; p2 != NULL; p1 = p2, cp1 = cp2,
       p2 = p2->next, cp2 = cp2->next) {
    _xgdBezierSpline((double) (p1->x + x), (double) (p1->y + y),
		  cp1->rx +(double) x, cp1->ry + (double)y,
		     cp2->lx + (double)x, cp2->ly + (double)y,
		  (double) (p2->x + x), (double) (p2->y + y), op);
    
  }
  
  _xgdAddPoint(p1->x + x, p1->y + y);

  if (fp != XGD_FILL_PATTERN_NONE)
    XFillPolygon(display, window, gc, __xgdpoints,
		 __xgdnpoints, Complex, CoordModeOrigin);

  if (lp != XGD_LINE_PATTERN_NONE)
    XDrawLines(display, window, gc, __xgdpoints,
	       __xgdnpoints, CoordModeOrigin);

}

/*
 ***************************************************************************
 * _xgdDrawOpenSpline - Draw an open spline
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
_xgdDrawOpenSpline(display, window, gc, fp, lp, spline, x, y)
     Display *display;
     Drawable window;
     GC       gc;
     int      fp, lp;
     XgdSpline *spline;
     int        x, y;
#else
_xgdDrawOpenSpline(Display *display,
		   Drawable window,
		   GC gc,
		   int fp,
		   int lp,
		   XgdSpline *spline,
		   int x,
		   int y)
#endif
{
  XgdPointList        *p;
  double           cx1, cy1, cx2, cy2, cx3, cy3, cx4, cy4;
  double           x1, y1, x2, y2;
  
  if (!_xgdInitPointArray(300, 200))
    return;
  
  p = spline->points;
  x1 = (double) (p->x + x);
  y1 = (double) (p->y + y);
  p = p->next;
  x2 = (double) (p->x + x);
  y2 = (double) (p->y + y);
  cx1 = (x1 + x2) / 2.0;
  cy1 = (y1 + y2) / 2.0;
  cx2 = (cx1 + x2) / 2.0;
  cy2 = (cy1 + y2) / 2.0;
  _xgdAddPoint((int) x1, (int) y1);
  
  for (p = p->next; p != NULL; p = p->next) {
    x1 = x2;
    y1 = y2;
    x2 = (double) (p->x + x);
    y2 = (double) (p->y + y);
    cx4 = (x1 + x2) / 2.0;
    cy4 = (y1 + y2) / 2.0;
    cx3 = (x1 + cx4) / 2.0;
    cy3 = (y1 + cy4) / 2.0;
    _xgdQuadraticSpline(cx1, cy1, cx2, cy2, cx3, cy3, cx4, cy4);
    cx1 = cx4;
    cy1 = cy4;
    cx2 = (cx1 + x2) / 2.0;
    cy2 = (cy1 + y2) / 2.0;
  }
  
  _xgdAddPoint(round(cx1), round(cy1));
  _xgdAddPoint((int) x2, (int) y2);

  if (fp != XGD_FILL_PATTERN_NONE)
    XFillPolygon(display, window, gc, __xgdpoints,
		 __xgdnpoints, Complex, CoordModeOrigin);

  if (lp != XGD_LINE_PATTERN_NONE)
    XDrawLines(display, window, gc,__xgdpoints, __xgdnpoints, CoordModeOrigin);
  
}

/*
 ***************************************************************************
 * _xgdDrawClosedSpline - Draw a closed spline
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
_xgdDrawClosedSpline(display, window, gc, fp, lp, spline, x, y)
     Display *display;
     Drawable window;
     GC       gc;
     int      fp, lp;
     XgdSpline *spline;
     int        x, y;
#else
_xgdDrawClosedSpline(Display *display,
		     Drawable window,
		     GC gc,
		     int fp,
		     int lp,
		     XgdSpline *spline,
		     int x,
		     int y)
#endif
{
  XgdPointList        *p;
  double           cx1, cy1, cx2, cy2, cx3, cy3, cx4, cy4;
  double           x1, y1, x2, y2;
  
  if (!_xgdInitPointArray(300, 200))
    return;
  
  p = spline->points;
  x1 = (double) (p->x + x);
  y1 = (double) (p->y + y);
  p = p->next;
  x2 = (double) (p->x + x);
  y2 = (double) (p->y + y);
  cx1 = (x1 + x2) / 2.0;
  cy1 = (y1 + y2) / 2.0;
  cx2 = (x1 + 3 * x2) / 4.0;
  cy2 = (y1 + 3 * y2) / 4.0;
  
  for (p = p->next; p != NULL; p = p->next) {
    x1 = x2;
    y1 = y2;
    x2 = (double) (p->x + x);
    y2 = (double) (p->y + y);
    cx4 = (x1 + x2) / 2.0;
    cy4 = (y1 + y2) / 2.0;
    cx3 = (x1 + cx4) / 2.0;
    cy3 = (y1 + cy4) / 2.0;
    _xgdQuadraticSpline(cx1, cy1, cx2, cy2, cx3, cy3, cx4, cy4);
    cx1 = cx4;
    cy1 = cy4;
    cx2 = (cx1 + x2) / 2.0;
    cy2 = (cy1 + y2) / 2.0;
  }
  x1 = x2;
  y1 = y2;
  p = spline->points->next;
  x2 = (double) (p->x + x);
  y2 = (double) (p->y + y);
  cx4 = (x1 + x2) / 2.0;
  cy4 = (y1 + y2) / 2.0;
  cx3 = (x1 + cx4) / 2.0;
  cy3 = (y1 + cy4) / 2.0;
  _xgdQuadraticSpline(cx1, cy1, cx2, cy2, cx3, cy3, cx4, cy4);
  
  _xgdAddPoint((int) cx4, (int) cy4);
  
  if (fp != XGD_FILL_PATTERN_NONE)
    XFillPolygon(display, window, gc, __xgdpoints,
		 __xgdnpoints, Complex, CoordModeOrigin);

  if (lp != XGD_LINE_PATTERN_NONE)
    XDrawLines(display, window, gc, __xgdpoints,
	       __xgdnpoints, CoordModeOrigin);
  
}



/********************* CURVES FOR SPLINES *****************************
  
  The following spline drawing routine is from
  
  "An Algorithm for High-Speed Curve Generation"
  by George Merrill Chaikin,
  Computer Graphics and Image Processing, 3, Academic Press,
  1974, 346-349.
  
  and
  
  "On Chaikin's Algorithm" by R. F. Riesenfeld,
  Computer Graphics and Image Processing, 4, Academic Press,
  1975, 304-310.
  
  ***********************************************************************/

#define         half(z1, z2)    ((z1+z2)/2.0)
#define         THRESHOLD       5

/* iterative version */
/*
 * because we draw the spline with small line segments, the style parameter
 * doesn't work
 */

/*
 ***************************************************************************
 * _xgdQuadraticSpline - Generate a quadratic spline.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
_xgdQuadraticSpline(a1, b1, a2, b2, a3, b3, a4, b4)
     double           a1, b1, a2, b2, a3, b3, a4, b4;
#else
_xgdQuadraticSpline(double a1,
		 double b1,
		 double a2,
		 double b2,
		 double a3,
		 double b3,
		 double a4,
		 double b4)
#endif
{
  register double  xmid, ymid;
  double           x1, y1, x2, y2, x3, y3, x4, y4;
  
  _xgdClearStack();
  _xgdPush(a1, b1, a2, b2, a3, b3, a4, b4);
  
  while (_xgdPop(&x1, &y1, &x2, &y2, &x3, &y3, &x4, &y4)) {
    xmid = half(x2, x3);
    ymid = half(y2, y3);
    if (fabs(x1 - xmid) < THRESHOLD && fabs(y1 - ymid) < THRESHOLD &&
	fabs(xmid - x4) < THRESHOLD && fabs(ymid - y4) < THRESHOLD) {
      _xgdAddPoint(round(x1), round(y1));
      _xgdAddPoint(round(xmid), round(ymid));
    } else {
      _xgdPush(xmid, ymid, half(xmid, x3), half(ymid, y3),
	   half(x3, x4), half(y3, y4), x4, y4);
      _xgdPush(x1, y1, half(x1, x2), half(y1, y2),
	   half(x2, xmid), half(y2, ymid), xmid, ymid);
    }
  }
}



/*
 ***************************************************************************
 * _xgdBezierSpline - Generate a bezier spline
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
_xgdBezierSpline(a0, b0, a1, b1, a2, b2, a3, b3, op)
     double           a0, b0, a1, b1, a2, b2, a3, b3;
     int             op;
#else
     _xgdBezierSpline(double a0,
		   double b0,
		   double a1,
		   double b1,
		   double a2,
		   double b2,
		   double a3,
		   double b3,
		   int op)
#endif
{
  register double  tx, ty;
  double           x0, y0, x1, y1, x2, y2, x3, y3;
  double           sx1, sy1, sx2, sy2, tx1, ty1, tx2, ty2, xmid, ymid;
  
  _xgdClearStack();
  _xgdPush(a0, b0, a1, b1, a2, b2, a3, b3);
  
  while (_xgdPop(&x0, &y0, &x1, &y1, &x2, &y2, &x3, &y3)) {
    if (fabs(x0 - x3) < THRESHOLD && fabs(y0 - y3) < THRESHOLD) {
      _xgdAddPoint(round(x0), round(y0));
    } else {
      tx = half(x1, x2);
      ty = half(y1, y2);
      sx1 = half(x0, x1);
      sy1 = half(y0, y1);
      sx2 = half(sx1, tx);
      sy2 = half(sy1, ty);
      tx2 = half(x2, x3);
      ty2 = half(y2, y3);
      tx1 = half(tx2, tx);
      ty1 = half(ty2, ty);
      xmid = half(sx2, tx1);
      ymid = half(sy2, ty1);
      
      _xgdPush(xmid, ymid, tx1, ty1, tx2, ty2, x3, y3);
      _xgdPush(x0, y0, sx1, sy1, sx2, sy2, xmid, ymid);
    }
  }
}

/* utilities used by spline drawing routines */

#define         STACK_DEPTH     20

typedef struct stack {
  double           x1, y1, x2, y2, x3, y3, x4, y4;
}
Stack;

static Stack    stack[STACK_DEPTH];
static Stack   *stack_top;
static int      stack_count;

/*
 ***************************************************************************
 * _xgdClearStack - Clear the stack
 ***************************************************************************
 */
void
#ifdef _NO_PROTO  
_xgdClearStack()
#else
_xgdClearStack(void)
#endif     
{
  stack_top = stack;
  stack_count = 0;
}

/*
 ***************************************************************************
 * _xgdPush = Push points onto the stack
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
_xgdPush(x1, y1, x2, y2, x3, y3, x4, y4)
     double           x1, y1, x2, y2, x3, y3, x4, y4;
#else
     _xgdPush(double x1,
	  double y1,
	  double x2,
	  double y2,
	  double x3,
	  double y3,
	  double x4,
	  double y4)
#endif
{
  stack_top->x1 = x1;
  stack_top->y1 = y1;
  stack_top->x2 = x2;
  stack_top->y2 = y2;
  stack_top->x3 = x3;
  stack_top->y3 = y3;
  stack_top->x4 = x4;
  stack_top->y4 = y4;
  stack_top++;
  stack_count++;
}

/*
 ***************************************************************************
 * _xgdPop - pop points off the top of the stack
 ***************************************************************************
 */
int
#ifdef _NO_PROTO
_xgdPop(x1, y1, x2, y2, x3, y3, x4, y4)
     double *x1, *y1, *x2, *y2, *x3, *y3, *x4, *y4;
#else
_xgdPop(double *x1,
    double *y1,
    double *x2,
    double *y2,
    double *x3,
    double *y3,
    double *x4,
    double *y4)
#endif
{
  if (stack_count == 0)
    return (0);
  stack_top--;
  stack_count--;
  *x1 = stack_top->x1;
  *y1 = stack_top->y1;
  *x2 = stack_top->x2;
  *y2 = stack_top->y2;
  *x3 = stack_top->x3;
  *y3 = stack_top->y3;
  *x4 = stack_top->x4;
  *y4 = stack_top->y4;
  return (1);
}
