/*
 * FIG : Facility for Interactive Generation of figures Copyright (c) 1985 by
 * Supoj Sutanthavibul
 * 
 * "Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty."
 * 
 */

#include <math.h>
#include "xgrass_dlib.h"
#include "ptlines.h"

extern short   __xgdptxmin, __xgdptxmax ,__xgdptymin, __xgdptymax;

/*
 ***************************************************************************
 * _xgdCreateSplineObject - initialize the spline data structures and call
 *       the appropriate (Interpolated or approximated) drawing routine
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
_xgdCreateSplineobject(obj, mode)
     XgdObject  *obj;
     int         mode;
#else
_xgdCreateSplineobject(XgdObject *obj, int mode)
#endif
{
  XgdSpline *spline;
  XgdPointList *tmp;
  
  if ((spline = (XgdSpline *) XtMalloc(sizeof(XgdSpline))) == NULL) {
    return;
  }

  if (obj->Obj.Spline.points == NULL)
    return;

  if (obj->Obj.Spline.points->next == NULL)
    return;

  spline->points = obj->Obj.Spline.points;
  spline->controls = NULL;

  if (mode == APPROX)
    _xgdDrawSpline(obj->display, obj->window, obj->objgc, obj->fp,
		   obj->lp, spline, obj->x, obj->y, obj->type);
  else {
    _xgdCreateControlPoints(spline);
    _xgdDrawIntSpline(obj->display, obj->window, obj->objgc, obj->fp,
		      obj->lp, spline, obj->x, obj->y, obj->type);
  }

  /* FIgure out the actual bounding box */
  if (obj->width != abs(__xgdptxmax - __xgdptxmin) ||
      obj->height != abs(__xgdptymax - __xgdptymin)){
    tmp = obj->Obj.Spline.points;
    while (tmp){
      tmp->x -= abs(obj->x - __xgdptxmin);
      tmp->y -= abs(obj->y - __xgdptymin);
      tmp->box->t = tmp->y - 3;
      tmp->box->b = tmp->y + 3;
      tmp->box->l = tmp->x - 3;
      tmp->box->r = tmp->x + 3;
      tmp = tmp->next;
    }
    obj->x = __xgdptxmin;
    obj->y = __xgdptymin;
    obj->width = __xgdptxmax - __xgdptxmin;
    obj->height = __xgdptymax - __xgdptymin;
  }
	
  if ( obj->bbox )
    XgdUpdateBoxForObject(obj, __xgdptymin, __xgdptymax,
			  __xgdptxmin, __xgdptxmax);
  else
    XgdCreateBoxForObject(obj);

  XgdConfigureResizeHandles(obj);
}


/*
 ***************************************************************************
 * _xgdCreateControlList - go through and create space for the control points.
 ***************************************************************************
 */
int
#ifdef _NO_PROTO
_xgdCreateControlList(s)
     XgdSpline *s;
#else
_xgdCreateControlList(XgdSpline *s)
#endif
{
  XgdPointList   *p;
  XgdControlList   *cp;
  
  if ((s->controls=(XgdControlList *)XtMalloc(sizeof(XgdControlList))) == NULL)
    return (-1);
  
  cp = s->controls;
  for (p = s->points; p != NULL; p = p->next) {
    if ((cp->next =(XgdControlList *)XtMalloc(sizeof(XgdControlList))) == NULL)
      return (-1);
    cp = cp->next;
  }
  cp->next = NULL;
  return (1);
}

/*
 ***************************************************************************
 * _xgdCreateControlPoints - create the actual control points through a maze
 *      of function calls :-)
 ***************************************************************************
 */
int 
#ifdef _NO_PROTO
_xgdCreateControlPoints(s)
     XgdSpline *s;
#else
_xgdCreateControlPoints(XgdSpline *s)
#endif
{
  
  if (-1 == _xgdCreateControlList(s))
    return;

  _xgdComputeCP(s->points, s->controls, 1);
}

#define         T               0.45
#define         _2xPI           6.2832
#define         _1dSQR2         0.7071
#define         _SQR2           1.4142

/*
 ***************************************************************************
 * _xgdComputeCP - Aha ... here's where the actual control points are created
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
_xgdComputeCP(points, controls, path)
     XgdPointList     *points;
     XgdControlList   *controls;
     int              path;
#else
_xgdComputeCP(XgdPointList *points, XgdControlList *controls, int path)
#endif
{
  XgdControlList  *cp, *cpn;
  XgdPointList    *p, *p2, *pk;    /* Pk is the next-to-last point. */
  double          dx, dy;
  double          x1, y1, x2, y2, x3, y3;
  double          l1, l2, theta1, theta2;
  
  x1 = points->x;
  y1 = points->y;
  pk = p2 = points->next;
  x2 = p2->x;
  y2 = p2->y;
  p = p2->next;
  x3 = p->x;
  y3 = p->y;
  
  dx = x1 - x2;
  dy = y2 - y1;
  l1 = sqrt((double) (dx * dx + dy * dy));
  if (l1 == 0.0)
    theta1 = 0.0;
  else
    theta1 = atan2((double) dy, (double) dx);
  dx = x3 - x2;
  dy = y2 - y3;
  l2 = sqrt((double) (dx * dx + dy * dy));
  if (l2 == 0.0)
    theta2 = 0.0;
  else
    theta2 = atan2((double) dy, (double) dx);
  /* -PI <= theta1, theta2 <= PI */
  if (theta1 < 0)
    theta1 += _2xPI;
  if (theta2 < 0)
    theta2 += _2xPI;
  /* 0 <= theta1, theta2 < 2PI */
  
  cp = controls->next;
  _xgdControlPoints(x2, y2, l1, l2, theta1, theta2, T, cp);
  /* control points for (x2, y2) */
  if (path == OPEN_PATH) {
    controls->lx = 0.0;
    controls->ly = 0.0;
    controls->rx = (x1 + 3 * cp->lx) / 4;
    controls->ry = (y1 + 3 * cp->ly) / 4;
    cp->lx = (3 * cp->lx + x2) / 4;
    cp->ly = (3 * cp->ly + y2) / 4;
  }
  while (1) {
    x2 = x3;
    y2 = y3;
    l1 = l2;
    if (theta2 >= M_PI)
      theta1 = theta2 - M_PI;
    else
      theta1 = theta2 + M_PI;
    if ((p = p->next) == NULL)
      break;
    pk = pk->next;
    x3 = p->x;
    y3 = p->y;
    dx = x3 - x2;
    dy = y2 - y3;
    l2 = sqrt((double) (dx * dx + dy * dy));
    if (l2 == 0.0)
      theta2 = 0.0;
    else
      theta2 = atan2((double) dy, (double) dx);
    if (theta2 < 0)
      theta2 += _2xPI;
    cp = cp->next;
    _xgdControlPoints(x2, y2, l1, l2, theta1, theta2, T, cp);
  };
  
  if (path == CLOSED_PATH) {
    dx = p2->x - x2;
    dy = y2 - p2->y;
    l2 = sqrt((double) (dx * dx + dy * dy));
    theta2 = atan2((double) dy, (double) dx);
    if (theta2 < 0)
      theta2 += _2xPI;
    cp = cp->next;
    _xgdControlPoints(x2, y2, l1, l2, theta1, theta2, T, cp);
    controls->lx = cp->lx;
    controls->ly = cp->ly;
    controls->rx = cp->rx;
    controls->ry = cp->ry;
  } else {
    cpn = cp->next;
    cpn->lx = (3 * cp->rx + x2) / 4;
    cpn->ly = (3 * cp->ry + y2) / 4;
    cpn->rx = 0.0;
    cpn->ry = 0.0;
    cp->rx = (pk->x + 3 * cp->rx) / 4;
    cp->ry = (pk->y + 3 * cp->ry) / 4;
  }
  
  
}

/*
 * The parameter t is the tension.  It must range in [0, 1]. The bigger the
 * value of t, the higher the tension.
 */

void
#ifdef _NO_PROTO
_xgdControlPoints(x, y, l1, l2, theta1, theta2, t, cp)
     double           x, y, l1, l2, theta1, theta2, t;
     XgdControlList *cp;
#else
_xgdControlPoints(double x, double y, double l1, double l2,
		  double theta1, double theta2, double t, XgdControlList *cp)
#endif
{
  double           s, theta, r = 1 - t;
  
  /* 0 <= theta1, theta2 < 2PI */
  
  theta = (theta1 + theta2) / 2;
  
  if (theta1 > theta2) {
    s = sin((double) (theta - theta2));
    theta1 = theta + M_PI_2;
    theta2 = theta - M_PI_2;
  } else {
    s = sin((double) (theta2 - theta));
    theta1 = theta - M_PI_2;
    theta2 = theta + M_PI_2;
  }
  if (s > _1dSQR2)
    s = _SQR2 - s;
  s *= r;
  l1 *= s;
  l2 *= s;
  cp->lx = x + l1 * cos((double) theta1);
  cp->ly = y - l1 * sin((double) theta1);
  cp->rx = x + l2 * cos((double) theta2);
  cp->ry = y - l2 * sin((double) theta2);
}

