#include "xgrass_dlib.h"
#include "bbox.h"


#define round(x) ((x > 0.0)?((int)(x + 0.5)):((int)((x) - 0.5)))

/********************************/
/*  Private function prototypes */
/********************************/

static int _xgdIntersect(
#ifndef _NO_PROTO
    int x1,
    int y1,
    int x2,
    int y2,
    int axis_val,			 
    char axis
#endif
);

static void _xgdLocation(
#ifndef _NO_PROTO
    int x,
    int y,
    XgdBox box,
    short *top,
    short *bottom,
    short *left,
    short *right
#endif
);
			 
/*
 ***************************************************************************
 * XgdBoxesOverlap - check to see if box b1 overlaps with box b2
 ***************************************************************************
 */
Status
#ifdef _NO_PROTO
XgdBoxesOverlap(b1, b2)
     XgdBox b1, b2;
#else
XgdBoxesOverlap(XgdBox b1, XgdBox b2)
#endif
{
  XgdLine line1, line2, line3, line4; 

  if (XgdBoxContains(b1, b2))
    return(True);

  if (XgdBoxContains(b2, b1))
    return(True);

  line1.x1 = b1.l; line1.x2 = b1.l; line1.y1 = b1.t; line1.y2 = b1.b;
  line2.x1 = b1.r; line2.x2 = b1.r; line2.y1 = b1.t; line2.y2 = b1.b;
  line3.x1 = b1.l; line3.x2 = b1.r; line3.y1 = b1.t; line3.y2 = b1.t;
  line4.x1 = b1.l; line4.x2 = b1.r; line4.y1 = b1.b; line4.y2 = b1.b;

  if (XgdLineIntersectBox(b2, line1))
    return(True);
  else  if (XgdLineIntersectBox(b2, line2))
    return(True);
  else  if (XgdLineIntersectBox(b2, line3))
    return(True);
  else  if (XgdLineIntersectBox(b2, line4))
    return(True);
  else
    return(False);
}

/*
 ***************************************************************************
 * XgdBoxContains - check to see if box b2 contains box b1
 ***************************************************************************
 */
Status
#ifdef _NO_PROTO
XgdBoxContains(b1, b2)
     XgdBox b1, b2;
#else
XgdBoxContains(XgdBox b1, XgdBox b2)
#endif
{
  if ((b2.t<= b1.t) && (b2.b >= b1.b) && (b2.r >= b1.r) && (b2.l <= b1.l))
    return (True);

  return(False);
}

/*
 ***************************************************************************
 * XgdPointInBox - Check to see if the point (x,y) is in the box b
 ***************************************************************************
 */
Status
#ifdef _NO_PROTO
XgdPointInBox(b, x, y)
     XgdBox b;
     int     x;
     int     y;
#else
XgdPointInBox(XgdBox b, int x, int y)
#endif
{
  if ((x >= b.l) && (x <= b.r) && (y >= b.t) && (y <= b.b))
    return(True);

  return(False);
}

/*
 ***************************************************************************
 * XgdUpdateBoxForObject - Update a box for the given object
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdUpdateBoxForObject(obj, t, b, l, r)
XgdObject *obj;
int t, b, l, r;
#else
XgdUpdateBoxForObject( XgdObject *obj, int t, int b, int l, int r)
#endif
{
    obj->bbox->l = l;
    obj->bbox->r = r;
    obj->bbox->t = t;
    obj->bbox->b = b;
}

/*
 ***************************************************************************
 * XgdCreateBoxForObject - Create a box for the given object
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdCreateBoxForObject(obj)
XgdObject *obj;
#else
XgdCreateBoxForObject( XgdObject *obj)
#endif
{
    obj->bbox = 
        XgdSetBox(obj->y, obj->y + obj->height, obj->x, obj->x + obj->width);
}

/*
 ***************************************************************************
 * XgdSetBox - Create a box with the given top (t), bottom (b), etc., and
 *  return it.
 ***************************************************************************
 */
XgdBox *
#ifdef _NO_PROTO
XgdSetBox(t, b, l, r)
     int t;
     int b;
     int l;
     int r;
#else
XgdSetBox(int t, int b, int l, int r)
#endif
{
  XgdBox *box;

  box = (XgdBox *) malloc(sizeof(XgdBox));

  box->t = t;
  box->b = b;
  box->l = l;
  box->r = r;

  return(box);
}

/*
 ***************************************************************************
 * XgdLineIntersectBox - Check to see if line 'line' intersects with box 'box'
 ***************************************************************************
 */
Status
#ifdef _NO_PROTO
XgdLineIntersectBox(box, line)
     XgdBox box;
     XgdLine line;
#else
XgdLineIntersectBox(XgdBox box, XgdLine line)
#endif
{
  short t1, t2;
  short b1, b2;
  short l1, l2;
  short r1, r2;
  short accepted, rejected;
  short inside1, inside2;
  int x1, x2, y1, y2;
  int tmpx, tmpy;
  
/*
 * Check Foley and Van Damn, et al, "Computer Graphics - Principals and
 *  Practice" Second Edition, page 113-117 for the desciption of the
 *  Cohen-Sutherland line clipping algorithm.
 */
  
  x1 = line.x1;
  x2 = line.x2;
  y1 = line.y1;
  y2 = line.y2;
  
  _xgdLocation(x2, y2, box, &t2, &b2, &l2, &r2);

  inside2 = !(t2 || b2 || r2 || l2);
  accepted = 0;
  rejected = 0;
  while (!(accepted || rejected)){
    _xgdLocation(x1, y1, box, &t1, &b1, &l1, &r1);
    inside1 = !(t1 || b1 || r1 || l1);
    accepted = inside1 && inside2;
    if (!accepted){
      rejected = ((t1 && t2) || (b1 && b2) ||
		  (r1 && r2) || (l1 && l2));
      if (!rejected){
	if (inside1){
	  tmpx = x1;
	  x1 = x2;
	  x2 = tmpx;
	  tmpy = y1;
	  y1 = y2;
	  y2 = tmpy;
	  t1 = t2;
	  b1 = b2;
	  r1 = r2;
	  l1 = l2;
	  inside2 = 1;
	  t2 = b2 = r2 = l2 = 0;
	}
	
	if (t1){
	  x1 = _xgdIntersect(x1, y1, x2, y2, box.t, 'y');
	  y1 = box.t;
	}
	else if (b1){
	  x1 = _xgdIntersect(x1, y1, x2, y2, box.b, 'y');
	  y1 = box.b;
	}
	else if (l1){
	  y1 = _xgdIntersect(x1, y1, x2, y2, box.l, 'x');
	  x1 = box.l;
	}
	else if (r1){
	  y1 = _xgdIntersect(x1, y1, x2, y2, box.r, 'x');
	  x1 = box.r;
	}
      }
    }
  }
  if (accepted)
    return(True);
  else
    return(False);
}

/*
 ***************************************************************************
 * _xgdLocation - given the point (x,y) and the box 'box', compute where
 * the point lies in relation to the box (to the left, right, top, bottom)
 ***************************************************************************
 */
static void
#ifdef _NO_PROTO
_xgdLocation(x, y, box, top, bottom, left, right)
     int x;
     int y;
     XgdBox box;
     short *top, *bottom, *left, *right;
#else
_xgdLocation(int x, int y, XgdBox box, short *top, short *bottom,
	     short *left, short *right)
#endif
{
  *top = (y < box.t);
  *bottom = (y > box.b);
  *left = (x < box.l);
  *right = (x > box.r);
}

/*
 ***************************************************************************
 * _xgdIntersect - Compute the intersection of a line with the give axis.
 ***************************************************************************
 */
static int
#ifdef _NO_PROTO
_xgdIntersect(x1, y1, x2, y2, axis_val, axis)
     int x1, y1, x2, y2, axis_val;
     char axis;
#else
_xgdIntersect(int x1, int y1, int x2, int y2, int axis_val, char axis)
#endif
{
  double slope;
  double b;

  if ((y2 == y1) || (x1 == x2))
    if (axis == 'x')
      return(y1);
    else
      return(x1);

  slope = ((double)(y2 - y1))/((double)(x2 - x1));
  b = y1 - (slope * (double)x1);

  if (axis == 'x')
    return(round(((slope * (double) axis_val) + b)));
  else
    return(round((((double) axis_val - b)/slope)));
}

/*
 ***************************************************************************
 * XgdLineContainedInBox - Check to see if line l is contained in box b
 ***************************************************************************
 */
Status
#ifdef _NO_PROTO
XgdLineContainedInBox(b, l)
     XgdBox b;
     XgdLine l;
#else
XgdLineContainedInBox(XgdBox b, XgdLine l)
#endif
{
  short t1, t2, b1, b2, l1, l2, r1, r2;
  short inside1, inside2;
  _xgdLocation(l.x1, l.y1, b, &t1, &b1, &l1, &r1);
  _xgdLocation(l.x2, l.y2, b, &t2, &b2, &l2, &r2);

  inside1 = (!(t1 && b1 && l1 && r1));
  inside2 = (!(t2 && b2 && l2 && r2));

  if (inside1 && inside2)
    return(True);

  return(False);
}

/*
 ***************************************************************************
 * XgdIsPointInObject - Is the point in the object?  I just don't know.
 ***************************************************************************
 */
Boolean
#ifdef _NO_PROTO
XgdIsPointInObject(obj, x, y)
XgdObject *obj;
int x, y;
#else
XgdIsPointInObject(XgdObject *obj, int x, int y)
#endif
{
    return(XgdPointInBox(*(obj->bbox), x, y)); 
}

/*
 ***************************************************************************
 * XgdIsObjectInBox - Is the object in da box?  Please tell me.
 ***************************************************************************
 */
Boolean 
#ifdef _NO_PROTO
XgdIsObjectInBox(obj, box)
XgdObject *obj;
XgdBox *box;
#else
XgdIsObjectInBox( XgdObject *obj, XgdBox *box)
#endif
{
    return( XgdBoxContains(*(obj->bbox), *box));
}

/*
 ***************************************************************************
 * XgdGetBBoxOfObject - return the bbox of an object
 ***************************************************************************
 */
XgdBox *
#ifdef _NO_PROTO
XgdGetBBoxOfObject(obj)
XgdObject *obj;
#else
XgdGetBBoxOfObject(XgdObject *obj)
#endif
{
    return(obj->bbox);
}

