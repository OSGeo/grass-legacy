/* * Last edited: Sep 26 11:55 1991 (mallet) */
/*
 * $Id: Graphics.c,v 1.5 1991/09/27 08:41:10 mallet Exp $
 * 
 * Copyright 1991 Lionel Mallet
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appears in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Lionel MALLET not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  Lionel MALLET makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * Lionel MALLET DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 * SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS, IN NO EVENT SHALL Lionel MALLET BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION 
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  This software is opened and free. Furthermore, everybody is kindly
 * invited to participate to improve it for the benefit of all.
 * Improvements can be new features, bugs fixes and porting issues
 * resolution.
 *
 * Author:  Lionel Mallet, SIMULOG
 */

/*
 * $XConsortium: Graphics.c,v 1.1 90/06/09 20:20:29 dmatic Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Davor Matic, MIT X Consortium
 */



#include <X11/IntrinsicP.h>
#include <X11/Xmu/Converters.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include "PixmapP.h"

#include <stdio.h>
#include <math.h>

#define XtStrlen(s)                   ((s) ? strlen(s) : 0)
#define abs(x)                        (((x) > 0) ? (x) : -(x))
#define min(x, y)                     (((x) < (y)) ? (x) : (y))
#define max(x, y)                     (((x) > (y)) ? (x) : (y))

#ifdef IGRAPH
#define rint(x) ((double) ((int) ((x) + 0.5)))
#endif

/**************************************************************************
 *
 * Utility functions
 *
 **************************************************************************/

typedef enum { Lazy, NotLazy} DrawingMode;

#define WithinMark(PW, xi, yi) \
  ((xi <= max(PW->pixmap.mark.from_x, PW->pixmap.mark.to_x)) &&\
   (xi >= min(PW->pixmap.mark.from_x, PW->pixmap.mark.to_x)) &&\
   (yi <= max(PW->pixmap.mark.from_y, PW->pixmap.mark.to_y)) &&\
   (yi >= min(PW->pixmap.mark.from_y, PW->pixmap.mark.to_y)))

#define HotInMark(PW) WithinMark(PW, PW->pixmap.hot.x, PW->pixmap.hot.y)

#define IsHotSpot(PW, xi, yi) \
  ((xi== PW->pixmap.hot.x) && (yi == PW->pixmap.hot.y))

#define GetPxl(image, x, y)\
  XGetPixel(image, x, y)

#define SetPxl(image, x, y, color)\
  XPutPixel(image, x, y, color)

#define ClearPxl(image, x, y, white)\
  XPutPixel(image, x, y, white)


Pixel PWGetPxl(w, x, y)
     Widget  w;
     Position      x, y;
{
  PixmapWidget PW = (PixmapWidget) w;
  
  if (QueryInPixmap(PW, x, y))
    return GetPxl(PW->pixmap.image, x, y);
  else
    return NotSet;
}


XPoint *HotSpotShape(PW, x ,y)
    PixmapWidget PW;
    Position     x, y;
{
    static XPoint points[5];
  
    points[0].x = InWindowX(PW, x);
    points[0].y = InWindowY(PW, y + 1.0/2);
    points[1].x = InWindowX(PW, x + 1.0/2);
    points[1].y = InWindowY(PW, y + 1);
    points[2].x = InWindowX(PW, x + 1);
    points[2].y = InWindowY(PW, y + 1.0/2);
    points[3].x = InWindowX(PW, x + 1.0/2);
    points[3].y = InWindowY(PW, y);
    points[4].x = InWindowX(PW, x);
    points[4].y = InWindowY(PW, y + 1.0/2);
    
    return points;
}

#define DrawHotSpot(PW, x, y)\
  XFillPolygon(XtDisplay(PW), XtWindow(PW), PW->pixmap.framing_gc,\
	       HotSpotShape(PW, x, y), 5, Convex, CoordModeOrigin)

#define HighlightHotSpot(PW, x, y)\
  XFillPolygon(XtDisplay(PW), XtWindow(PW), PW->pixmap.highlighting_gc,\
	       HotSpotShape(PW, x, y), 5, Convex, CoordModeOrigin)


void HighlightSquare(PW, x, y)
     PixmapWidget PW;
     Position x, y;
{
  XFillRectangle(dpy, XtWindow(PW),
		 PW->pixmap.highlighting_gc,
		 InWindowX(PW, x), InWindowY(PW, y),
		 PW->pixmap.squareW, PW->pixmap.squareH);
}

void RedrawAxesInSquare();


void ClearSquare(PW, x, y)
     PixmapWidget PW;
     Position x, y;
{
  XFillRectangle(dpy, XtWindow(PW),
		 PW->pixmap.clear_gc,
		 InWindowX(PW, x), InWindowY(PW, y),
		 PW->pixmap.squareW, PW->pixmap.squareH);

}


void DrawSquare(PW, x, y, color)
     PixmapWidget PW;
     Position x, y;
     Pixel color;
{
  if (color == PW->pixmap.clear_pixel) ClearSquare(PW, x, y);
  else if (color == PW->pixmap.transparent_pixel)
    {
      XFillRectangle(dpy, XtWindow(PW),
    		 PW->pixmap.transparent_gc,
    		 InWindowX(PW, x), InWindowY(PW, y),
    		 PW->pixmap.squareW, PW->pixmap.squareH);
    }
  else
    {
      XSetForeground(dpy,PW->pixmap.drawing_gc,
    		 color);
      XFillRectangle(dpy, XtWindow(PW),
    		 PW->pixmap.drawing_gc,
    		 InWindowX(PW, x), InWindowY(PW, y),
    		 PW->pixmap.squareW, PW->pixmap.squareH);
      XSetForeground(dpy,PW->pixmap.drawing_gc,
    		 PW->pixmap.foreground_pixel);
    }
}

#define DrawPoint(PW, x, y, color, mode)\
  if ((mode == NotLazy) || (GetPxl(PW->pixmap.image, x, y) != color)) {\
    DrawSquare(PW, x, y, color); \
    SetPxl(PW->pixmap.image, x, y, color); \
    /* now put back the status of the pixel */ \
    PWRedrawGrid(PW, x, y, x, y); \
    RedrawAxesInSquare(PW, x, y); \
    if (WithinMark(PW, x, y)) HighlightSquare(PW, x, y); \
    if (IsHotSpot(PW, x, y)) PWRedrawHotSpot(PW);}

#define ClearPoint(PW, x, y, mode) \
  if ((mode == NotLazy) || \
      (GetPxl(PW->pixmap.image, x, y) != PW->pixmap.clear_pixel)) {\
    ClearSquare(PW, x, y);  \
    ClearPxl(PW->pixmap.image, x, y, PW->pixmap.clear_pixel); \
    /* now put back the status of the pixel */ \
    PWRedrawGrid(PW, x, y, x, y); \
    RedrawAxesInSquare(PW, x, y); \
    if (WithinMark(PW, x, y)) HighlightSquare(PW, x, y); \
    if (IsHotSpot(PW, x, y)) PWRedrawHotSpot(PW);}


#define QueryFlood(PW, x, y, value)\
  (QueryInPixmap(PW, x, y)\
     && (GetPxl(PW->pixmap.image, x, y) == value))

#define Flood(PW, x, y, value)\
  DrawPoint(PW, x, y, value, NotLazy)

void FloodLoop(PW, x, y, value, toflood)
     PixmapWidget PW;
     Position     x, y;
     Pixel          value, toflood;
{
  Position save_x, save_y, x_left, x_right;

  if (value == toflood) return; /* nothing to flood in this case */
  
  if (QueryFlood(PW, x, y, toflood)) 
    Flood(PW, x, y, value);
      
  save_x = x;
  save_y = y;
  
  x++;
  while (QueryFlood(PW, x, y, toflood)) {
    Flood(PW, x, y, value);
    x++;
  }
  x_right = --x;
  
  x = save_x;
  x--;
  while (QueryFlood(PW, x, y, toflood)) {
    Flood(PW, x, y, value);
    x--;
  }
  x_left = ++x;
  
  
  x = x_left;
  y = save_y;
  y++;
  
  while (x <= x_right) {
    Boolean flag = False;
    Position x_enter;
    
    while (QueryFlood(PW, x, y, toflood) && (x <= x_right)) {
      flag = True;
      x++;
    }
    
    if (flag) {
      if ((x == x_right) && QueryFlood(PW, x, y, toflood))
	FloodLoop(PW, x, y, value, toflood);
      else
	FloodLoop(PW, x - 1, y, value, toflood);
    }
    
    x_enter = x;
    
    while (!QueryFlood(PW, x, y, toflood) && (x < x_right))
      x++;
    
    if (x == x_enter) x++;
  }
  
  x = x_left;
  y = save_y;
  y--;
  
  while (x <= x_right) {
    Boolean flag = False;
    Position x_enter;
    
    while (QueryFlood(PW, x, y, toflood) && (x <= x_right)) {
      flag = True;
      x++;
    }
    
    if (flag) {
      if ((x == x_right) && QueryFlood(PW, x, y, toflood))
	FloodLoop(PW, x, y, value, toflood);
      else
	FloodLoop(PW, x - 1, y, value, toflood);
    }
    
    x_enter = x;
    
    while (!QueryFlood(PW, x, y, toflood) && (x < x_right))
      x++;
    
    if (x == x_enter) x++;
  }
}

void CopyImageData(source, destination, from_x, from_y, to_x, to_y, at_x, at_y)
     XImage *source, *destination;
     Position  from_x, from_y, to_x, to_y, at_x, at_y;
{
  Position x, y, delta_x, delta_y;
  
  delta_x = to_x - from_x + 1;
  delta_y = to_y - from_y + 1;
  
  for (x = 0; x < delta_x; x++)
    for (y = 0; y < delta_y; y++)
      SetPxl(destination, at_x + x, at_y + y, 
	     GetPxl(source, from_x + x, from_y + y));
}


void TransferImageData(source, destination)
     XImage *source, *destination;
{
  Position x, y;
  Pixel color;
  
  for (x = 0; x < min(source->width, destination->width); x++)
    for (y = 0; y < min(source->height, destination->height); y++)
      if ((color = GetPxl(source, x, y)) != GetPxl(destination, x, y))
	SetPxl(destination, x, y, color);
}

void DrawImageData(PW, image, at_x, at_y, value, mode)
     PixmapWidget PW;
     XImage *image;
     Position at_x, at_y;
     int value;
     DrawingMode mode;
{
  Position x, y;
  Pixel      A, B;
  register Pixel Cl, Fg, val;
  
  Cl = PW->pixmap.clear_pixel;
  Fg = PW->pixmap.foreground_pixel;
  
  for (x = 0; x < image->width; x++) 
    for (y = 0; y < image->height; y++) {
      if (!QueryInPixmap(PW, at_x + x, at_y + y)) break;
      A = GetPxl(image, x, y);
      B = GetPxl(PW->pixmap.image, at_x + x, at_y + y);
      switch (value)
	{
	case Clear:
	  if ((mode == NotLazy) || (B != Cl)) val = Cl;
	  else val = NotSet;
	  break;
	case Set:
	  if ((mode == NotLazy) || (B != A)) val = A;
	  else val = NotSet;
	  break;
	case Invert:
	  if (((mode == NotLazy) && (A != Cl)) || ((A != Cl) && (B != Cl)))
	    val = Cl;
	  else if (((mode == NotLazy) && (A == Cl)) || 
		   ((A == Cl) && (B != Fg))) val = Fg;
	  else val = NotSet;
	  break;
	case Highlight:
	  if (A != B) PWDrawPoint(PW, at_x + x, at_y + y, A);
	  break;
	}
      if ((val != NotSet) && (val != Highlight))
	{
	  DrawPoint(PW, at_x + x, at_y + y, val, mode);
	}
      else 
	if (value == Highlight) HighlightSquare(PW, x, y);
	
    }
}

#define PRECISION 0.00001
#define OR(p1, p2) (p1 ? p1 : (p2 ? p2 : NULL))

/* this function checks the intersection of two lines
 * returns intersection if it exists, first point if two lines are equal,
 * null otherwise. Space in which result in returned should be free by the
 * caller
 */
XPoint *intersect(P1, Q1, P2, Q2)
     XPoint P1, Q1, P2, Q2;
     
{
  XPoint *result;
  int vert1 = 0, vert2 = 0, horiz1 = 0, horiz2 = 0;
  double alpha1, beta1, alpha2, beta2;
  double x, y;

  if (P1.x != Q1.x) /* not vertical */
    {
      alpha1 = (Q1.y*1. - P1.y*1.) / (Q1.x*1. - P1.x*1.);
      beta1 = P1.y*1. - alpha1 * P1.x*1.;
      if (Q1.y == P1.y) horiz1 = 1;
    }
  else /* vertical */
    {
      vert1 = 1;
      /* check line1 not point */
      if (P1.y == Q1.y) return(NULL);
      alpha1 = 0.;
      beta1 = P1.x;
    }

  if (P2.x != Q2.x) /* not vertical */
    {
      if (P2.y == Q2.y) horiz2 = 1;
      if (!vert1)
	{
	  alpha2 = (Q2.y*1. - P2.y*1.) / (Q2.x*1. - P2.x*1.);
	  beta2 = P2.y*1. - alpha2 * P2.x*1.;
	}
      else if (!horiz2) /* and vert1 implicitly */
	{
	  alpha2 = (Q2.x*1. - P2.x*1.) / (Q2.y*1. - P2.y*1.);
	  beta2 = P2.x*1. - alpha2 * P2.y*1.;
	}
      else /* vert1 and horiz2 */
	{
	  alpha2 = 0.;
	  beta2 = P2.y;
	}
    }
  else /* vertical */
    {
      vert2 = 1;
      /* check line2 not point */
      if (P2.y == Q2.y) return(NULL);
      alpha2 = 0.;
      beta2 = P2.x;
      if ((!vert1) && (!horiz1))
	{
	  alpha1 = (Q1.x*1. - P1.x*1.) / (Q1.y*1. - P1.y*1.);
	  beta1 = P1.x*1. - alpha1 * P1.y*1.;
	}	  
    }
  
#ifdef DEBUG
  printf("D1 vert %d horiz %d, D2 vert %d horiz %d\n", vert1, horiz1, 
	 vert2, horiz2);
  printf("alpha1 %lf beta1 %lf, alpha2 %lf beta2 %lf\n", alpha1, beta1,
	 alpha2, beta2);
#endif DEBUG

  if ((vert1 && vert2) || (horiz1 && horiz2))
    {
      if (!fabs(beta1 - beta2) > PRECISION) return(NULL);
      x = P1.x;
      y = P1.y;
    }
  else if ((vert1 && horiz2) || (vert2 && horiz1))
    {
      if (vert1)
	  x = P1.x , y = P2.y;
      else 
	  x = P2.x , y = P1.y;
    }
  else if ((vert1 && !vert2 && !horiz2) || (vert2 && !vert1 && !horiz1))
    {
      if (vert1)
	  x = P1.x , y = (x - beta2) / alpha2;
      else x = P2.x , y = (x - beta1) / alpha1;
    }
  else if ((horiz1 && !vert2 && !horiz2) || (horiz2 && !vert1 && !horiz1))
    {
      if (horiz1)
	y = P1.y , x = (y - beta2) / alpha2;
      else y = P2.y , x = (y - beta1) / alpha1;
    }
  else if (!vert1 && !horiz1 && !vert2 && !horiz2)
    {
      if (fabs(alpha1 - alpha2) > PRECISION) /* alpha1 != alpha2 */
	{
	  x = (beta2 - beta1) / (alpha1 - alpha2);
	  y = alpha1 * x + beta1;
	}
      else if (fabs(beta1 - beta2) > PRECISION) /* alpha1 = alpha2
						   && beta1 != beta2 */
	return(NULL);
      else /* alpha1 = alpha2 && beta1 = beta2 */
	x = P1.x , y = P1.y;
    }

  /* now check if solution in between P1, Q1 and P2, Q2, else return NULL */
  if (((Position)rint(x) < min(P1.x, Q1.x)) || 
      ((Position)rint(x) > max(P1.x, Q1.x)) ||
      ((Position)rint(y) < min(P1.y, Q1.y)) ||
      ((Position)rint(y) > max(P1.y, Q1.y)) ||
      ((Position)rint(x) < min(P2.x, Q2.x)) || 
      ((Position)rint(x) > max(P2.x, Q2.x)) ||
      ((Position)rint(y) < min(P2.y, Q2.y)) ||
      ((Position)rint(y) > max(P2.y, Q2.y)))
    return(NULL);
  else
    {
      result = (XPoint *) malloc(sizeof(XPoint));
      result->x = (Position) rint(x);
      result->y = (Position) rint(y);
      return(result);
    }
}


/* the algorithm is : find A1, A2, B1, B2 intersections of the axe to draw
 * and the borders of the square (t, l, b, r) and then draw line between
 * (A1 or A2) and (B1 or B2) 
 */
void RedrawAxesInSquare(wi, x, y)
     Widget wi;
     Position x, y;
{
  PixmapWidget PW = (PixmapWidget) wi;
  int w = PW->pixmap.width, h =PW->pixmap.height;
  XPoint P1, Q1, P2, Q2, *A1, *A2, *B1, *B2, *from, *to;

  if (PWQueryAxes(PW))
    {
      /* first diagonal */
      P2.x = InWindowX(PW, 0); P2.y = InWindowY(PW, 0);
      Q2.x = InWindowX(PW, w); Q2.y = InWindowY(PW, h);
      
      /* A1: top intersect diagonal */
      P1.x = InWindowX(PW, x); P1.y = InWindowY(PW, y);
      Q1.x = InWindowX(PW, x+1); Q1.y = P1.y;
      A1 = intersect(P1, Q1, P2, Q2);
      
      /* A2: left intersect diagonal */
      P1.x = InWindowX(PW, x); P1.y = InWindowY(PW, y);
      Q1.x = P1.x; Q1.y = InWindowY(PW, y+1);
      A2 = intersect(P1, Q1, P2, Q2);
      
      /* B1: bottom intersect diagonal */
      P1.x = InWindowX(PW, x); P1.y = InWindowY(PW, y+1);
      Q1.x = InWindowX(PW, x+1); Q1.y = P1.y;
      B1 = intersect(P1, Q1, P2, Q2);
      
      /* B2: right intersect diagonal */
      P1.x = InWindowX(PW, x+1); P1.y = InWindowY(PW, y);
      Q1.x = P1.x; Q1.y = InWindowY(PW, y+1);
      B2 = intersect(P1, Q1, P2, Q2);
      
      /* line to draw from=OR(a1, a2) to=OR(b1, b2) */
      from = OR(A1, A2); to = OR(B1, B2);
      if (from && to) XDrawLine(dpy, XtWindow(PW), PW->pixmap.axes_gc,
				from->x, from->y, to->x, to->y);
      if (A1) free(A1); if (A2) free(A2);
      if (B1) free(B1); if (B2) free(B2);
      
      /* second diagonal */
      P2.x = InWindowX(PW, 0); P2.y = InWindowY(PW, h);
      Q2.x = InWindowX(PW, w); Q2.y = InWindowY(PW, 0);
      
      /* A1: top intersect diagonal */
      P1.x = InWindowX(PW, x); P1.y = InWindowY(PW, y);
      Q1.x = InWindowX(PW, x+1); Q1.y = P1.y;
      A1 = intersect(P1, Q1, P2, Q2);
      
      /* A2: right intersect diagonal */
      P1.x = InWindowX(PW, x+1); P1.y = InWindowY(PW, y);
      Q1.x = P1.x; Q1.y = InWindowY(PW, y+1);
      A2 = intersect(P1, Q1, P2, Q2);
      
      /* B1: bottom intersect diagonal */
      P1.x = InWindowX(PW, x); P1.y = InWindowY(PW, y+1);
      Q1.x = InWindowX(PW, x+1); Q1.y = P1.y;
      B1 = intersect(P1, Q1, P2, Q2);
      
      /* B2: left intersect diagonal */
      P1.x = InWindowX(PW, x); P1.y = InWindowY(PW, y);
      Q1.x = P1.x; Q1.y = InWindowY(PW, y+1);
      B2 = intersect(P1, Q1, P2, Q2);
      
      /* line to draw from=OR(a1, a2) to=OR(b1, b2) */
      from = OR(A1, A2); to = OR(B1, B2);
      if (from && to) XDrawLine(dpy, XtWindow(PW), PW->pixmap.axes_gc,
				from->x, from->y, to->x, to->y);
      if (A1) free(A1); if (A2) free(A2);
      if (B1) free(B1); if (B2) free(B2);

      /* horizontal */
      if ((h % 2) != 0) /* height is even */
	{
	  Position y0;
	  
	  y0 = InWindowY(PW, 0)
	    + rint((InWindowY(PW, h) - InWindowY(PW, 0)) / 2.);
	  if ((y0 > InWindowY(PW, y)) && (y0 < InWindowY(PW, y+1)))
	    XDrawLine(dpy, XtWindow(PW), PW->pixmap.axes_gc,
		      InWindowX(PW, x), y0, InWindowX(PW, x+1), y0);
	}
      
      /* vertical */
      if ((w % 2) != 0) /* width is even */
	{
	  Position x0;
	  
	  x0 = InWindowX(PW, 0)
	    + rint((InWindowX(PW, w) - InWindowX(PW, 0)) / 2.);
	  if ((x0 > InWindowX(PW, x)) && (x0 < InWindowX(PW, x+1)))
	    XDrawLine(dpy, XtWindow(PW), PW->pixmap.axes_gc,
		      x0, InWindowY(PW, y), x0, InWindowY(PW, y+1));
	}
    }
}

typedef struct {
  Position *x, *y;
  Dimension *width, *height;
} Table;

XImage *ScalePixmapImage(PW, src, scale_x, scale_y)
     PixmapWidget PW;
     XImage *src;
     double scale_x, scale_y;
{
  XImage *dst;
  Table table;    
  Position x, y, w, h;
  Dimension width, height;
  Pixel pixel;
  
  width = max(rint(scale_x * src->width), 1);
  height = max(rint(scale_y * src->height), 1);
  
  dst = CreatePixmapImage(PW, width, height);
  
  /*
   * It would be nice to check if width or height < 1.0 and
   * average the skipped pixels. But, it is slow as it is now.
   */
  if (scale_x == 1.0 && scale_y == 1.0)
    CopyImageData(src, dst, 0, 0, width-1 , height-1, 0, 0);
  else {
    table.x = (Position *) XtMalloc(sizeof(Position) * src->width);
    table.y = (Position *) XtMalloc(sizeof(Position) * src->height);
    table.width = (Dimension *) XtMalloc(sizeof(Dimension) * src->width);
    table.height = (Dimension *) XtMalloc(sizeof(Dimension) * src->height);
    
    for (x = 0; x < src->width; x++) {
      table.x[x] = rint(scale_x * x);
      table.width[x] = rint(scale_x * (x + 1)) - rint(scale_x * x);
    }
    for (y = 0; y < src->height; y++) {
      table.y[y] = rint(scale_y * y);
      table.height[y] = rint(scale_y * (y + 1)) - rint(scale_y * y);
    }
    
    for (x = 0; x < src->width; x++)
      for (y = 0; y < src->height; y++) {
	pixel = GetPxl(src, x, y);
	for (w = 0; w < table.width[x]; w++)
	  for (h = 0; h < table.height[y]; h++)
	    if (pixel != PW->pixmap.clear_pixel) SetPxl(dst, 
							table.x[x] + w, 
							table.y[y] + h,
							pixel);
      }
    
    XtFree(table.x);
    XtFree(table.y);
    XtFree(table.width);
    XtFree(table.height);
  }
  
  return (dst);
}


/*************************************************************************
 *
 * Interface functions
 *
 *************************************************************************/
/* high level procedures */

void PWRedrawHotSpot(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;

    if (QuerySet(PW->pixmap.hot.x, PW->pixmap.hot.y))
	DrawHotSpot(PW, PW->pixmap.hot.x, PW->pixmap.hot.y);
}

void PWClearHotSpot(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;
    
    DrawHotSpot(PW, PW->pixmap.hot.x, PW->pixmap.hot.y);
    PW->pixmap.hot.x = PW->pixmap.hot.y = NotSet;
}

void PWDrawHotSpot(w, x, y, value)
    Widget w;
    Position x, y;
{
    PixmapWidget PW = (PixmapWidget) w;
    
    if (QueryInPixmap(PW, x, y)) {
	if (QuerySet(PW->pixmap.hot.x, PW->pixmap.hot.y) &&
	    ((PW->pixmap.hot.x == x) && (PW->pixmap.hot.y == y))) {
	    if ((value == Clear) || (value == Invert)) {
		PWClearHotSpot(w);
	    }
	}
	else
	    if ((value == Set) || (value == Invert)) {
		PWClearHotSpot(w);
		DrawHotSpot(PW, x, y);
		PW->pixmap.hot.x = x;
		PW->pixmap.hot.y = y;
	    }
	
	if (value == Highlight)
	    HighlightHotSpot(PW, x, y); 
    }
}

void PWSetHotSpot(w, x, y)
    Widget w;
    Position x, y;
{
    if (QuerySet(x, y))
	PWDrawHotSpot(w, x, y, Set);
    else
	PWClearHotSpot(w);
}

void PWDrawPoint(w, x, y, value)
     Widget  w;
     Position      x, y;
     int           value;
{
  PixmapWidget PW = (PixmapWidget) w;
  
  if (QueryInPixmap(PW, x, y)) 
    {
      if (value == Highlight)
	HighlightSquare(PW, x, y);
      else if ((value == Clear) || 
	       ((value == Invert) && 
		(GetPxl(PW->pixmap.image, x, y) != PW->pixmap.clear_pixel)))
	{
	  ClearPoint(PW, x, y, Lazy);
	}
      else /* value == Set || (value == Invert && Pxl == clear_pixel) */
	{
	  DrawPoint(PW, x, y, PW->pixmap.foreground_pixel, Lazy);
	}
    }
}

void PWRedrawSquares(w, from_x, from_y, to_x, to_y)
     Widget  w;
     Position      from_x, from_y,
       to_x, to_y;
{
  PixmapWidget PW = (PixmapWidget) w;
  register int x, y;
  
  QuerySwap(from_x, to_x);
  QuerySwap(from_y, to_y);
  from_x = max(0, from_x);
  from_y = max(0, from_y);
  to_x = min(PW->pixmap.image->width - 1, to_x);
  to_y = min(PW->pixmap.image->height - 1, to_y);
  
  for (x = from_x; x <= to_x; x++)
    for (y = from_y; y <= to_y; y++)
      DrawSquare(PW, x, y, GetPxl(PW->pixmap.image, x, y));
}

void PWRedrawPoints(w, from_x, from_y, to_x, to_y)
     Widget  w;
     Position      from_x, from_y,
       to_x, to_y;
{
  PixmapWidget PW = (PixmapWidget) w;
  register int x, y;
  
  QuerySwap(from_x, to_x);
  QuerySwap(from_y, to_y);
  from_x = max(0, from_x);
  from_y = max(0, from_y);
  to_x = min(PW->pixmap.image->width - 1, to_x);
  to_y = min(PW->pixmap.image->height - 1, to_y);
  
  for (x = from_x; x <= to_x; x++)
    for (y = from_y; y <= to_y; y++)
      DrawPoint(PW, x, y, GetPxl(PW->pixmap.image, x, y), NotLazy);
}

void PWDrawGrid(w, from_x, from_y, to_x, to_y)
     Widget w;
     Position from_x, from_y,
       to_x, to_y;
{
  PixmapWidget PW = (PixmapWidget) w;
  int i;
  
  QuerySwap(from_x, to_x);
  QuerySwap(from_y, to_y);
  from_x = max(0, from_x);
  from_y = max(0, from_y);
  to_x = min(PW->pixmap.image->width - 1, to_x);
  to_y = min(PW->pixmap.image->height - 1, to_y);
  
  for(i = from_x + (from_x == 0); i <= to_x; i++)
    XDrawLine(dpy, XtWindow(PW), 
	      PW->pixmap.framing_gc,
	      InWindowX(PW, i), InWindowY(PW, from_y),
	      InWindowX(PW, i), InWindowY(PW, to_y + 1));
  
  for(i = from_y + (from_y == 0); i <= to_y; i++)
    XDrawLine(dpy, XtWindow(PW), 
	      PW->pixmap.framing_gc,
	      InWindowX(PW, from_x), InWindowY(PW, i),
	      InWindowX(PW, to_x + 1), InWindowY(PW, i));
}


void PWRedrawGrid(w, from_x, from_y, to_x, to_y)
     Widget w;
     Position from_x, from_y,
       to_x, to_y;
{
  PixmapWidget PW = (PixmapWidget) w;
  
  if (PW->pixmap.grid)
    PWDrawGrid(w, from_x, from_y, to_x, to_y);
}

void PWDrawLine(w, from_x, from_y, to_x, to_y, value)
     Widget  w;
     Position      from_x, from_y,
       to_x, to_y;
     int value;
{
  Position i;
  register double x, y;
  double dx, dy, delta;
  
  dx = to_x - from_x;
  dy = to_y - from_y;
  x = from_x + 0.5;
  y = from_y + 0.5;
  delta = max(abs(dx), abs(dy));
  if (delta > 0) {
    dx /= delta;
    dy /= delta;
    for(i = 0; i <= delta; i++) {
      PWDrawPoint(w, (Position) x, (Position) y, value);
      x += dx;
      y += dy;
    }
  }
  else
    PWDrawPoint(w, from_x, from_y, value);
}

void PWBlindLine(w, from_x, from_y, to_x, to_y, value)
     Widget  w;
     Position from_x, from_y, to_x, to_y;
     int value;
{
  Position i;
  register double x, y;
  double dx, dy, delta;
  
  dx = to_x - from_x;
  dy = to_y - from_y;
  x = from_x + 0.5;
  y = from_y + 0.5;
  delta = max(abs(dx), abs(dy));
  if (delta > 0) {
    dx /= delta;
    dy /= delta;
    x += dx;
    y += dy;
    for(i = 1; i <= delta; i++) {
      PWDrawPoint(w, (Position) x, (Position) y, value);
      x += dx;
      y += dy;
    }
  }
  else
    PWDrawPoint(w, from_x, from_y, value);
}

void PWDrawRectangle(w, from_x, from_y, to_x, to_y, value)
     Widget w;
     Position     from_x, from_y,
       to_x, to_y;
     int          value;
{
  register Position i;
  Dimension delta, width, height;
  
  QuerySwap(from_x, to_x);
  QuerySwap(from_y, to_y);
  
  width = to_x - from_x;
  height = to_y - from_y;
  
  delta = max(width, height);
  
  if (!QueryZero(width, height)) {
    for (i = 0; i < delta; i++) {
      if (i < width) {
	PWDrawPoint(w, from_x + i, from_y, value);
	PWDrawPoint(w, to_x - i, to_y, value);
      }
      if (i < height) {
	PWDrawPoint(w, from_x, to_y - i, value);
	PWDrawPoint(w, to_x, from_y + i, value);
      }
    }
  }
  else
    PWDrawLine(w, 
	       from_x, from_y, 
	       to_x, to_y, value);
}

void PWDrawFilledRectangle(w, from_x, from_y, to_x, to_y, value)
     Widget   w;
     Position from_x, from_y,
       to_x, to_y;
     int      value;
{
  register Position x, y;
  
  QuerySwap(from_x, to_x);
  QuerySwap(from_y, to_y);
  
  for (x = from_x; x <= to_x; x++)
    for (y = from_y; y <= to_y; y++)
      PWDrawPoint(w, x, y, value);
}

void PWDrawCircle(w, origin_x, origin_y, point_x, point_y, value)
     Widget  w;
     Position      origin_x, origin_y,
       point_x, point_y;
     int           value;
{
  register Position i, delta;
  Dimension dx, dy, half;
  double radius;
  
  dx = abs(point_x - origin_x);
  dy = abs(point_y - origin_y);
  radius = sqrt((double) (dx * dx + dy * dy));
  if (radius < 1.0) {
    PWDrawPoint(w, origin_x, origin_y, value);
  }
  else {
    PWDrawPoint(w, origin_x - (Position) floor(radius), origin_y, value);
    PWDrawPoint(w, origin_x + (Position) floor(radius), origin_y, value);
    PWDrawPoint(w, origin_x, origin_y - (Position) floor(radius), value);
    PWDrawPoint(w, origin_x, origin_y + (Position) floor(radius), value);
  }
  half = radius / sqrt(2.0);
  for(i = 1; i <= half; i++) {
    delta = sqrt(radius * radius - i * i);
    PWDrawPoint(w, origin_x - delta, origin_y - i, value);
    PWDrawPoint(w, origin_x - delta, origin_y + i, value);
    PWDrawPoint(w, origin_x + delta, origin_y - i, value);
    PWDrawPoint(w, origin_x + delta, origin_y + i, value);
    if (i != delta) {
      PWDrawPoint(w, origin_x - i, origin_y - delta, value);
      PWDrawPoint(w, origin_x - i, origin_y + delta, value);
      PWDrawPoint(w, origin_x + i, origin_y - delta, value);
      PWDrawPoint(w, origin_x + i, origin_y + delta, value);
    }
  }
}

void PWDrawFilledCircle(w, origin_x, origin_y, point_x, point_y, value)
     Widget  w;
     Position      origin_x, origin_y,
       point_x, point_y;
     int           value;
{
  register Position i, j, delta;
  Dimension dx, dy;
  double radius;
  
  dx = abs(point_x - origin_x);
  dy = abs(point_y - origin_y);
  radius = sqrt((double) (dx * dx + dy * dy));
  for(j = origin_x - (Position) floor(radius); 
      j <= origin_x + (Position) floor(radius); j++)
    PWDrawPoint(w, j, origin_y, value);
  for(i = 1; i <= (Position) floor(radius); i++) {
    delta = sqrt(radius * radius - i * i);
    for(j = origin_x - delta; j <= origin_x + delta; j++) {
      PWDrawPoint(w, j, origin_y - i, value);
      PWDrawPoint(w, j, origin_y + i, value);
    }
  }
}


void PWFloodFill(w, x, y, value)
     Widget  w;
     Position      x, y;
     int           value;
{
  PixmapWidget PW = (PixmapWidget) w;
  Pixel pixel, foreground = PW->pixmap.foreground_pixel;
  Pixel clear = PW->pixmap.clear_pixel;
  
  pixel = GetPxl(PW->pixmap.image, x, y);
  
  if (value == Invert)
    FloodLoop(PW, x, y, ((pixel != clear) ? clear : foreground), pixel);
  else if (value == Clear)
    FloodLoop(PW, x, y, clear, pixel);
  else
    FloodLoop(PW, x, y, foreground, pixel); 
}

void PWUp(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  register Position x, y;
  Pixel first, up, down;
  Position from_x, from_y, to_x, to_y;
  
  if (PWQueryMarked(w)) {
    from_x = PW->pixmap.mark.from_x;
    from_y = PW->pixmap.mark.from_y;
    to_x = PW->pixmap.mark.to_x;
    to_y = PW->pixmap.mark.to_y;
  }
  else {
    from_x = 0;
    from_y = 0;
    to_x = PW->pixmap.width - 1;
    to_y = PW->pixmap.height - 1;
  }
  
  if ((to_y - from_y) == 0)
    return;
  
  for(x = from_x; x <= to_x; x++) {
    first = up = GetPxl(PW->pixmap.image, x, to_y);
    for(y = to_y - 1; y >= from_y; y--) {
      down = GetPxl(PW->pixmap.image, x, y);
      if (up != down) 
	{
	  DrawPoint(PW, x, y, up, Lazy);
	}
      up = down;
    }
    if (first != down)
      {
	DrawPoint(PW, x, to_y, down, Lazy);
      }
  }
  
  if (QuerySet(PW->pixmap.hot.x, PW->pixmap.hot.y)
      &&
      !PWQueryMarked(w))
    PWSetHotSpot(w,
		 PW->pixmap.hot.x,
		 (PW->pixmap.hot.y - 1 + PW->pixmap.image->height) % 
		 PW->pixmap.image->height);
}

void PWDown(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  register Position x, y;
  Pixel first, down, up;
  Position from_x, from_y, to_x, to_y;
  
  if (PWQueryMarked(w)) {
    from_x = PW->pixmap.mark.from_x;
    from_y = PW->pixmap.mark.from_y;
    to_x = PW->pixmap.mark.to_x;
    to_y = PW->pixmap.mark.to_y;
  }
  else {
    from_x = 0;
    from_y = 0;
    to_x = PW->pixmap.width - 1;
    to_y = PW->pixmap.height - 1;
  }
  
  if ((to_y - from_y) == 0)
    return;
  
  for(x = from_x; x <= to_x; x++) {
    first = down = GetPxl(PW->pixmap.image, x, from_y);
    for(y = from_y + 1; y <= to_y; y++) {
      up = GetPxl(PW->pixmap.image, x, y);
      if (down != up)
	{
	  DrawPoint(PW, x, y, down, Lazy);
	}
      down = up;
    }
    if(first != up) 
      {
	DrawPoint(PW, x, from_y, up, Lazy);
      }
  }
  
  if (QuerySet(PW->pixmap.hot.x, PW->pixmap.hot.y)
      &&
      !PWQueryMarked(w))
    PWSetHotSpot(w,
		 PW->pixmap.hot.x,
		 (PW->pixmap.hot.y + 1) % PW->pixmap.image->height);
}

void PWLeft(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  register Position x, y;
  Pixel first, left, right;
  Position from_x, from_y, to_x, to_y;
  
  if (PWQueryMarked(w)) {
    from_x = PW->pixmap.mark.from_x;
    from_y = PW->pixmap.mark.from_y;
    to_x = PW->pixmap.mark.to_x;
    to_y = PW->pixmap.mark.to_y;
  }
  else {
    from_x = 0;
    from_y = 0;
    to_x = PW->pixmap.width - 1;
    to_y = PW->pixmap.height - 1;
  }
  
  if ((to_x - from_x) == 0)
    return;
  
  for(y = from_y; y <= to_y; y++) {
    first = left = GetPxl(PW->pixmap.image, to_x, y);
    for(x = to_x - 1; x >= from_x; x--) {
      right = GetPxl(PW->pixmap.image, x, y);
      if (left != right)
	{
	  DrawPoint(PW, x, y, left, Lazy);
	}
      left = right;
    }
    if(first != right)
      {
	DrawPoint(PW, to_x, y, right, Lazy);
      }
  }

  if (QuerySet(PW->pixmap.hot.x, PW->pixmap.hot.y)
      &&
      !PWQueryMarked(w))
    PWSetHotSpot(w,
		 (PW->pixmap.hot.x - 1 + PW->pixmap.image->width) % 
		 PW->pixmap.image->width,
		 PW->pixmap.hot.y);
}

void PWRight(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  register Position x, y;
  Pixel first, right, left;
  Position from_x, from_y, to_x, to_y;
  
  if (PWQueryMarked(w)) {
    from_x = PW->pixmap.mark.from_x;
    from_y = PW->pixmap.mark.from_y;
    to_x = PW->pixmap.mark.to_x;
    to_y = PW->pixmap.mark.to_y;
  }
  else {
    from_x = 0;
    from_y = 0;
    to_x = PW->pixmap.width - 1;
    to_y = PW->pixmap.height - 1;
  }
  
  if ((to_x - from_x) == 0)
    return;
  
  for(y = from_y; y <= to_y; y++) {
    first = right = GetPxl(PW->pixmap.image, from_x, y);
    for(x = from_x + 1; x <= to_x; x++) {
      left = GetPxl(PW->pixmap.image, x, y);
      if (right != left)
	{
	  DrawPoint(PW, x, y, right, Lazy);
	}
      right = left;
    }
    if(first != left)
      {
	DrawPoint(PW, from_x, y, left, Lazy);
      }
  }

  if (QuerySet(PW->pixmap.hot.x, PW->pixmap.hot.y)
      &&
      !PWQueryMarked(w))
    PWSetHotSpot(w,
		 (PW->pixmap.hot.x + 1) % PW->pixmap.image->width,
		 PW->pixmap.hot.y);
}

void PWRedrawMark(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  
  if (QuerySet(PW->pixmap.mark.from_x, PW->pixmap.mark.from_y)) 
    XFillRectangle(dpy, XtWindow(PW), PW->pixmap.highlighting_gc,
		   InWindowX(PW, PW->pixmap.mark.from_x), 
		   InWindowY(PW, PW->pixmap.mark.from_y), 
		   InWindowX(PW, PW->pixmap.mark.to_x + 1) - 
		   InWindowX(PW, PW->pixmap.mark.from_x),
		   InWindowY(PW, PW->pixmap.mark.to_y + 1) -
		   InWindowY(PW, PW->pixmap.mark.from_y));
}

void PWFold(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  Position x, y, new_x, new_y;
  Dimension horiz, vert;
  XImage *storage;
  Pixel color;
  
  storage = CreatePixmapImage(PW, (Dimension) PW->pixmap.image->width, 
			      (Dimension) PW->pixmap.image->height);
  
  TransferImageData(PW->pixmap.image, storage);
  
  PW->pixmap.fold ^= True;
  horiz = (PW->pixmap.image->width + PW->pixmap.fold) / 2;
  vert = (PW->pixmap.image->height + PW->pixmap.fold) / 2;
  
  for (x = 0; x < PW->pixmap.image->width; x++)
    for (y = 0; y < PW->pixmap.image->height; y++) {
      new_x = (x + horiz) % PW->pixmap.image->width;
      new_y = (y + vert) % PW->pixmap.image->height;
      if(GetPxl(PW->pixmap.image, new_x, new_y) != 
	 (color = GetPxl(storage, x, y)))
	{
	  DrawPoint(PW, new_x, new_y, color, Lazy);
	}
    }
  
  DestroyPixmapImage(&storage);
  if (QuerySet(PW->pixmap.hot.x, PW->pixmap.hot.y))
    PWSetHotSpot(w, 
		 (Position) 
		 ((PW->pixmap.hot.x + horiz) % PW->pixmap.image->width),
		 (Position)
		 ((PW->pixmap.hot.y + vert) % PW->pixmap.image->height));
}


void PWClear(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  register Position x, y;
  
  for (x = 0; x < PW->pixmap.image->width; x++)
    for (y = 0; y < PW->pixmap.image->height; y++)
      ClearPoint(PW, x, y, Lazy);
/* This way is too flashy
      ClearPxl(PW->pixmap.image, x, y, PW->pixmap.clear_pixel);
  XClearArea(dpy, XtWindow(PW),
	     0, 0, PW->core.width, PW->core.height, True);*/

}

void PWSet(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  register Position x, y;
  
  for (x = 0; x < PW->pixmap.image->width; x++)
    for (y = 0; y < PW->pixmap.image->height; y++)
      {
	DrawPoint(PW, x, y, PW->pixmap.foreground_pixel, Lazy);
      }

}

void PWRedraw(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  
  XClearArea(dpy, XtWindow(PW),
	     0, 0, PW->core.width, PW->core.height,
	     True);
}

void PWFlipHoriz(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  register Position x, y;
  Position from_x, from_y, to_x, to_y;
  float half;
  Pixel color1, color2;
  
  if (PWQueryMarked(w)) {
    from_x = PW->pixmap.mark.from_x;
    from_y = PW->pixmap.mark.from_y;
    to_x = PW->pixmap.mark.to_x;
    to_y = PW->pixmap.mark.to_y;
  }
  else {
    from_x = 0;
    from_y = 0;
    to_x = PW->pixmap.width - 1;
    to_y = PW->pixmap.height - 1;
  }
  half = (float) (to_y - from_y) / 2.0 + 0.5;
  
  if (half == 0.0)
    return;
  
  for (x = from_x; x <= to_x; x++) 
    for (y = 0; y <  half; y++)
      if ((color1 = GetPxl(PW->pixmap.image, x, from_y + y)) != 
	  (color2 = GetPxl(PW->pixmap.image, x, to_y - y))) {
	DrawPoint(PW, x, from_y + y, color2, Lazy);
	DrawPoint(PW, x, to_y - y, color1, Lazy);
      }

  if (QuerySet(PW->pixmap.hot.x, PW->pixmap.hot.y)
      &&
      !PWQueryMarked(w))
    PWSetHotSpot(w,
		 PW->pixmap.hot.x,
		 PW->pixmap.image->height - 1 - PW->pixmap.hot.y);
}

void PWFlipVert(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  register Position x, y;
  Position from_x, from_y, to_x, to_y;
  float half;
  Pixel color1, color2;
  
  if (PWQueryMarked(w)) {
    from_x = PW->pixmap.mark.from_x;
    from_y = PW->pixmap.mark.from_y;
    to_x = PW->pixmap.mark.to_x;
    to_y = PW->pixmap.mark.to_y;
  }
  else {
    from_x = 0;
    from_y = 0;
    to_x = PW->pixmap.width - 1;
    to_y = PW->pixmap.height - 1;
  }
  half = (float) (to_x - from_x) / 2.0 + 0.5;
  
  if (half == 0)
    return;
  
  for (y = from_y; y <= to_y; y++)
    for (x = 0; x < half; x++)
      if ((color1 = GetPxl(PW->pixmap.image, from_x + x, y)) != 
	  (color2 = GetPxl(PW->pixmap.image, to_x - x, y))) {
	DrawPoint(PW, from_x + x, y, color2, Lazy);
	DrawPoint(PW, to_x - x, y, color1, Lazy);
      }

  if (QuerySet(PW->pixmap.hot.x, PW->pixmap.hot.y)
      &&
      !PWQueryMarked(w))
    PWSetHotSpot(w,
		 PW->pixmap.image->width - 1 - PW->pixmap.hot.x,
		 PW->pixmap.hot.y);
}


void PWRotateRight(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  Position x, y, delta, shift, tmp;
  Position half_width, half_height;
  XPoint hot;
  Pixel quad1, quad2, quad3, quad4;
  Position from_x, from_y, to_x, to_y;
  
  if (PWQueryMarked(w)) {
    from_x = PW->pixmap.mark.from_x;
    from_y = PW->pixmap.mark.from_y;
    to_x = PW->pixmap.mark.to_x;
    to_y = PW->pixmap.mark.to_y;
  }
  else {
    from_x = 0;
    from_y = 0;
    to_x = PW->pixmap.width - 1;
    to_y = PW->pixmap.height - 1;
  }
  
  half_width = floor((to_x - from_x) / 2.0 + 0.5);
  half_height = floor((to_y - from_y ) / 2.0 + 0.5);
  shift = min((Position)(to_x - from_x), (Position)(to_y - from_y )) % 2;
  delta = min((Position) half_width, (Position) half_height) - shift;
  
  for (x = 0; x <= delta; x++) {
    for (y = 1 - shift; y <= delta; y++) {
      quad1 = GetPxl(PW->pixmap.image, 
		     from_x + (Position)half_width + x, 
		     from_y + (Position)half_height + y);
      quad2 = GetPxl(PW->pixmap.image, 
		     from_x + (Position)half_width + y, 
		     from_y + (Position)half_height - shift - x);
      quad3 = GetPxl(PW->pixmap.image, 
		     from_x + (Position)half_width - shift - x, 
		     from_y + (Position)half_height - shift - y);
      quad4 = GetPxl(PW->pixmap.image, 
		     from_x + (Position)half_width - shift - y, 
		     from_y + (Position)half_height + x);
      
      if (quad1 != quad2)
	{
	  DrawPoint(PW, from_x + (Position)half_width + x, 
		    from_y + (Position)half_height + y, quad2, Lazy);
	}
      if (quad2 != quad3)
	{
	  DrawPoint(PW, from_x + (Position)half_width + y, 
		    from_y + (Position)half_height - shift - x, quad3, Lazy);
	}
      if (quad3 != quad4)
	{
	  DrawPoint(PW, from_x + (Position)half_width - shift - x,
		    from_y + (Position)half_height - shift - y, quad4, Lazy);
	}
      if (quad4 != quad1)
	{
	  DrawPoint(PW, from_x + (Position)half_width - shift - y, 
		    from_y + (Position)half_height + x, quad1, Lazy);
	}
    }
  }
  
  if (QuerySet(PW->pixmap.hot.x, PW->pixmap.hot.y)
      &&
      !PWQueryMarked(w)) {
    hot.x = PW->pixmap.hot.x - half_width;
    hot.y = PW->pixmap.hot.y - half_height;
    if (hot.x >= 0) hot.x += shift;
    if (hot.y >= 0) hot.y += shift;
    tmp = hot.x;
    hot.x = - hot.y;
    hot.y = tmp;
    if (hot.x > 0) hot.x -= shift;
    if (hot.y > 0) hot.y -= shift;
    hot.x += half_width;
    hot.y += half_height;
    if (QueryInPixmap(PW, hot.x, hot.y))
      PWSetHotSpot(w, hot.x, hot.y);
  }
}

void PWRotateLeft(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  Position x, y,delta, shift, tmp;
  Position half_width, half_height;
  XPoint hot;
  Pixel quad1, quad2, quad3, quad4;
  Position from_x, from_y, to_x, to_y;
  
  if (PWQueryMarked(w)) {
    from_x = PW->pixmap.mark.from_x;
    from_y = PW->pixmap.mark.from_y;
    to_x = PW->pixmap.mark.to_x;
    to_y = PW->pixmap.mark.to_y;
  }
  else {
    from_x = 0;
    from_y = 0;
    to_x = PW->pixmap.width - 1;
    to_y = PW->pixmap.height - 1;
  }
  
  half_width = floor((to_x - from_x) / 2.0 + 0.5);
  half_height = floor((to_y - from_y ) / 2.0 + 0.5);
  shift = min((Position)(to_x - from_x), (Position)(to_y - from_y )) % 2;
  delta = min((Position) half_width, (Position) half_height) - shift;
  
  for (x = 0; x <= delta; x++) {
    for (y = 1 - shift; y <= delta; y++) {
      quad1 = GetPxl(PW->pixmap.image, 
		     from_x + (Position)half_width + x, 
		     from_y + (Position)half_height + y);
      quad2 = GetPxl(PW->pixmap.image, 
		     from_x + (Position)half_width + y, 
		     from_y + (Position)half_height - shift - x);
      quad3 = GetPxl(PW->pixmap.image, 
		     from_x + (Position)half_width - shift - x, 
		     from_y + (Position)half_height - shift - y);
      quad4 = GetPxl(PW->pixmap.image, 
		     from_x + (Position)half_width - shift - y, 
		     from_y + (Position)half_height + x);
      
      if (quad1 != quad4)
	{
	  DrawPoint(PW, from_x + (Position)half_width + x, 
		    from_y + (Position)half_height + y, quad4, Lazy);
	}
      if (quad2 != quad1)
	{
	  DrawPoint(PW, from_x + (Position)half_width + y, 
		    from_y + (Position)half_height - shift - x, quad1, Lazy);
	}
      if (quad3 != quad2)
	{
	  DrawPoint(PW, from_x + (Position)half_width - shift - x,
		    from_y + (Position)half_height - shift - y, quad2, Lazy);
	}
      if (quad4 != quad3)
	{
	  DrawPoint(PW, from_x + (Position)half_width - shift - y, 
		    from_y + (Position)half_height + x, quad3, Lazy);
	}
    }
  }

  if (QuerySet(PW->pixmap.hot.x, PW->pixmap.hot.y)
      &&
      !PWQueryMarked(w)) {
    hot.x = PW->pixmap.hot.x - half_width;
    hot.y = PW->pixmap.hot.y - half_height;
    if (hot.x >= 0) hot.x += shift;
    if (hot.y >= 0) hot.y += shift;
    tmp = hot.x;
    hot.x = hot.y;
    hot.y = - tmp;
    if (hot.x > 0) hot.x -= shift;
    if (hot.y > 0) hot.y -= shift;
    hot.x += half_width;
    hot.y += half_height;
    if (QueryInPixmap(PW, hot.x, hot.y))
      PWSetHotSpot(w, hot.x, hot.y);
  }
}


void PWStore(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  Dimension width, height;
  
  if (QuerySet(PW->pixmap.mark.from_x, PW->pixmap.mark.from_y)) {
    
    DestroyPixmapImage(&PW->pixmap.storage);
    
    width = PW->pixmap.mark.to_x - PW->pixmap.mark.from_x + 1;
    height = PW->pixmap.mark.to_y - PW->pixmap.mark.from_y + 1;
    
    PW->pixmap.storage = CreatePixmapImage(PW, width, height);
    
    CopyImageData(PW->pixmap.image, PW->pixmap.storage,
		  PW->pixmap.mark.from_x,  PW->pixmap.mark.from_y,
		  PW->pixmap.mark.to_x,  PW->pixmap.mark.to_y,
		  0, 0);
  }
}

void PWClearMarked(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  
  if (QuerySet(PW->pixmap.mark.from_x, PW->pixmap.mark.from_y))
    {
      Position from_x = PW->pixmap.mark.from_x, 
      from_y = PW->pixmap.mark.from_y, to_x = PW->pixmap.mark.to_x, 
      to_y = PW->pixmap.mark.to_y;
      Position x, y;
      QuerySwap(from_x, to_x);
      QuerySwap(from_y, to_y);
      
      for (x = from_x; x <= to_x; x++)
	for (y = from_y; y <= to_y; y++)
	  {
	    ClearPoint(PW, x, y, Lazy);
	  }
    }
}


void PWDragMarked(w, at_x, at_y)
     Widget w;
     Position     at_x, at_y;
{
  PixmapWidget PW = (PixmapWidget) w;
  
  if (QuerySet(PW->pixmap.mark.from_x, PW->pixmap.mark.from_y))
    PWDrawRectangle(w, 
		    at_x, at_y, 
		    at_x + PW->pixmap.mark.to_x - PW->pixmap.mark.from_x,
		    at_y + PW->pixmap.mark.to_y - PW->pixmap.mark.from_y,
		    Highlight);
}

void PWDragStored(w, at_x, at_y)
     Widget w;
     Position     at_x, at_y;
{
  PixmapWidget PW = (PixmapWidget) w;
  
  if (PW->pixmap.storage)
    PWDrawRectangle(w, 
		    at_x, at_y,
		    at_x + PW->pixmap.storage->width - 1,
		    at_y + PW->pixmap.storage->height - 1,
		    Highlight);
}

void PWRestore(w, at_x, at_y, value)
     Widget w;
     Position     at_x, at_y;
     int          value;
{
  PixmapWidget PW = (PixmapWidget) w;
  
  if (PW->pixmap.storage)
    DrawImageData(PW, PW->pixmap.storage, at_x, at_y, value, Lazy);
}

void PWCopy(w, at_x, at_y, value)
     Widget w;
     Position     at_x, at_y;
     int          value;
{
  PixmapWidget PW = (PixmapWidget) w;
  XImage *storage;
  Dimension width, height;
  
  if (QuerySet(PW->pixmap.mark.from_x, PW->pixmap.mark.from_y)) {
    
    width = PW->pixmap.mark.to_x - PW->pixmap.mark.from_x + 1;
    height = PW->pixmap.mark.to_y - PW->pixmap.mark.from_y + 1;
    
    storage = CreatePixmapImage(PW, width, height);
    
    CopyImageData(PW->pixmap.image, storage,
		  PW->pixmap.mark.from_x,  PW->pixmap.mark.from_y,
		  PW->pixmap.mark.to_x,  PW->pixmap.mark.to_y,
		  0, 0);
    
    DrawImageData(PW, storage, at_x, at_y, value, NotLazy);
    
    PWMark(w, at_x, at_y,
	   at_x + PW->pixmap.mark.to_x - PW->pixmap.mark.from_x,
	   at_y + PW->pixmap.mark.to_y - PW->pixmap.mark.from_y); 

    DestroyPixmapImage(&storage);
  }
}

void PWMove(w, at_x, at_y, value)
     Widget   w;
     Position at_x, at_y;
     int      value;
{
  PixmapWidget PW = (PixmapWidget) w;
  XImage *storage;
  Dimension width, height;
  
  if (QuerySet(PW->pixmap.mark.from_x, PW->pixmap.mark.from_y)) {
    
    width = PW->pixmap.mark.to_x - PW->pixmap.mark.from_x + 1;
    height = PW->pixmap.mark.to_y - PW->pixmap.mark.from_y + 1;
    
    storage = CreatePixmapImage(PW, width, height);
    
    CopyImageData(PW->pixmap.image, storage,
		  PW->pixmap.mark.from_x,  PW->pixmap.mark.from_y,
		  PW->pixmap.mark.to_x,  PW->pixmap.mark.to_y,
		  0, 0);
    
    PWClearMarked(w);

    DrawImageData(PW, storage, at_x, at_y, value, NotLazy); 
    
    PWMark(w, at_x, at_y,
	   at_x + PW->pixmap.mark.to_x - PW->pixmap.mark.from_x,
	   at_y + PW->pixmap.mark.to_y - PW->pixmap.mark.from_y);

    DestroyPixmapImage(&storage);
  }
}

void PWStoreToBuffer(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  
  /* first check available space and resize if necessary */
  if ((PW->pixmap.image->width != PW->pixmap.buffer->width) ||
      (PW->pixmap.image->height != PW->pixmap.buffer->height))
    {
      XImage *buffer = CreatePixmapImage(PW, PW->pixmap.image->width, 
					 PW->pixmap.image->height);
      
      DestroyPixmapImage(&PW->pixmap.buffer);
      PW->pixmap.buffer = buffer;
    }
  
  CopyImageData(PW->pixmap.image, PW->pixmap.buffer, 0, 0, 
		PW->pixmap.image->width-1, PW->pixmap.image->height-1, 0, 0);
  
  PW->pixmap.buffer_hot = PW->pixmap.hot;
  PW->pixmap.buffer_mark = PW->pixmap.mark;
}

void PWUnmark(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  
  PW->pixmap.buffer_mark = PW->pixmap.mark;
  
  if (QuerySet(PW->pixmap.mark.from_x, PW->pixmap.mark.from_y)) {
    XFillRectangle(dpy, XtWindow(PW), PW->pixmap.highlighting_gc,
		   InWindowX(PW, PW->pixmap.mark.from_x), 
		   InWindowY(PW, PW->pixmap.mark.from_y), 
		   InWindowX(PW, PW->pixmap.mark.to_x + 1) - 
		   InWindowX(PW, PW->pixmap.mark.from_x),
		   InWindowY(PW, PW->pixmap.mark.to_y + 1) -
		   InWindowY(PW, PW->pixmap.mark.from_y));
    
    PW->pixmap.mark.from_x = PW->pixmap.mark.from_y = NotSet;
    PW->pixmap.mark.to_x = PW->pixmap.mark.to_y = NotSet;
  }
}

void PWMark(w, from_x, from_y, to_x, to_y)
     Widget w;
     Position from_x, from_y,
       to_x, to_y;
{
  PixmapWidget PW = (PixmapWidget) w;
  
  PWUnmark(w);
  
  if (QuerySet(from_x, from_y)) {
    if ((from_x == to_x) && (from_y == to_y)) {
      /*
	PW->pixmap.mark.from_x = 0;
	PW->pixmap.mark.from_y = 0;
	PW->pixmap.mark.to_x = PW->pixmap.image->width - 1;
	PW->pixmap.mark.to_y = PW->pixmap.image->height - 1;
	*/
      return;
    }
    else {
      QuerySwap(from_x, to_x);
      QuerySwap(from_y, to_y);
      from_x = max(0, from_x);
      from_y = max(0, from_y);
      to_x = min(PW->pixmap.image->width - 1, to_x);
      to_y = min(PW->pixmap.image->height - 1, to_y);
      
      PW->pixmap.mark.from_x = from_x;
      PW->pixmap.mark.from_y = from_y;
      PW->pixmap.mark.to_x = to_x;
      PW->pixmap.mark.to_y = to_y;
    }
    
    XFillRectangle(dpy, XtWindow(PW), PW->pixmap.highlighting_gc,
		   InWindowX(PW, PW->pixmap.mark.from_x),
		   InWindowY(PW, PW->pixmap.mark.from_y), 
		   InWindowX(PW, PW->pixmap.mark.to_x + 1) -
		   InWindowX(PW, PW->pixmap.mark.from_x),
		   InWindowY(PW, PW->pixmap.mark.to_y +1) - 
		   InWindowY(PW, PW->pixmap.mark.from_y));
  }
}

void PWUndo(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  Position x, y;
  XPoint tmp_hot;
  XImage *tmp_image;
  PWArea tmp_mark;
  Pixel color;
  
  tmp_image = PW->pixmap.image;
  PW->pixmap.image = PW->pixmap.buffer;
  PW->pixmap.buffer = tmp_image;

  tmp_hot = PW->pixmap.hot;
  tmp_mark = PW->pixmap.mark;
  PW->pixmap.mark = PW->pixmap.buffer_mark;
  PW->pixmap.buffer_mark= tmp_mark;


  if ((PW->pixmap.image->width != PW->pixmap.buffer->width) ||
      (PW->pixmap.image->height != PW->pixmap.buffer->height))
    {
      PW->pixmap.width = PW->pixmap.image->width;
      PW->pixmap.height = PW->pixmap.image->height;
      Resize(PW);
      PWRedraw(w);
    }
  else
    {
      for (x = 0; x < PW->pixmap.image->width; x++)
	for (y = 0; y < PW->pixmap.image->height; y++)
	  if (GetPxl(PW->pixmap.buffer, x, y) != 
	      (color = GetPxl(PW->pixmap.image, x, y)))
	    {
	      DrawPoint(PW, x, y, color, NotLazy);
	    }
  
      /* Now treating Undo copy seperatly :
	 when mark is set and is different from buffer one, redraw them */
      if ((QuerySet(PW->pixmap.mark.from_x, PW->pixmap.mark.from_y)) &&
	  (QuerySet(PW->pixmap.buffer_mark.from_x, 
		    PW->pixmap.buffer_mark.from_y))
	  && ((PW->pixmap.mark.from_x != PW->pixmap.buffer_mark.from_x) ||
	      (PW->pixmap.mark.from_y != PW->pixmap.buffer_mark.from_y) ||
	      (PW->pixmap.mark.to_x != PW->pixmap.buffer_mark.to_x) ||
	      (PW->pixmap.mark.to_y != PW->pixmap.buffer_mark.to_y)))
	{
	  PWRedrawPoints((Widget) PW, PW->pixmap.mark.from_x, 
			 PW->pixmap.mark.from_y, 
			 PW->pixmap.mark.to_x, 
			 PW->pixmap.mark.to_y);
	  PWRedrawPoints((Widget) PW, PW->pixmap.buffer_mark.from_x, 
			 PW->pixmap.buffer_mark.from_y, 
			 PW->pixmap.buffer_mark.to_x, 
			 PW->pixmap.buffer_mark.to_y);
	}
    }

  PWSetHotSpot(w, PW->pixmap.buffer_hot.x, PW->pixmap.buffer_hot.y);
  PW->pixmap.buffer_hot = tmp_hot;
}


void PWHighlightAxes(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  
  XDrawLine(dpy, XtWindow(PW),
	    PW->pixmap.axes_gc,
	    InWindowX(PW, 0), 
	    InWindowY(PW, 0),
	    InWindowX(PW, PW->pixmap.width),
	    InWindowY(PW, PW->pixmap.height));
  XDrawLine(dpy, XtWindow(PW),
	    PW->pixmap.axes_gc,
	    InWindowX(PW, PW->pixmap.width),
	    InWindowY(PW, 0), 
	    InWindowX(PW, 0),
	    InWindowY(PW, PW->pixmap.height));
  if (PWQueryGrid(PW))
    {
      if ((((int)(PW->pixmap.height / 2.0)) * 2) != PW->pixmap.height)
	XDrawLine(dpy, XtWindow(PW),
		  PW->pixmap.axes_gc,
		  InWindowX(PW, 0),
		  InWindowY(PW, PW->pixmap.height / 2.0),
		  InWindowX(PW, PW->pixmap.width),
		  InWindowY(PW, PW->pixmap.height / 2.0));
      if ((((int)(PW->pixmap.width / 2.0)) * 2) != PW->pixmap.width)
	XDrawLine(dpy, XtWindow(PW),
		  PW->pixmap.axes_gc,
		  InWindowX(PW, PW->pixmap.width / 2.0),
		  InWindowY(PW, 0),
		  InWindowX(PW, PW->pixmap.width / 2.0),
		  InWindowY(PW, PW->pixmap.height));
    }
}

void PWSetForeground(w,color)
     
     Widget w;
     Pixel color;
     
{
  PixmapWidget PW = (PixmapWidget) w;
  
  XSetForeground(dpy,PW->pixmap.drawing_gc, color);
  PW->pixmap.foreground_pixel = color;
}

