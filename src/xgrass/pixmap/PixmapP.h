/* * Last edited: Sep 26 11:54 1991 (mallet) */
/*
 * $Id: PixmapP.h,v 1.5 1991/09/27 17:36:01 mallet Exp $
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
 * $XConsortium: PixmapP.h,v 1.6 90/06/09 20:19:47 dmatic Exp $
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



#ifndef _PixmapP_h
#define _PixmapP_h

#include "Pixmap.h"
#include <X11/CoreP.h>

#if ! defined(XlibSpecificationRelease)
#ifndef _XPOINTER_DEF
typedef char *XPointer;
#define _XPOINTER_DEF
#endif
#endif

/**********/
typedef struct {
  char       *name;
  int         status_size;
  void      (*engage)();
  XPointer     engage_client_data;
  void      (*terminate)();
  XPointer     terminate_client_data;
  void      (*remove)();
  XPointer     remove_client_data;
} PWRequestRec;

typedef struct {
    Atom           *targets;
    Cardinal        num_targets;
    PWRequestRec   *requests;
    Cardinal        num_requests;
    PWRequestRec   *request[100];
  
} PixmapClassPart;

/* Full class record declaration */
typedef struct _PixmapClassRec {
  CoreClassPart          core_class;
  PixmapClassPart        pixmap_class;
} PixmapClassRec;

extern PixmapClassRec pixmapClassRec;

typedef struct {
  Position from_x, from_y,
           to_x, to_y;
} PWArea;

typedef struct {
    PWRequestRec *request;
    XPointer       status;
    Boolean       trap;
    XPointer       call_data;
} PWRequestStack;

typedef struct {
    XImage   *image, *buffer;
    XPoint    hot;
    Position  at_x, at_y;
    Boolean   fold;
    Boolean   grid;
    Boolean   changed;
} PWZoom;

typedef struct {
    Boolean   own;
    Boolean   limbo;
} PWSelection;

/* New fields for the Pixmap widget record */
typedef struct {
  /* resources */
  Cursor           cursor;
  Pixel            foreground_pixel;
  Pixel            highlight_pixel;
  Pixel            framing_pixel;
  Pixel            transparent_pixel;
  Pixmap           stipple;
  Boolean          stippled;
  Boolean          proportional;
  Boolean          grid;
  Dimension        grid_tolerance;
  Boolean          axes;
  Boolean          resize;
  Dimension        distance, squareW, squareH, width, height;
  int              button_action[5];
  String           filename;
  AddColorNotifyProc AddColorNotify;
  /* private state */
  XPoint           hot;
  XPoint           buffer_hot;
  Pixel            clear_pixel;
  Position         horizOffset, vertOffset;
  void           (*notify)();
  UseColorNotifyProc colorNotify;
  PWRequestStack  *request_stack;
  Cardinal         cardinal, current;
  /*Boolean          trapping;*/
  XImage          *image, *buffer, *storage;
  PWArea           mark, buffer_mark;
  GC               drawing_gc;
  GC               highlighting_gc;
  GC               framing_gc;
  GC               axes_gc;
  GC               clear_gc;
  GC               transparent_gc;
  Boolean          changed;
  Boolean          fold;
  Boolean          zooming;
  PWZoom           zoom;
  XPointer         *value;
  char             status[80];
  PWSelection      selection;
  PWColorInfo    **colorTable;
  char            *hints_cmt;
  char            *colors_cmt;
  char            *pixels_cmt;
} PixmapPart;

/* Full instance record declaration */
typedef struct _PixmapRec {
  CorePart      core;
  PixmapPart    pixmap;
} PixmapRec;

/* Private functions */

#define InPixmapX(PW, x)\
	(Position) (min((Position)((max(PW->pixmap.horizOffset, x)  -\
				   PW->pixmap.horizOffset) /\
				   PW->pixmap.squareW), PW->pixmap.width - 1))
    
#define InPixmapY(PW, y)\
	(Position) (min((Position)((max(PW->pixmap.vertOffset, y)  -\
				   PW->pixmap.vertOffset) /\
				   PW->pixmap.squareH), PW->pixmap.height - 1))
    
#define InWindowX(PW, x)\
	(Position) (PW->pixmap.horizOffset + ((x) * PW->pixmap.squareW))

#define InWindowY(PW, y)\
	(Position) (PW->pixmap.vertOffset + ((y) * PW->pixmap.squareH))
     
#define GetPixmap(PW, image)\
    XCreateBitmapFromData(XtDisplay(PW), XtWindow(PW),\
			  image->data, image->width, image->height)


#define QuerySet(x, y) (((x) != NotSet) && ((y) != NotSet))

#define QueryZero(x, y) (((x) == 0) || ((y) == 0))

#define Swap(x, y) {Position t; t = x; x = y; y = t;}

#define QuerySwap(x, y) if(x > y) Swap(x, y)

#define QueryInPixmap(PW, x, y)\
  (((x) >= 0) && ((x) < PW->pixmap.image->width) &&\
   ((y) >= 0) && ((y) < PW->pixmap.image->height))

#define Value(PW, button)   (PW->pixmap.button_action[button - 1])

XImage *CreatePixmapImage();
void DestroyPixmapImage();

#endif /* _PixmapP_h */
