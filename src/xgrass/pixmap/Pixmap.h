/* * Last edited: Sep 26 10:40 1991 (mallet) */
/*
 * $Id: Pixmap.h,v 1.5 1991/09/27 17:34:06 mallet Exp $
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
 * $XConsortium: Pixmap.h,v 1.4 90/04/25 08:30:49 dmatic Exp $
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


#ifndef _Pixmap_h
#define _Pixmap_h

/****************************************************************
 *
 * Pixmap widget
 *
 ****************************************************************/

#include <X11/Core.h>
#include <X11/Xmu/Converters.h>
#include <X11/Xos.h>
#include <X11/Xproto.h>
#include "xpm.h"

#if ! defined(XlibSpecificationRelease)
#ifndef _XPOINTER_DEF
typedef char *XPointer;
#define _XPOINTER_DEF
#endif
#endif


/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		------------- 
 cursor              Cursor             Cursor          XC_tcross
 foreground          Foreground         Pixel           XtDefaultForeground
 highlight           Highlight          Pixel           XtDefaultForeground
 framing             Framing            Pixel           XtDefaultForeground
 transparent         Transparent        Pixel           XtDefaultTransparent
 proportional        Proportional       Boolean         True
 grid                Grid               Boolean         True
 gridTolerance       GridTolerance      Dimension       5
 stippled            Stippled           Boolean         True
 axes                Axes               Boolean         True
 resize              Resize             Boolean         True
 distance            Distance           Dimension       10
 squareSize          SquareSize         Dimension       20
 pixmapWidth         PixmapWidth        Dimension       16
 pixmapHeight        PixmapHeight       Dimension       16
 button1Action       Button1Action      Action          Set  
 button2Action       Button2Action      Action          Set
 button3Action       Button3Action      Action          Clear
 button4Action       Button4Action      Action          Clear
 button5Action       Button5Action      Action          Clear
 filename            Filename           String          "scratch"
 addColorNtfyProc   AddColorNtfyProc  AddColorNotifyProc NULL
 stipple             Stipple            Bitmap          XtUnspecifiedPixmap

Inherited from core:
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	0
 height		     Height		Dimension	0
 x		     Position		Position	320
 y		     Position		Position	320
*/

/* define any special resource names here that are not in <X11/StringDefs.h> */

#define XtNcursor "cursor"
#define XtNframing "framing"
#define XtNtransparent "transparent"
#define XtNproportional "proportional"
#define XtNgrid "grid"
#define XtNgridTolerance "gridTolerance"
#define XtNstippled "stippled"
#define XtNaxes "axes"
#define XtNdistance "distance"
#define XtNsquareSize "squareSize"
#define XtNpixmapWidth "pixmapWidth"
#define XtNpixmapHeight "pixmapHeight"
#define XtNbutton1Action "button1Action"
#define XtNbutton2Action "button2Action"
#define XtNbutton3Action "button3Action"
#define XtNbutton4Action "button4Action"
#define XtNbutton5Action "button5Action"
#define XtNfilename "filename"
#define XtNaddColorNtfyProc "addColorNtfyProc"
#define XtNstipple "stipple"

#define XtNscratch "scratch"
#define XtNdummy "dummy"

#define XtCHighlight "Highlight"
#define XtCFraming "Framing"
#define XtCTransparent "Transparent"
#define XtCProportional "Proportional"
#define XtCGrid "Grid"
#define XtCGridTolerance "GridTolerance"
#define XtCStippled "Stippled"
#define XtCAxes "Axes"
#define XtCDistance "Distance"
#define XtCSquareSize "SquareSize"
#define XtCPixmapWidth "PixmapWidth"
#define XtCPixmapHeight "PixmapHeight"
#define XtCButton1Action "Button1Action"
#define XtCButton2Action "Button2Action"
#define XtCButton3Action "Button3Action"
#define XtCButton4Action "Button4Action"
#define XtCButton5Action "Button5Action"
#define XtCFilename "Filename"
#define XtCAddColorNtfyProc "AddColorNtfyProc"
#define XtCStipple "Stipple"

#define XtDefaultTransparent "gray90"

/* pixmap defines */

#define NotSet    -1
#define Clear     0
#define Set       1
#define Invert    2
#define Highlight 3
#define On        True
#define Off       False

#define MarkRequest "MarkRequest"
#define StoreRequest "StoreRequest"
#define RestoreRequest "RestoreRequest"
#define CopyRequest "CopyRequest"
#define MoveRequest "MoveRequest"
#define PointRequest "PointRequest"
#define LineRequest "LineRequest"
#define CurveRequest "CurveRequest"
#define RectangleRequest "RectangleRequest"
#define FilledRectangleRequest "FilledRectangleRequest"
#define CircleRequest "CircleRequest"
#define FilledCircleRequest "FilledCircleRequest"
#define FloodFillRequest "FloodFillRequest"
#define HotSpotRequest "HotSpotRequest"
#define ZoomInRequest "ZoomInRequest"
#define PasteRequest "PasteRequest"
#define ImmediateCopyRequest "ImmediateCopyRequest"
#define ImmediateMoveRequest "ImmediateMoveRequest"

#define NoColorName "None"
#define UNDEF_PIXEL 0x80

typedef char *PWRequest;

/* declare specific PixmapWidget class and instance datatypes */
typedef struct _PixmapClassRec *PixmapWidgetClass;
typedef struct _PixmapRec      *PixmapWidget;
/* declare the class constant */
extern WidgetClass pixmapWidgetClass;
/* pixmap exports */
typedef struct _PWColorInfo
{
  char *symbol;  /* symbol used to write pixmap to file */
  char *s_name;  /* symbolic name of color */
  char *m_name;  /* monochrome display color */
  char *g4_name; /* g4 scale display color */
  char *g_name;  /* g6 scale display color */
  char *c_name;  /* color display name */
  Pixel pixel;   /* pixel of color in colormap */
} PWColorInfo;

typedef void (*AddColorNotifyProc)(
#if NeedFunctionPrototypes
  Widget /* w */,
  Pixel /* pix */,
  char* /* name */
#endif 
);

typedef void (*UseColorNotifyProc)(
#if NeedFunctionPrototypes
  Widget /* w */,
  Pixel  /* current */
#endif 
);

  
extern Boolean PWEngageRequest(
#if NeedFunctionPrototypes
    Widget      /* w */,
    PWRequest   /* name */,
    Boolean     /* trap */,
    XPointer    /* call_data */,
    Cardinal    /* call_data_size */
#endif
);
extern Boolean PWTerminateRequest(
#if NeedFunctionPrototypes
    Widget      /* w */,
    Boolean     /* cont */
#endif
);
extern void PWRemoveAllRequests(
#if NeedFunctionPrototypes
    Widget      /* w */
#endif
);

extern void PWClear(
#if NeedFunctionPrototypes
    Widget      /* w */
#endif
);
extern void PWSet(
#if NeedFunctionPrototypes
    Widget      /* w */
#endif
);
extern void PWUp(
#if NeedFunctionPrototypes
    Widget      /* w */
#endif
);
extern void PWDown(
#if NeedFunctionPrototypes
    Widget      /* w */
#endif
);
extern void PWLeft(
#if NeedFunctionPrototypes
    Widget      /* w */
#endif
);
extern void PWRight(
#if NeedFunctionPrototypes
    Widget      /* w */
#endif
);
extern void PWRotateRight(
#if NeedFunctionPrototypes
    Widget      /* w */
#endif
);
extern void PWRotateLeft(
#if NeedFunctionPrototypes
    Widget      /* w */
#endif
);
extern void PWSwitchGrid(
#if NeedFunctionPrototypes
    Widget      /* w */
#endif
);
extern void PWGrid(
#if NeedFunctionPrototypes
    Widget      /* w */,
    Boolean     /* _switch */
#endif
);
extern void PWSwitchAxes(
#if NeedFunctionPrototypes
    Widget      /* w */
#endif
);
extern void PWAxes(
#if NeedFunctionPrototypes
    Widget      /* w */,
    Boolean     /* _switch */
#endif
);
extern void PWDrawPoint(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* x */,
     Position   /* y */,
     int        /* value */
#endif
);
extern void PWDrawLine(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* from_x */,
     Position   /* from_y */,
     Position   /* to_x */,
     Position   /* to_y */,
     int        /* value */
#endif
);
extern void PWDrawRectangle(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* from_x */,
     Position   /* from_y */,
     Position   /* to_x */,
     Position   /* to_y */,
     int        /* value */
#endif
);
extern void PWDrawFilledRectangle(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* from_x */,
     Position   /* from_y */,
     Position   /* to_x */,
     Position   /* to_y */,
     int        /* value */
#endif
);
extern void PWDrawCircle(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* origin_x */,
     Position   /* origin_y */,
     Position   /* point_x */,
     Position   /* point_y */,
     int        /* value */
#endif
);
extern void PWDrawFilledCircle(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* origin_x */,
     Position   /* origin_y */,
     Position   /* point_x */,
     Position   /* point_y */,
     int        /* value */
#endif
);
extern void PWFloodFill(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* x */,
     Position   /* y */,
     int        /* value */
#endif
);
extern void PWMark(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* from_x */,
     Position   /* from_y */,
     Position   /* to_x */,
     Position   /* to_y */
#endif
);
extern void PWSelect(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* from_x */,
     Position   /* from_y */,
     Position   /* to_x */,
     Position   /* to_y */,
     Time       /* time */
#endif
);
extern void PWUnmark(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWStore(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWStoreToBuffer(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWUndo(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWResize(
#if NeedFunctionPrototypes
    Widget      /* w */,
    Dimension   /* width */,
    Dimension   /* height */
#endif
);
extern void PWRescale(
#if NeedFunctionPrototypes
    Widget      /* w */,
    Dimension   /* width */,
    Dimension   /* height */
#endif
);
extern void PWClip(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* from_x */,
     Position   /* from_y */,
     Position   /* to_x */,
     Position   /* to_y */
#endif
);
extern void PWUnclip(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWGrabSelection(
#if NeedFunctionPrototypes
    Widget      /* w */,
    Time        /* time */
#endif
);
extern void PWRequestSelection(
#if NeedFunctionPrototypes
    Widget      /* w */,
    Time        /* time */,
    Boolean     /* wait */
#endif
);
extern void PWSetChanged(
#if NeedFunctionPrototypes
    Widget      /* w */
#endif
);
extern Boolean PWQueryChanged(
#if NeedFunctionPrototypes
    Widget      /* w */
#endif
);
extern int  PWReadFile(
#if NeedFunctionPrototypes
    Widget      /* w */,
    String      /* filename */
#endif
);
extern int  PWWriteFile(
#if NeedFunctionPrototypes
    Widget      /* w */,
    String      /* filename */
#endif
);
extern String PWUnparseStatus(
#if NeedFunctionPrototypes
    Widget      /* w */
#endif
);
extern String PWGetFilename(
#if NeedFunctionPrototypes
    Widget      /* w */,
    String*     /* str */
#endif
);
extern void PWGetUnzoomedPixmap(
#if NeedFunctionPrototypes
    Widget      /* w */,
    Pixmap*     /* pix */,
    Pixmap*     /* pix_mask */
#endif
);
extern void PWAddColorNotifyProc(
#if NeedFunctionPrototypes
     Widget             /* w */,
     AddColorNotifyProc /* proc */
#endif
);
extern void PWUseColorInTable(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Pixel      /* pixel */,
     String     /* symbol */,
     String     /* sname */,
     String     /* mname */,
     String     /* g4name */,
     String     /* gname */, 
     String     /* cname */
#endif
);
extern void PWUpdateColorInTable(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Pixel      /* pixel */,
     String     /* symbol */,
     String     /* sname */,
     String     /* mname */,
     String     /* g4name */,
     String     /* gname */, 
     String     /* cname */
#endif
);
extern PWColorInfo **PWGetColorTable(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWComments(
#if NeedFunctionPrototypes
     Widget     /* w */,
     String*    /* hints_cmt */, 
     String*    /* colors_cmt */, 
     String*    /* pixels_cmt */
#endif
);
extern void Notify(
#if NeedFunctionPrototypes
     Widget     /* w */,
     void (*)() /* proc */
#endif
);
extern void ColorNotify(
#if NeedFunctionPrototypes
     Widget     /* w */,
     UseColorNotifyProc /* proc */
#endif
);
extern void PWBlindLine(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* from_x */, 
     Position   /* from_y */,
     Position   /* to_x */,
     Position   /* to_y */, 
     int        /* value */
#endif
);
extern void PWChangeFilename(
#if NeedFunctionPrototypes
     Widget     /* w */,
     String     /* str */
#endif
);
extern void PWChangeNotify(
#if NeedFunctionPrototypes
     Widget     /* w */,
     XPointer   /* client_data */,
     XPointer   /* call_data */
#endif
);
extern void PWClearChanged(
#if NeedFunctionPrototypes
    Widget     /* w */
#endif
);
extern void PWClearHotSpot(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWClearMarked(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWCopy(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* at_x */,
     Position   /* at_y */, 
     int        /* value */
#endif
);
extern void PWDebug(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWDragMarked(
#if NeedFunctionPrototypes
    Widget     /* w */,
     Position   /* at_x */,
     Position   /* at_y */
#endif
);
extern void PWDragStored(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* at_x */,
     Position   /* at_y */
#endif
);
extern void PWDrawGrid(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* from_x */, 
     Position   /* from_y */, 
     Position   /* to_x */, 
     Position   /* to_y */
#endif
);
extern void PWDrawHotSpot(
#if NeedFunctionPrototypes
     Widget     /* w */, 
     Position   /* x */,
     Position   /* y */, 
     int        /* value */
#endif
);
extern Boolean PWAddRequest(
#if NeedFunctionPrototypes
     Widget     /* w */, 
     PWRequest  /* name */, 
     Boolean    /* trap */,
     XPointer   /* call_data */,
     Cardinal   /*call_data_size */
#endif
);
extern Boolean PWRemoveRequest(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern Boolean PWQueryGrid(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern Boolean PWQueryStippled(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern Boolean PWQueryAxes(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern Boolean PWQueryStored(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern Boolean PWQueryMarked(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern Boolean PWQueryZooming(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern Boolean PWQueryProportional(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWFlipHoriz(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWFlipVert(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWFold(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWHighlightAxes(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWMove(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* at_x */,
     Position   /* at_y */,
     int        /* value */
#endif
);
extern void PWProportional(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Boolean    /* _switch */
#endif
);
extern void PWPutImage(
#if NeedFunctionPrototypes
     PixmapWidget     /* w */,
     Display*   /* display */,
     Drawable   /* drawable */, 
     GC         /* gc */, 
     Position   /* x */,
     Position   /* y */
#endif
);
extern void PWRedraw(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWRedrawAxes(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWRedrawGrid(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* from_x */,
     Position   /* from_y */,
     Position   /* to_x */,
     Position   /* to_y */
#endif
);
extern void PWRedrawHotSpot(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWRedrawMark(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWRedrawPoints(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* from_x */,
     Position   /* from_y */,
     Position   /* to_x */,
     Position   /* to_y */
#endif
);
extern void PWRedrawSquares(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* from_x */,
     Position   /* from_y */,
     Position   /* to_x */,
     Position   /* to_y */
#endif
);
extern void PWRestore(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* at_x */,
     Position   /* at_y */,
     int        /* value */
#endif
);
extern void PWSetForeground(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Pixel      /* color */
#endif
);
extern void PWSetHotSpot(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* x */,
     Position   /* y */
#endif
);
extern void PWSwitchProportional(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWTerminate(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWZoomIn(
#if NeedFunctionPrototypes
     Widget     /* w */,
     Position   /* from_x */,
     Position   /* from_y */,
     Position   /* to_x */,
     Position   /* to_y */
#endif
);
extern void PWZoomMarked(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);
extern void PWZoomOut(
#if NeedFunctionPrototypes
     Widget     /* w */
#endif
);

static char *rgb_fname = "/usr/lib/X11/rgb.txt";

#endif /* _Pixmap_h */
