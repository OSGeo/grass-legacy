/* * Last edited: Sep 12 16:59 1991 (mallet) */
/*
 * $Id: Requests.h,v 1.3 1991/09/27 17:03:57 mallet Exp $
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
 * $XConsortium: Requests.h,v 1.2 90/06/09 20:20:44 dmatic Exp $
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

#if ! defined(XlibSpecificationRelease)
#ifndef _XPOINTER_DEF
typedef char *XPointer;
#define _XPOINTER_DEF
#endif
#endif

typedef struct {
    Boolean  success;
    Position at_x, at_y;
    Position from_x, from_y,
             to_x, to_y;
    void   (*draw)();
    Pixel    value;
    Time     time;
    int      state;
} PWStatus;

void OnePointEngage();
void OnePointTerminate();
void OnePointTerminateTransparent();
void DragOnePointEngage();
void DragOnePointTerminate();
void TwoPointsEngage();
void TwoPointsTerminate();
void TwoPointsTerminateTransparent();
void TwoPointsTerminateTimed();
void DragTwoPointsEngage();
void DragTwoPointsTerminate();
void Interface();
void Paste();

void PWMark();
void PWUnmark();
void PWStore();
void PWDragMarked();
void PWDragStored();
void PWRestore();
void PWCopy();
void PWMove();
void PWDrawPoint();
void PWDrawLine();
void PWBlindLine();
void PWDrawRectangle();
void PWDrawFilledRectangle();
void PWDrawCircle();
void PWDrawFilledCircle();
void PWFloodFill();
void PWDrawHotSpot();
void PWChangeNotify();
void PWZoomIn();

static PWRequestRec requests[] =
{
{MarkRequest, sizeof(PWStatus),
     TwoPointsEngage, (XPointer) PWDrawRectangle,
     TwoPointsTerminateTimed, (XPointer) PWSelect,
     NULL, (XPointer) NULL},
{RestoreRequest, sizeof(PWStatus),
     OnePointEngage, (XPointer) PWDragStored,
     OnePointTerminate, (XPointer) PWRestore,
     NULL, (XPointer) NULL},
{ImmediateCopyRequest, sizeof(PWStatus),
     OnePointEngage, (XPointer) PWDragMarked,
     OnePointTerminate, (XPointer) PWCopy,
     NULL, (XPointer) NULL},
{ImmediateMoveRequest, sizeof(PWStatus),
     OnePointEngage, (XPointer) PWDragMarked,
     OnePointTerminate, (XPointer) PWMove,
     NULL, (XPointer) NULL},
{CopyRequest, sizeof(PWStatus),
     DragOnePointEngage, (XPointer) Paste,
     DragOnePointTerminate, (XPointer) ImmediateCopyRequest,
     Interface, (XPointer) PWUnmark},
{MoveRequest, sizeof(PWStatus),
     DragOnePointEngage, (XPointer) Paste,
     DragOnePointTerminate, (XPointer) ImmediateMoveRequest,
     Interface, (XPointer) PWUnmark},
{PointRequest, sizeof(PWStatus),
     DragOnePointEngage, (XPointer) PWDrawPoint,
     DragOnePointTerminate, (XPointer) PWDrawPoint,
     NULL, (XPointer) NULL},
{CurveRequest, sizeof(PWStatus),
     DragTwoPointsEngage, (XPointer) PWBlindLine,
     DragTwoPointsTerminate, (XPointer) PWBlindLine,
     NULL, (XPointer) NULL},
{LineRequest, sizeof(PWStatus), 
     TwoPointsEngage, (XPointer) PWDrawLine, 
     TwoPointsTerminate, (XPointer) PWDrawLine,
     NULL, (XPointer) NULL},
{RectangleRequest, sizeof(PWStatus), 
     TwoPointsEngage, (XPointer) PWDrawRectangle,
     TwoPointsTerminate, (XPointer) PWDrawRectangle,
     NULL, (XPointer) NULL},
{FilledRectangleRequest, sizeof(PWStatus), 
     TwoPointsEngage, (XPointer) PWDrawRectangle,
     TwoPointsTerminate, (XPointer) PWDrawFilledRectangle,
     NULL, (XPointer) NULL},
{CircleRequest, sizeof(PWStatus), 
     TwoPointsEngage, (XPointer) PWDrawCircle,
     TwoPointsTerminate, (XPointer) PWDrawCircle,
     NULL, (XPointer) NULL},
{FilledCircleRequest, sizeof(PWStatus), 
     TwoPointsEngage, (XPointer) PWDrawCircle, 
     TwoPointsTerminate, (XPointer) PWDrawFilledCircle,
     NULL, (XPointer) NULL},
{FloodFillRequest, sizeof(PWStatus),
     OnePointEngage, (XPointer) NULL,
     OnePointTerminate, (XPointer) PWFloodFill,
     NULL, (XPointer) NULL},
{HotSpotRequest, sizeof(PWStatus),
     OnePointEngage, (XPointer) PWDrawHotSpot,
     OnePointTerminate, (XPointer) PWDrawHotSpot,
     NULL, (XPointer) NULL},
{ZoomInRequest, sizeof(PWStatus),
     TwoPointsEngage, (XPointer) PWDrawRectangle,
     TwoPointsTerminate, (XPointer) PWZoomIn,
     NULL, (XPointer) NULL},
};


