/* * Last edited: Sep  5 16:19 1991 (mallet) */
/*
 * $Id: Handlers.c,v 1.2 1991/09/27 17:12:39 mallet Exp $
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
 * $XConsortium: Handlers.c,v 1.1 90/06/09 20:20:35 dmatic Exp $
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
#include <X11/Xos.h>
#include "PixmapP.h"
    
#include <stdio.h>
#include <math.h>

#define XtStrlen(s)                   ((s) ? strlen(s) : 0)
#define abs(x)                        (((x) > 0) ? (x) : -(x))
#define min(x, y)                     (((x) < (y)) ? (x) : (y))
#define max(x, y)                     (((x) > (y)) ? (x) : (y))


/*****************************************************************************
 *                                  Handlers                                 *
 *****************************************************************************/

#define QueryInSquare(PW, x, y, square_x, square_y)\
    ((InPixmapX(PW, x) == (square_x)) &&\
     (InPixmapY(PW, y) == (square_y)))


void DragOnePointHandler(w, status, event)
     Widget       w;
     PWStatus    *status;
     XEvent      *event;
{
    PixmapWidget PW = (PixmapWidget) w;

    if (_PWDEBUG)
	fprintf(stderr, "D1PH ");

    switch (event->type) {
    
    case ButtonPress:
	if (event->xbutton.state != status->state) return;
	if (!QuerySet(status->at_x, status->at_y)) {
	    PWStoreToBuffer(w);
	    status->value = Value(PW, event->xbutton.button);
	    status->time = event->xbutton.time;
	    status->at_x = InPixmapX(PW, event->xbutton.x);
	    status->at_y = InPixmapY(PW, event->xbutton.y);
	    status->success = (Boolean) status->draw;
	    if (status->draw)
		(*status->draw)(w,
				status->at_x, status->at_y, status->value);
	}
	break;
	
    case ButtonRelease:
	if (QuerySet(status->at_x, status->at_y)) {
	    status->value = Value(PW, event->xbutton.button);
	    status->time = event->xbutton.time;
	    status->at_x = InPixmapX(PW, event->xbutton.x);
	    status->at_y = InPixmapY(PW, event->xbutton.y);
	    status->success = (Boolean) status->draw;
      
	    PWTerminateRequest(w, TRUE);
	}
	break;

    case MotionNotify:
	if (QuerySet(status->at_x, status->at_y)) {
	    if (!QueryInSquare(PW, event->xmotion.x, event->xmotion.y,
			       status->at_x, status->at_y)) {
		status->at_x = InPixmapX(PW, event->xmotion.x);
		status->at_y = InPixmapY(PW, event->xmotion.y);
		if (status->draw)
		    (*status->draw)(w,
				    status->at_x, status->at_y, status->value);
	    }
	}
	break;

    }
}

void DragOnePointEngage(w, status, draw, state)
    Widget      w;
    PWStatus   *status;
    void      (*draw)();
    int        *state;
{
    
    status->at_x = NotSet;
    status->at_y = NotSet;
    status->draw = draw;
    status->success = False;
    status->state = *state;
    
    XtAddEventHandler(w,
		      ButtonPressMask | ButtonReleaseMask | PointerMotionMask,
		      FALSE, DragOnePointHandler, status);
}

void DragOnePointTerminate(w, status, client_data)
    Widget     w;
    PWStatus  *status;
    XPointer    client_data;
{
    
    if (status->success) {
	PWChangeNotify(w, NULL, NULL);
	PWSetChanged(w);
    }
    
    XtRemoveEventHandler(w,
		 ButtonPressMask | ButtonReleaseMask | PointerMotionMask,
		 FALSE, DragOnePointHandler, status);
    
}

void OnePointHandler(w, status, event)
    Widget       w;
    PWStatus    *status;
    XEvent      *event;
{
    PixmapWidget PW = (PixmapWidget) w;
    
    if (_PWDEBUG)
	fprintf(stderr, "1PH ");

    switch (event->type) {
	
    case Expose:
	if (QuerySet(status->at_x, status->at_y)) {
	    PWClip(w, 
		   InPixmapX(PW, event->xexpose.x), 
		   InPixmapY(PW, event->xexpose.y),
		   InPixmapX(PW, event->xexpose.x + event->xexpose.width),
		   InPixmapY(PW, event->xexpose.y + event->xexpose.height));
	    if (status->draw)
		(*status->draw)(w,
				status->at_x, status->at_y, Highlight);
	    
	    PWUnclip(w);
	}
	break;
	
    case ButtonPress:
	if (event->xbutton.state != status->state) return;
	if (!QuerySet(status->at_x, status->at_y)) {
	    status->value = Value(PW, event->xbutton.button);
	    status->time = event->xbutton.time;
	    status->at_x = InPixmapX(PW, event->xbutton.x);
	    status->at_y = InPixmapY(PW, event->xbutton.y);
	    if (status->draw)
		(*status->draw)(w,
				status->at_x, status->at_y, Highlight);
	}
	break;
	
    case ButtonRelease:
	if (QuerySet(status->at_x, status->at_y)) {
	    if (status->draw)
		(*status->draw)(w,
				status->at_x, status->at_y, Highlight);
	    
	    /* the following 3 lines is a small fix to Ctrl ops */
	    if (event->xbutton.state&ControlMask)
	      status->value = Set;
	    else status->value = Value(PW, event->xbutton.button);

	    status->time = event->xbutton.time;
	    status->at_x = InPixmapX(PW, event->xbutton.x);
	    status->at_y = InPixmapY(PW, event->xbutton.y);
	    status->success = True;
	    
	    PWTerminateRequest(w, TRUE);
	}
	break;
	
    case MotionNotify:
	if (QuerySet(status->at_x, status->at_y)) {
	    if (!QueryInSquare(PW, event->xmotion.x, event->xmotion.y,
			       status->at_x, status->at_y)) {
		if (status->draw)
		    (*status->draw)(w,
				    status->at_x, status->at_y, Highlight);
		status->at_x = InPixmapX(PW, event->xmotion.x);
		status->at_y = InPixmapY(PW, event->xmotion.y);
		if (status->draw)
		    (*status->draw)(w,
				    status->at_x, status->at_y, Highlight);
	    }
	}      
	break;
    }
}

void OnePointEngage(w, status, draw, state)
    Widget      w;
    PWStatus   *status;
    void      (*draw)();
    int        *state;
{
    status->at_x = NotSet;
    status->at_y = NotSet;
    status->draw = draw;
    status->success = False;
    status->state = *state;

    XtAddEventHandler(w,
		      ButtonPressMask | ButtonReleaseMask | 
		      ExposureMask | PointerMotionMask,
		      FALSE, OnePointHandler, status);
}

void OnePointImmediateEngage(w, status, draw, state)
    Widget      w;
    PWStatus   *status;
    void      (*draw)();
    int        *state;
{
    status->at_x = 0;
    status->at_y = 0;
    status->draw = draw;
    status->success = False;
    status->state = *state;
    
    if (status->draw)
	(*status->draw)(w,
			status->at_x, status->at_y, Highlight);
    
    XtAddEventHandler(w,
		      ButtonPressMask | ButtonReleaseMask | 
		      ExposureMask | PointerMotionMask,
		      FALSE, OnePointHandler, status);
}

void OnePointTerminate(w, status, draw)
    Widget     w;
    PWStatus  *status;
    void     (*draw)();
{
    
    if (status->success && draw) {
	PWStoreToBuffer(w);
	(*draw)(w,
		status->at_x, status->at_y,
		status->value);
	PWChangeNotify(w, NULL, NULL);
	PWSetChanged(w);
    }    
    else
	if (QuerySet(status->at_x, status->at_y))
	    if (status->draw)
		(*status->draw)(w,
				status->at_x, status->at_y, Highlight);
    
    XtRemoveEventHandler(w,
			 ButtonPressMask | ButtonReleaseMask | 
			 ExposureMask | PointerMotionMask,
			 FALSE, OnePointHandler, status);
}

void OnePointTerminateTransparent(w, status, draw)
    Widget     w;
    PWStatus  *status;
    void     (*draw)();
{
    
    if (status->success && draw)
	(*draw)(w,
		status->at_x, status->at_y,
		status->value);
    else
	if (QuerySet(status->at_x, status->at_y))
	    if (status->draw)
		(*status->draw)(w,
				status->at_x, status->at_y, Highlight);
    
    XtRemoveEventHandler(w,
			 ButtonPressMask | ButtonReleaseMask | 
			 ExposureMask | PointerMotionMask,
			 FALSE, OnePointHandler, status);
    
}


void TwoPointsHandler(w, status, event)
    Widget      w;
    PWStatus   *status;
    XEvent     *event;
{
    PixmapWidget PW = (PixmapWidget) w;

    if (_PWDEBUG)
	fprintf(stderr, "2PH ");
    
    switch (event->type) {
	
    case Expose:
	if (QuerySet(status->from_x, status->from_y) && 
	    QuerySet(status->to_x, status->to_y)) {
	    PWClip(w, 
		   InPixmapX(PW, event->xexpose.x), 
		   InPixmapY(PW, event->xexpose.y),
		   InPixmapX(PW, event->xexpose.x + event->xexpose.width),
		   InPixmapY(PW, event->xexpose.y + event->xexpose.height));
	    if (status->draw)
		(*status->draw)(w,
				status->from_x, status->from_y, 
				status->to_x, status->to_y, Highlight);
	    PWUnclip(w);
	}
	break;
	
    case ButtonPress:
	if (event->xbutton.state != status->state) return;
	if (!QuerySet(status->from_x, status->from_y)) {
	    status->value = Value(PW, event->xbutton.button);
	    status->time = event->xbutton.time;
	    status->from_x = InPixmapX(PW, event->xbutton.x);
	    status->from_y = InPixmapY(PW, event->xbutton.y);
	    status->to_x = InPixmapX(PW, event->xbutton.x);
	    status->to_y = InPixmapY(PW, event->xbutton.y);
	    if (status->draw)
		(*status->draw)(w,
				status->from_x, status->from_y, 
				status->to_x, status->to_y, Highlight);
	}
	break;
	
    case ButtonRelease:
	if (QuerySet(status->from_x, status->from_y)) {
	    if (status->draw)
		(*status->draw)(w,
				status->from_x, status->from_y, 
				status->to_x, status->to_y, Highlight);
	    status->value = Value(PW, event->xbutton.button);
	    status->time = event->xbutton.time;	    
	    status->to_x = InPixmapX(PW, event->xbutton.x);
	    status->to_y = InPixmapY(PW, event->xbutton.y);
	    status->success = True;
	    
	    PWTerminateRequest(w, TRUE);
	}
	break;
	
    case MotionNotify:
	if (QuerySet(status->from_x, status->from_y)) {
	    if (QuerySet(status->to_x, status->to_y)) {
		if (!QueryInSquare(PW, event->xmotion.x, event->xmotion.y,
				   status->to_x, status->to_y)) {
		    if (status->draw)
			(*status->draw)(w,
					status->from_x, status->from_y, 
					status->to_x, status->to_y, Highlight);
		    status->to_x = InPixmapX(PW, event->xmotion.x);
		    status->to_y = InPixmapY(PW, event->xmotion.y);
		    if (status->draw)
			(*status->draw)(w,
					status->from_x, status->from_y, 
					status->to_x, status->to_y, Highlight);
		}
	    }
	    else {
		status->to_x = InPixmapX(PW, event->xmotion.x);
		status->to_y = InPixmapY(PW, event->xmotion.y);
		if (status->draw)
		    (*status->draw)(w,
				    status->from_x, status->from_y, 
				    status->to_x, status->to_y, Highlight);
	    }
	}
	break;
    }
}

void TwoPointsEngage(w, status, draw, state)
    Widget     w;
    PWStatus  *status;
    void     (*draw)();
    int       *state;
{
    
    status->from_x = NotSet;
    status->from_y = NotSet;
    status->to_x = NotSet;
    status->to_y = NotSet;
    status->draw = draw;
    status->success = False;
    status->state = *state;

    XtAddEventHandler(w,
		      ButtonPressMask | ButtonReleaseMask | 
		      ExposureMask | PointerMotionMask,
		      FALSE, TwoPointsHandler, status);
}

void TwoPointsTerminate(w, status, draw)
    Widget    w;
    PWStatus *status;
    void    (*draw)();
{
    
    if (status->success && draw) {
	PWStoreToBuffer(w);
	(*draw)(w,
		status->from_x, status->from_y,
		status->to_x, status->to_y,
		status->value);
	PWChangeNotify(w, NULL, NULL);
	PWSetChanged(w);
    }
    else
	if (QuerySet(status->from_x, status->from_y) && 
	    QuerySet(status->to_x, status->to_y))
	    if (status->draw)
		(*status->draw)(w,
				status->from_x, status->from_y, 
				status->to_x, status->to_y, Highlight);
    
    XtRemoveEventHandler(w,
			 ButtonPressMask | ButtonReleaseMask | 
			 ExposureMask | PointerMotionMask,
			 FALSE, TwoPointsHandler, status);
}

void TwoPointsTerminateTransparent(w, status, draw)
    Widget    w;
    PWStatus *status;
    void    (*draw)();
{
    
    if (status->success && draw)
	(*draw)(w,
		status->from_x, status->from_y,
		status->to_x, status->to_y,
		status->value);
    else
	if (QuerySet(status->from_x, status->from_y) && 
	    QuerySet(status->to_x, status->to_y))
	    if (status->draw)
		(*status->draw)(w,
				status->from_x, status->from_y, 
				status->to_x, status->to_y, Highlight);
    
    XtRemoveEventHandler(w,
			 ButtonPressMask | ButtonReleaseMask | 
			 ExposureMask | PointerMotionMask,
			 FALSE, TwoPointsHandler, status);
}

void TwoPointsTerminateTimed(w, status, draw)
    Widget    w;
    PWStatus *status;
    void    (*draw)();
{
    
    if (status->success && draw)
	(*draw)(w,
		status->from_x, status->from_y,
		status->to_x, status->to_y,
		status->time);
    else
	if (QuerySet(status->from_x, status->from_y) && 
	    QuerySet(status->to_x, status->to_y))
	    if (status->draw)
		(*status->draw)(w,
				status->from_x, status->from_y, 
				status->to_x, status->to_y, Highlight);
    
    XtRemoveEventHandler(w,
			 ButtonPressMask | ButtonReleaseMask | 
			 ExposureMask | PointerMotionMask,
			 FALSE, TwoPointsHandler, status);
}

void Interface(w, status, action)
    Widget     w;
    XPointer    status;
    void     (*action)();
{
 	(*action)(w);
}

void Paste(w, at_x, at_y, value)
    Widget    w;
    Position  at_x, at_y;
    int       value;
{
    PixmapWidget    PW = (PixmapWidget) w;
    PWStatus       *my_status;
    PWRequest       request;

    my_status = (PWStatus *) 
	PW->pixmap.request_stack[PW->pixmap.current].status;

    my_status->draw = NULL;

   request = (PWRequest)
   PW->pixmap.request_stack[PW->pixmap.current].request->terminate_client_data;
	
    PWTerminateRequest(w, FALSE);
    
    if ((at_x == max(PW->pixmap.mark.from_x, min(at_x, PW->pixmap.mark.to_x)))
	&&
      (at_y == max(PW->pixmap.mark.from_y, min(at_y, PW->pixmap.mark.to_y)))) {
	
	PWStatus *status;
	
	if (_PWDEBUG)
	    fprintf(stderr, "Prepaste request: %s\n", request);
	
	PWEngageRequest(w, request, False, &(my_status->state), sizeof(int));
	
	status = (PWStatus *) 
	    PW->pixmap.request_stack[PW->pixmap.current].status;
	
	status->at_x = at_x;
	status->at_y = at_y;
	status->value = value;
	(*status->draw) (w, at_x, at_y, Highlight);	
    }
    else {

	PWStatus *status;
	
      PWEngageRequest(w, MarkRequest, False, &(my_status->state), sizeof(int));
	
	status = (PWStatus *) 
	    PW->pixmap.request_stack[PW->pixmap.current].status;
	
	status->from_x = status->to_x = at_x;
	status->from_y = status->to_y = at_y;
	status->value = value;
	(*status->draw) (w, at_x, at_y, at_x, at_y, Highlight);
    }
}


void DragTwoPointsHandler(w, status, event)
    Widget      w;
    PWStatus   *status;
    XEvent     *event;
{
    PixmapWidget PW = (PixmapWidget) w;

    if (_PWDEBUG)
	fprintf(stderr, "D2PH ");

    switch (event->type) {
	
    case ButtonPress:
	if (event->xbutton.state != status->state) return;
	if (!QuerySet(status->from_x, status->from_y)) {
	    PWStoreToBuffer(w);
	    status->value = Value(PW, event->xbutton.button);
	    status->time = event->xbutton.time;
	    status->from_x = InPixmapX(PW, event->xbutton.x);
	    status->from_y = InPixmapY(PW, event->xbutton.y);
	    status->to_x = InPixmapX(PW, event->xbutton.x);
	    status->to_y = InPixmapY(PW, event->xbutton.y);
	    status->success = (Boolean) status->draw;
	    if (status->draw)
		(*status->draw)(w,
				status->from_x, status->from_y, 
				status->to_x, status->to_y, status->value);
	}
	break;
	
    case ButtonRelease:
	if (QuerySet(status->from_x, status->from_y)) {
	    status->value = Value(PW, event->xbutton.button);
	    status->time = event->xbutton.time;	    
	    status->from_x = status->to_x;
	    status->from_y = status->to_y;
	    status->to_x = InPixmapX(PW, event->xbutton.x);
	    status->to_y = InPixmapY(PW, event->xbutton.y);
	    status->success = True;
	    
	    PWTerminateRequest(w, TRUE);
	}
	break;
	
    case MotionNotify:
	if (QuerySet(status->from_x, status->from_y)) {
	    if (QuerySet(status->to_x, status->to_y)) {
		if (!QueryInSquare(PW, event->xmotion.x, event->xmotion.y,
				   status->to_x, status->to_y)) {
		    status->from_x = status->to_x;
		    status->from_y = status->to_y;
		    status->to_x = InPixmapX(PW, event->xmotion.x);
		    status->to_y = InPixmapY(PW, event->xmotion.y);
		    if (status->draw)
			(*status->draw)(w,
					status->from_x, status->from_y, 
					status->to_x, status->to_y, status->value);
		}
	    }
	}
	break;
    }
}

void DragTwoPointsEngage(w, status, draw, state)
    Widget     w;
    PWStatus  *status;
    void     (*draw)();
    int       *state;
{
    
    status->from_x = NotSet;
    status->from_y = NotSet;
    status->to_x = NotSet;
    status->to_y = NotSet;
    status->draw = draw;
    status->success = False;
    status->state = *state;

    XtAddEventHandler(w,
		      ButtonPressMask | ButtonReleaseMask | PointerMotionMask,
		      FALSE, DragTwoPointsHandler, status);
}

void DragTwoPointsTerminate(w, status, draw)
    Widget     w;
    PWStatus  *status;
    void     (*draw)();
{
    
    if (status->success && draw) {
	if ((status->from_x != status->to_x) 
	    || 
	    (status->from_y != status->to_y))
	    (*draw)(w,
		    status->from_x, status->from_y,
		    status->to_x, status->to_y,
		    status->value);
	PWChangeNotify(w, NULL, NULL);
	PWSetChanged(w);
    }
    
    XtRemoveEventHandler(w,
		         ButtonPressMask | ButtonReleaseMask | PointerMotionMask,
			 FALSE, DragTwoPointsHandler, status);
}


void PWTPaste(w, event)
    Widget  w;
    XEvent *event;
{
    PixmapWidget PW = (PixmapWidget) w;

    if (!PW->pixmap.selection.own)
	PWRequestSelection(w, event->xbutton.time, TRUE);
    else 
	PWStore(w);

    if (!PWQueryStored(w))
	return;

    PWEngageRequest(w, RestoreRequest, False, 
		    &(event->xbutton.state), sizeof(int));
    
    OnePointHandler(w,
	       (PWStatus*) PW->pixmap.request_stack[PW->pixmap.current].status,
	       event);
}

void PWTMark(w, event)
    Widget  w;
    XEvent *event;
{
    PixmapWidget PW = (PixmapWidget) w;

    PWEngageRequest(w, MarkRequest, False,
		    &(event->xbutton.state), sizeof(int));
    TwoPointsHandler(w,
            (PWStatus*) PW->pixmap.request_stack[PW->pixmap.current].status,
	     event);
}

void PWTUnmark(w)
    Widget w;
{
    PWUnmark(w);
}


/*****************************************************************************/
