/* * Last edited: Sep  5 16:17 1991 (mallet) */
/*
 * $Id: CutAndPaste.c,v 1.2 1991/09/27 17:06:20 mallet Exp $
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
 * $XConsortium: CutAndPaste.c,v 1.1 90/06/09 20:20:17 dmatic Exp $
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
 *                               Cut and Paste                               *
 *****************************************************************************/


Boolean ConvertSelection(w, selection, target, type, value, length, format)
    Widget w;
    Atom *selection, *target, *type;
    XPointer *value;
    unsigned long *length;
    int *format;
{
    PixmapWidget PW = (PixmapWidget) w;
    Pixmap *pixmap;
    XImage *image;
    Dimension width, height;
 
    switch (*target) {
/*
    case XA_TARGETS:
	*type = XA_ATOM;
	*value = (XPointer) pixmapClassRec.pixmap_class.targets;
	*length = pixmapClassRec.pixmap_class.num_targets;
	*format = 32;
	return True;
*/
    case XA_BITMAP:
    case XA_PIXMAP:
	if (!PWQueryMarked(w)) return False;
	width = PW->pixmap.mark.to_x - PW->pixmap.mark.from_x + 1;
	height = PW->pixmap.mark.to_y - PW->pixmap.mark.from_y + 1;
	image = CreatePixmapImage(PW, width, height);
	CopyImageData(PW->pixmap.image, image, 
		      PW->pixmap.mark.from_x, PW->pixmap.mark.from_y,
		      PW->pixmap.mark.to_x, PW->pixmap.mark.to_y, 0, 0);
	pixmap = (Pixmap *) XtMalloc(sizeof(Pixmap));
	*pixmap = GetPixmap(PW, image);
	DestroyPixmapImage(&image);
	*type = XA_PIXMAP;
	*value = (XPointer) pixmap;
	*length = 1;
	*format = 32;
	return True;

    case XA_STRING:
	*type = XA_STRING;
	*value = "Hello world!\n";
	*length = XtStrlen(*value);
	*format = 8;
	return True;

    default:
	return False;
    }
}

void LoseSelection(w, selection)
    Widget w;
    Atom *selection;
{
    PixmapWidget PW = (PixmapWidget) w;

    if (_PWDEBUG)
	fprintf(stderr, "Lost Selection\n");
    PW->pixmap.selection.own = False;
    PWUnmark(w);
}

void SelectionDone(w, selection, target)
    Widget w;
    Atom *selection, *target;
{
    PixmapWidget PW = (PixmapWidget) w;
/*  
    if (*target != XA_TARGETS)
	XtFree(PW->pixmap.value);
*/
}

void PWGrabSelection(w, time)
    Widget w;
    Time time;
{
    PixmapWidget PW = (PixmapWidget) w;

    PW->pixmap.selection.own = XtOwnSelection(w, XA_PRIMARY, time,
					      ConvertSelection, 
					      LoseSelection, 
					      SelectionDone);
	if (_PWDEBUG && PW->pixmap.selection.own)
	    fprintf(stderr, "Own the selection\n");
}

XImage *GetImage();

void SelectionCallback(w, client_data, selection, type, value, length, format)
    Widget w;
    XPointer client_data;
    Atom *selection, *type;
    XPointer value;
    unsigned long *length;
    int *format;
{
    PixmapWidget PW = (PixmapWidget) w;
    Pixmap *pixmap;

   switch (*type) {
	
    case XA_BITMAP:
    case XA_PIXMAP:
	DestroyPixmapImage(&PW->pixmap.storage);
	pixmap = (Pixmap *) value;
	PW->pixmap.storage = GetImage(PW, *pixmap);
	XFreePixmap(XtDisplay(w), *pixmap);
	break;
	
    case XA_STRING:
	if (_PWDEBUG)
	    fprintf(stderr, "Received:%s\n", value);
	break;

    default:
	XtWarning(" selection request failed.  PixmapWidget");
	break;
    }

    PW->pixmap.selection.limbo = FALSE;
}

void PWRequestSelection(w, time, wait)
    Widget w;
    Time time;
    Boolean wait;
{
    PixmapWidget PW = (PixmapWidget) w;
    
    XtGetSelectionValue(w, XA_PRIMARY, XA_PIXMAP,
			SelectionCallback, NULL, time);

    PW->pixmap.selection.limbo = TRUE;

    if (wait)
	while (PW->pixmap.selection.limbo) {
	    XEvent event;
	    XtNextEvent(&event);
	    XtDispatchEvent(&event);
	}
}

/*****************************************************************************/
