static char rcsid[] = "@(#)XGRASS $Id: client.c,v 0.0.0.3 1992/05/05 14:59:36 kurt Exp kurt $";
/*
 * File: client.c
 *
 * Desc: handle client message events
 *
 * Auth: Kurt Buehler
 *
 * Date: Tue Oct 22 11:28:35 CDT 1991
 *
 * Modification History:
 *
 *
 */
#include "xgrass.h"

void
_XgWMClientMessage(w, cld, msg)
Widget w;
XtPointer cld;
XClientMessageEvent *msg;
{
    Widget shell = (Widget)cld;
    char  *str;
    int   message = msg->data.l[0];
    Atom  WM_DELETE_WINDOW;

    if (msg->type != ClientMessage)
        return;

    WM_DELETE_WINDOW = XmInternAtom(msg->display, "WM_DELETE_WINDOW", False);

    if (message == WM_DELETE_WINDOW) {
	XFlush(XtDisplay(w));
	exit(0);
    }
}
