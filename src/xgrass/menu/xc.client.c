static char rcsid[] = "@(#)XGRASS $Id: xc.client.c,v 0.0.0.1 1992/05/05 14:59:01 kurt Exp kurt $";
/*
 * File: client.c
 *
 * Desc: handle client message events
 *
 * Auth: Kurt Buehler
 *
 * Date: Tue Feb 11 16:09:50 CST 1992
 *
 * Modification History:
 *
 *
 */
#include "xc.xclip.h"

void
_XcWMClientMessage(w, cld, cad)
Widget w;
XtPointer cld;
XtPointer cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XClientMessageEvent *msg = (XClientMessageEvent *) cad;
    Widget shell = (Widget)theBunch->data;
    char  *str;
    int   message = msg->data.l[0];
    Atom  WM_DELETE_WINDOW;

    if (msg->type != ClientMessage)
        return;

    WM_DELETE_WINDOW = XmInternAtom(msg->display, "WM_DELETE_WINDOW", False);

    if (message == WM_DELETE_WINDOW) {
        if ( shell == Global->applShell ) {
	    int exit_code = 0;

	    XClipExit(Global->applShell, exit_code,Global);
	} else {
	    XtPopdown(shell);
	}
    }
}
