static char rcsid[] = "@(#)XGRASS $Id: client.c,v 0.0.0.1 1992/05/05 14:58:23 kurt Exp kurt $";
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
        if ( shell == _XG_Global.applShell ) {
	    int exit_code = 0;

	    XgrassExit(exit_code);
	} else if ( shell == _XG_Global.historyWidget ) {
            XtPopdown(_XG_Global.historyWidget);
            XtDestroyWidget(_XG_Global.historyWidget);
            _XG_Global.historyWidget = (Widget)0;
        } else if ( XmIsFileSelectionBox(shell)) {
	    XtUnmanageChild(shell);
	} else {
	    XtPopdown(shell);
	}
    }
}
