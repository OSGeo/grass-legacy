static char rcsid[] = "@(#)XGRASS $Id: client.c,v 0.0 1992/05/05 14:56:11 sink Exp sink $";
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
XgWMClientMessage(w, cld, msg)
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

    /* Get the atom name associated with the client message */
    str = XGetAtomName(msg->display, msg->message_type);
    fprintf(stderr,"msg type = %s (%d)\n", str, msg->message_type);
    XFree(str);

    /* Get the atom name of the message itself... */
    str = XGetAtomName(msg->display, message);
    fprintf(stderr,"message = %s (%d)\n", str, message);
    XFree(str);

    if (message == WM_DELETE_WINDOW) {
        puts("closing window");
        if ( shell == _XG_Global.applShell ) {
	    puts("exiting");
	    XgrassExit(0);
	}
        if ( shell == _XG_Global.historyWidget ) {
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
