#include "xgdisp.h"

void
_XgWMClientMessage(w, cld, msg)
Widget w;
XtPointer cld;
XClientMessageEvent *msg;
{
    Widget shell = (Widget)cld;
    int   message = msg->data.l[0];
    Atom  WM_DELETE_WINDOW;

    if (msg->type != ClientMessage)
        return;

    WM_DELETE_WINDOW = XmInternAtom(msg->display, "WM_DELETE_WINDOW", False);

    if (message == WM_DELETE_WINDOW) {
        if ( shell == Global.applShell ) {
	    int exit_code = 0;

	    ConfirmExit(&exit_code);
        } else if ( XmIsFileSelectionBox(shell)) {
	    XtUnmanageChild(shell);
	} else {
	    XtPopdown(shell);
	}
    }
}
