static char rcsid[] = "@(#)XGRASS $Id: warning.c,v 0.0 1992/05/05 14:56:28 sink Exp sink $";
/*
 * File: warning.c
 *
 * Desc: contains code for displaying an XGRASS warning
 *
 * Auth: Kurt Buehler
 *
 * Date: Wed Oct 23 08:53:08 CDT 1991
 *
 * Modification History:
 *
 *
 */

#include "xgrass_lib.h"

void
#ifdef _NO_PROTO
XgWarningDialog(parent, string)
Widget parent;
char *string;
#else
XgWarningDialog( Widget parent, char *string)
#endif
{
    Arg al[10];
    int ac;
    Widget w;
    Widget shell;
    Atom protocol;

    if (!parent) {
      fprintf(stderr,"%s\n",string);
      return;
    }
    ac = 0;
    XtSetArg(al[ac],XmNokLabelString,XmStringCreateSimple("OK")); ac++;
    w = XmCreateWarningDialog(parent,"XGRASS Warning",al,ac);
    XtVaSetValues(w, 
	XmNmessageString, XmStringCreateSimple(string),
	XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
	NULL);

    shell = XtParent(w);
    if ( XmIsMotifWMRunning(shell) ) {
	unsigned int decor_flags;

	decor_flags = MWM_DECOR_BORDER;

	XtVaSetValues(shell,
	    XmNmwmDecorations, decor_flags,
	    NULL);
    }

    XtUnmanageChild( XmMessageBoxGetChild(w,XmDIALOG_HELP_BUTTON));
    XtUnmanageChild( XmMessageBoxGetChild(w,XmDIALOG_CANCEL_BUTTON));

    XtManageChild(w);

}
