static char rcsid[] = "@(#)XGRASS $Id: xc.exit.c,v 0.0.0.1 1992/05/05 14:59:07 kurt Exp kurt $";
/*
 * File: exit.c
 *
 * Desc: contains code for exiting xclip
 *
 * Auth: Kurt Buehler
 *
 * Date: Wed Nov 27 07:21:29 CST 1991
 *
 * Modification History:
 *
 *
 */

#include "xc.xclip.h"

XClipExit(w,exit_code,Global)
Widget w;
int exit_code;
XclipGlobalData *Global;
{
    _XcExitVerify(w,&exit_code,Global);
}

void
_XcExitOK(w, cld, cad)
Widget w;
XtPointer cld, cad;
{
    int *exit_code = (int *)cld;

    exit(*exit_code);
}

/* Add clean up callbacks to this list...*before* _XcExitOK */
static XtCallbackRec exitCBList[] = {
    {(XtCallbackProc) _XcExitOK, (caddr_t) NULL},
    {(XtCallbackProc) NULL, (caddr_t)NULL}
};

_XcExitVerify(widget, exit_code, Global)
Widget widget;
int *exit_code;
XclipGlobalData *Global;
{
    Widget w;
    Widget shell;
    Atom protocol;
    char buf[256];
    XmString xms1, xms2, xms3;

    /* a non-zero exit code means abnormal exit...don't prompt the user */
    if ( *exit_code != 0 ) {
	/* KAB maybe we should call callbacks here...depends on exitCBList */
	_XcExitOK(NULL,(caddr_t)exit_code, NULL);
    }
    exitCBList[0].closure = (caddr_t) exit_code;

    sprintf(buf,"%s Exit",Global->_xc_Data.title);
    w = XmCreateQuestionDialog(Global->applShell,"xclip_exit",NULL,0);
    xms1 = XmStringCreateSimple(buf);
    xms2 = XmStringCreateSimple("No");
    xms3 = XmStringCreateSimple("Yes");
    XtVaSetValues(w, 
	XmNmessageString, XmStringCreateSimple("Do you really want to exit?"),
	XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
	XmNdialogTitle, xms1,
	XmNcancelLabelString, xms2,
	XmNokLabelString, xms3,
	XmNokCallback, exitCBList,
	NULL);
    XmStringFree(xms1);
    XmStringFree(xms2);
    XmStringFree(xms3);

    shell = XtParent(w);
    if ( XmIsMotifWMRunning(shell) ) {
	unsigned int decor_flags;

	decor_flags = MWM_DECOR_BORDER;

	XtVaSetValues(shell,
	    XmNmwmDecorations, decor_flags,
	    NULL);
    }

    XtUnmanageChild( XmMessageBoxGetChild(w,XmDIALOG_HELP_BUTTON));

    XtManageChild(w);

}
