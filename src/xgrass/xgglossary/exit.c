static char rcsid[] = "@(#)XGRASS $Id: exit.c,v 0.0.0.1 1992/05/05 14:59:43 kurt Exp kurt $";
/*
 * File: exit.c
 *
 * Desc: contains code for exiting xgglossary
 *
 * Auth: Kurt Buehler
 *
 * Date: Wed Mar  4 22:40:39 CST 1992
 *
 * Modification History:
 *
 *
 */

#include "glossary.h"

void
GlossaryExitOK(w, cld, cad)
Widget w;
XtPointer cld, cad;
{
    int *exit_code = (int *)cad;

    XFlush(dpy);
    exit(*exit_code);
}

static XtCallbackRec exitCBList[] = {
    { GlossaryExitOK, NULL},
    { NULL, NULL},
};

GlossaryExit(exit_code)
int exit_code;
{
	GlossaryExitVerify(&exit_code);
}


GlossaryExitVerify(exit_code)
int *exit_code;
{
    Widget w;
    Widget shell;
    Atom protocol;
    char buf[256];

    /* a non-zero exit code means abnormal exit...don't prompt the user */
    if ( *exit_code != 0 ) {
    XFlush(dpy);
	exit(*exit_code);
    }
    exitCBList[0].closure = (caddr_t) exit_code;

    sprintf(buf,"Glossary Exit Dialog");
    w = XmCreateQuestionDialog(top, buf ,NULL,0);
    XtVaSetValues(w, 
	XmNmessageString, XmStringCreateLtoR("Do you really want to exit?",XmSTRING_DEFAULT_CHARSET),
	XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
	XmNcancelLabelString, XmStringCreateSimple("No"),
	XmNokLabelString, XmStringCreateSimple("Yes"),
	XmNokCallback, exitCBList,
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

    XtManageChild(w);

}
