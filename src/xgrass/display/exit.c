#include "xgdisp.h"


/* Add clean up callbacks to this list... */
static XtCallbackRec exitCBList[] = {
    {(XtCallbackProc) ExitOK, (caddr_t) NULL},
    {(XtCallbackProc) NULL, (caddr_t)NULL}
};

void
#ifdef _NO_PROTO
ConfirmExit(exit_code)
int *exit_code;
#else
ConfirmExit( int *exit_code)
#endif
{
    Widget w;
    Widget shell;

    /* a non-zero exit code means abnormal exit...don't prompt the user */
    if ( *exit_code != 0 ) {
	exit(*exit_code);
    }
    exitCBList[0].closure = (caddr_t) exit_code;

    w = XmCreateQuestionDialog(Global.applShell, "Exit Dialog" ,NULL,0);
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

void
#ifdef _NO_PROTO
ExitOK(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
ExitOK( Widget w, XtPointer cld, XtPointer cad)
#endif
{
    int *exit_code = (int *)cad;

    exit(*exit_code);
}
