#include "xgrass.h"

static void
Exit(w, cld, cad)
Widget w;
XtPointer cld, cad;
{
   XFlush(XtDisplay(w));
   exit(0);
}

static void
Execute(w, cld, cad)
Widget w;
XtPointer cld, cad;
{
    InteractorCallbackStruct *cbs = (InteractorCallbackStruct *)cad;
    int err = 0;
    char *command;

    XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET,&command);

    XgSystem(XtParent(w),command, False, &err, 1);
}

CreateInterface(shell, buf)
Widget shell;
char *buf;
{
    Widget dialog;
    XmString xms = 
	XmStringCreateLtoR("Command Executor Program\n\nEnter command:",
	    XmSTRING_DEFAULT_CHARSET);
    XmString xms1 = XmStringCreateSimple(buf);
    XmString xms2 = XmStringCreateSimple("Execute");
    XmString xms3 = XmStringCreateSimple("Exit");

    dialog = XgCreateInteractorPrompt(shell, "interactor", NULL, 0);
    XtVaSetValues(dialog, XmNpromptLabelString, xms, 
	XmNtextString, xms1,
	XmNokLabelString, xms2,
	XmNcancelLabelString, xms3,
	NULL);
    XtAddCallback(dialog,XmNokCallback, Execute, NULL);
    XtAddCallback(dialog,XmNcancelCallback, Exit, NULL);
    XtUnmanageChild(XgInteractorGetChild(dialog,XmINTERACT_APPLY_BUTTON));
    XtManageChild(dialog);
    XmStringFree(xms);
    XmStringFree(xms1);
    XmStringFree(xms2);
    XmStringFree(xms3);

}
