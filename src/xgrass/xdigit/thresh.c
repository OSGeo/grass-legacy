#include "digit.h"


Widget
make_thresh_popup (parent)
    Widget parent;
{
    Widget dialog;
    XmString t = XmStringCreateSimple ("Enter temporary threshold:");
    XmString title = XmStringCreateSimple ("nodes in threshold");
    Arg wargs[5];
    int n=0;

    XtSetArg (wargs[n], XmNSelectionLabelString, t); n++;
    XtSetArg (wargs[n], XmNautoUnmanage, False); n++;
    XtSetArg (wargs[n], XmNdialogTitle, title); n++;
    dialog = XmCreatePromptDialog (toplevel, "", wargs, n);
    XtVaSetValues (XtParent(dialog), 
		XmNsaveUnder, True,
		NULL);

    XtAddCallback (dialog, XmNcancelCallback, downcb, dialog);
    XtAddCallback (dialog, XmNokCallback, downcb, dialog);

    XtUnmanageChild(XmSelectionBoxGetChild(inter, XmDIALOG_HELP_BUTTON));
    XtUnmanageChild(dialog);

    return dialog;
}

