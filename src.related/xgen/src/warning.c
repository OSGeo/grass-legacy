#include "xgen.h"

void
XgWarningDialog(parent, string)
    Widget                          parent;
    char                           *string;
{
    Widget                          w;
    Widget                          shell;
    int n = 0;

    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);
    w = XmCreateWarningDialog(parent, "xgen Warning", args, n);
    XtVaSetValues(w,
                  XmNmessageString, XmStringCreateSimple(string),
                  XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
                  XmNokLabelString, XmStringCreateSimple("Acknowledged"),
                  NULL);

    shell = XtParent(w);
    if (XmIsMotifWMRunning(shell)) {
        unsigned int                    decor_flags;

        decor_flags = MWM_DECOR_BORDER;

        XtVaSetValues(shell,
                      XmNmwmDecorations, decor_flags,
                      NULL);
    }
    XtUnmanageChild(XmMessageBoxGetChild(w, XmDIALOG_HELP_BUTTON));
    XtUnmanageChild(XmMessageBoxGetChild(w, XmDIALOG_CANCEL_BUTTON));

    XtManageChild(w);

}

void
XgNotifyDialog(parent, string)
    Widget                          parent;
    char                           *string;
{
    Widget                          w;
    Widget                          shell;
    int n = 0;

    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);
    w = XmCreateInformationDialog(parent, "xgen Notification", args, n);
    XtVaSetValues(w,
                  XmNmessageString, XmStringCreateLtoR(string,SDC),
                  XmNdialogStyle, XmDIALOG_MODELESS,
                  XmNokLabelString, XmStringCreateSimple("Acknowledged"),
                  NULL);

    shell = XtParent(w);
    if (XmIsMotifWMRunning(shell)) {
        unsigned int                    decor_flags;

        decor_flags = MWM_DECOR_BORDER;

        XtVaSetValues(shell,
                      XmNmwmDecorations, decor_flags,
                      NULL);
    }
    XtUnmanageChild(XmMessageBoxGetChild(w, XmDIALOG_HELP_BUTTON));
    XtUnmanageChild(XmMessageBoxGetChild(w, XmDIALOG_CANCEL_BUTTON));

    XtManageChild(w);

}
