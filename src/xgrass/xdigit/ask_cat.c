
#include "digit.h"


set_value(w, value, cbs)
    Widget w;
    int *value;
    XmSelectionBoxCallbackStruct *cbs;
{
    char *buf;

    if (cbs->reason == XmCR_OK)
    {
        XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET,&buf);
	*value = atoi (buf);
	XtFree (buf);
    }
    else
	*value = 0;
}

ask_value (msg, value)
    char *msg;
    int *value;
{
    XEvent event;
    XmString title = XmStringCreateSimple ("?");
    XmString message = XmStringCreateSimple (msg);
    XmString accept = XmStringCreateSimple ("accept");
    Widget askpop;

    *value = -1;
    askpop = XmCreatePromptDialog (toplevel, "ask", NULL, 0);
    XtVaSetValues (askpop, 
		XmNselectionLabelString, message,
                XmNokLabelString, accept,
		XmNdialogTitle, title,
		XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
		NULL);

    XmStringFree (title);
    XmStringFree (message);
    XmStringFree (accept);

    XtAddCallback (askpop, XmNokCallback, set_value, value);
    XtAddCallback (askpop, XmNcancelCallback, set_value, value);
    XtUnmanageChild(XmSelectionBoxGetChild(askpop, XmDIALOG_HELP_BUTTON));
    
    XtManageChild(askpop);
    while (*value < 0)
    {
	XNextEvent (XtDisplay (toplevel), &event);
	XtDispatchEvent (&event);
    }
}

