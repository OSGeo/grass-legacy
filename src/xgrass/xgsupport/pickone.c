#include <xgrass_lib.h>
#include <ctype.h>

#define OPT1 1
#define OPT2  2

void
XgPickOneEventLoop(widget, appContext,flag)
        Widget widget;
        XtAppContext    appContext;
	int *flag;
{
        while (*flag == 0 || XtPending()) {
            XEvent          event;
            XtAppNextEvent(appContext, &event);
            XtDispatchEvent(&event);
        }
    XSync(XtDisplay(widget), False);
    while (XtPending()) {
        XEvent          event;
        XtAppNextEvent(appContext, &event);
        XtDispatchEvent(&event);
    }
}

Boolean
XgPickOne(parent, question, opt1, opt2)
    Widget          parent;
    char           *question;
    char *opt1, *opt2;
{
    XmString        text, yes, no;
    int             answer = 0;
    extern void     XgPickOneResponse();
    Widget          dialog;
    char *ptr;

    ptr = question;
    while ( *ptr ) {
        if ( *ptr == '\n' )
            *ptr = '\012';
        else if ( *ptr == '\r' )
            *ptr = '\012';
        else if ( !isprint(*ptr) )
            *ptr = ' ';
        ptr++;
    }

    dialog = XmCreateQuestionDialog(parent, "XGrass Pick One", NULL, 0);
    yes = XmStringCreateSimple(opt1);
    no = XmStringCreateSimple(opt2);
    XtVaSetValues(dialog,
		  XmNdialogTitle,XmStringCreateSimple("XGrass Pick One"),
                  XmNokLabelString, yes,
                  XmNcancelLabelString, no,
                  NULL);
    XtUnmanageChild(
                    XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
    XtAddCallback(dialog, XmNokCallback, XgPickOneResponse, &answer);
    XtAddCallback(dialog, XmNcancelCallback, XgPickOneResponse, &answer);
    text = XmStringCreateLtoR(question,XmSTRING_DEFAULT_CHARSET);
    XtVaSetValues(dialog,
                  XmNmessageString, text,
                  NULL);
    XmStringFree(text);
    XtManageChild(dialog);
    XtPopup(XtParent(dialog), XtGrabNone);

    /*
     * while the user hasn't provided an answer, simulate XtMainLoop. The
     * answer changes as soon as the user selects one of the buttons and the
     * callback routine changes its value.  Don't break loop until
     * XtPending() also returns False to assure widget destruction.
     */
    {
        XtAppContext    appContext;
        appContext = XtWidgetToApplicationContext(parent);
	XgPickOneEventLoop(parent, appContext,&answer);
    }

    return (answer == OPT1);
}

/*
 * XgPickOneResponse() --The user made some sort of response to the question
 * posed in XgPickOne().  Set the answer (client_data) accordingly and destroy
 * the dialog.
 */
void
XgPickOneResponse(w, answer, reason)
    Widget          w;
    int            *answer;
    XmAnyCallbackStruct *reason;
{
    switch (reason->reason) {
    case XmCR_OK:
        *answer = OPT1;
        break;
    case XmCR_CANCEL:
        *answer = OPT2;
        break;
    default:
        return;
    }
}
