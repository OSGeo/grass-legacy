#include <X11/Intrinsic.h>
#include <Xm/DialogS.h>
#include <Xm/SelectioB.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include <ctype.h>

#define YES 1
#define NO  2

void
XgEventLoop(appContext,flag)
        XtAppContext    appContext;
	int *flag;
{
        while (*flag == 0 || XtPending()) {
            XEvent          event;
            XtAppNextEvent(appContext, &event);
            XtDispatchEvent(&event);
        }
}

Boolean
XgYesNo(parent, question)
    Widget          parent;
    char           *question;
{
    XmString        text, yes, no;
    int             answer = 0;
    extern void     XgYesNoResponse();
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

    dialog = XmCreateQuestionDialog(parent, "XGrass Yes/No", NULL, 0);
    yes = XmStringCreateSimple("Yes");
    no = XmStringCreateSimple("No");
    XtVaSetValues(dialog,
		  XmNdialogTitle,XmStringCreateSimple("XGrass Yes/No"),
                  XmNdialogStyle, XmDIALOG_APPLICATION_MODAL,
                  XmNokLabelString, yes,
                  XmNcancelLabelString, no,
                  NULL);
    XtUnmanageChild(
                    XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
    XtAddCallback(dialog, XmNokCallback, XgYesNoResponse, &answer);
    XtAddCallback(dialog, XmNcancelCallback, XgYesNoResponse, &answer);
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
	XgEventLoop(appContext,&answer);
    }

    return (answer == YES);
}

/*
 * XgYesNoResponse() --The user made some sort of response to the question
 * posed in XgYesNo().  Set the answer (client_data) accordingly and destroy
 * the dialog.
 */
void
XgYesNoResponse(w, answer, reason)
    Widget          w;
    int            *answer;
    XmAnyCallbackStruct *reason;
{
    switch (reason->reason) {
    case XmCR_OK:
        *answer = YES;
        break;
    case XmCR_CANCEL:
        *answer = NO;
        break;
    default:
        return;
    }
}
