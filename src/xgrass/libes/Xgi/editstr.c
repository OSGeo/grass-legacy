static char rcsid[] = "@(#)XGRASS $Id: editstr.c,v 0.0 1992/05/05 14:56:17 sink Exp sink $";
/*
 * File:
 *
 * Desc:
 *
 * Auth:
 *
 * Date:
 *
 * Modification History:
 *
 *
 */

#include "xgrass_lib.h"

static char *EditStringInitial;
static char *EditStringResult;
static Boolean EditStringDone;

void EditStringOk(w,cld,cad)
Widget w;
XtPointer cld, cad;
{
    Widget dialog = (Widget)cld;
    Widget text;

    text = XmSelectionBoxGetChild(dialog, XmDIALOG_TEXT);

    EditStringResult = XmTextGetString(text);
    EditStringDone = True;
}

static void EditStringCancel(w,cld,cad)
Widget w;
XtPointer cld, cad;
{
    EditStringDone = True;
}

static void EditStringReset(w,cld,cad)
Widget w;
XtPointer cld, cad;
{
    Widget dialog = (Widget)cld;
    Widget text;

    text = XmSelectionBoxGetChild(dialog, XmDIALOG_TEXT);

    XmTextSetString(text, EditStringInitial);
}

#ifdef _NO_PROTO
char *
XgEditStringDialog(appContext, parent, thestring, theprompt)
    XtAppContext appContext;
    Widget       parent;
    char         *thestring;
    char         *theprompt;
#else
char *
XgEditStringDialog(
    XtAppContext appContext,
    Widget parent,
    char *thestring,
    char *theprompt)
#endif
{
    Widget dialog;
    Widget child;
    Arg args[4];
    XmString xmstring;

    EditStringDone = False;
    EditStringResult = EditStringInitial = _XgStrDup(thestring);

    xmstring = XmStringCreateSimple(thestring);
    XtSetArg(args[0], XmNautoUnmanage, False);
    XtSetArg(args[1], XmNdialogStyle, XmDIALOG_APPLICATION_MODAL);
    XtSetArg(args[2], XmNtextString, xmstring);
    dialog = XmCreatePromptDialog(parent, "String Editor", args, 3);
    XmStringFree(xmstring);

    XtVaSetValues(dialog,
        XmNdialogTitle,XmStringCreateSimple("String Editor"), 
        NULL);

    child = XmSelectionBoxGetChild(dialog, XmDIALOG_OK_BUTTON);
    XtAddCallback(child, XmNactivateCallback, EditStringOk, 
	(XtPointer)dialog);

    child = XmSelectionBoxGetChild(dialog, XmDIALOG_HELP_BUTTON);
    xmstring = XmStringCreateSimple("Reset");
    XtVaSetValues(child, XmNlabelString, xmstring, NULL);
    XmStringFree(xmstring);
    XtAddCallback(child, XmNactivateCallback, EditStringReset, 
	(XtPointer)dialog);

    child = XmSelectionBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON);
    XtAddCallback(child, XmNactivateCallback, EditStringCancel, NULL);

    if ( XmIsMotifWMRunning(parent) ) {
        unsigned int decor_flags, func_flags;

        decor_flags = MWM_DECOR_BORDER ;
        decor_flags |= MWM_DECOR_TITLE | MWM_DECOR_MENU;
        decor_flags |= MWM_DECOR_MINIMIZE;

        func_flags = MWM_FUNC_MOVE | MWM_FUNC_MINIMIZE;

        XtVaSetValues(XtParent(dialog),
            XmNmwmDecorations, decor_flags,
            XmNmwmFunctions, func_flags,
            NULL);
    }

    XtManageChild(dialog);

    /*
    XtRealizeWidget(dialog);
    */
   
    {
        XEvent                          e;
        while (!EditStringDone) {
            XtAppNextEvent(appContext, &e);
            XtDispatchEvent(&e);
        }
    }

    XtUnmanageChild(dialog);

    return EditStringResult;
}

