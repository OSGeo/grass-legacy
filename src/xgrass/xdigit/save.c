#include <stdio.h>
#include "xgrass_lib.h"
#include "Interact.h"
#include "digit.h"


Widget
make_saveas_pop (parent)

    Widget parent;
{
    Widget inter, sh, form, label, text;
    int i, n, x, y;
    Arg wargs[10];
    Pixel fg, bg;
    XmString str;

    str = XmStringCreateSimple ("save as");

    n = 0;
    XtSetArg (wargs[n], XtNx, 500); n++;
    XtSetArg (wargs[n], XtNy, 400); n++;
    XtSetArg (wargs[n], XmNdialogTitle, str); n++;
    XtSetArg (wargs[n], XmNsaveUnder, True); n++;
    sh = XtCreatePopupShell ("intro", transientShellWidgetClass,
				      parent, wargs, n);
    inter = XgCreateInteractor (sh, "inter", NULL, 0);
    form = XtCreateManagedWidget ("form", xmFormWidgetClass, inter, NULL, 0);
   
    XmStringFree (str);
    str = XmStringCreateSimple("Enter file name:");
    
    n = 0;
    XtSetArg (wargs[n], XmNlabelString, str); n++;
    XtSetArg (wargs[n], XmNheight, 30); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg (wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
    label = XtCreateManagedWidget ("label", xmLabelGadgetClass, form, wargs, n);
    XmStringFree (str);

    n = 0;
    XtSetArg (wargs[n], XmNtextColumns, 20); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, label); n++;
    text = 
    XtCreateManagedWidget ("text", xmTextFieldWidgetClass, form, wargs, n);

    XtAddCallback(inter, XmNokCallback, downcb, sh); 
    XtAddCallback(inter, XmNcancelCallback, downcb, sh); 


    XtManageChild (inter);
    XtUnmanageChild(XgInteractorGetChild(inter, XmINTERACT_HELP_BUTTON));
    
    return sh;
}
