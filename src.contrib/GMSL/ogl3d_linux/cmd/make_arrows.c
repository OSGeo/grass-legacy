

/* make_arrows:  make two arrows, txt, with a label & defined action
    Returns a form widget containing uparrow & downarrow on left,
    label to right of uparrow, text to right of downarrow
*/

#include "interface.h"

Widget 
make_arrows (Widget parent, char *label, int min, int max, int def, arrow_pair *ap, Arg *wargs, int *np)
{
Widget aform;
char str[100];
int n;

    n = *np;
    ap->min = min;
    ap->max = max;
    ap->val = def;

    strcpy(str, label);
    strcat(str, " form");
    aform = XtCreateManagedWidget(str, xmFormWidgetClass, parent, wargs,n);
    
    strcpy(str, label);
    strcat(str, " up");
    n = 0;
    XtSetArg(wargs[n],XmNwidth, 25); n++;
    SetPositionArgs(wargs, &n, 2, 48, 4, -1, XmATTACH_NONE);
    ap->up = XtCreateManagedWidget(str, xmArrowButtonWidgetClass, 
	    aform, wargs, n);	
    XtAddCallback(ap->up, XmNactivateCallback, update_arrows,ap);	

    strcpy(str, label);
    strcat(str, " down");
    n = 0;
    XtSetArg(wargs[n],XmNwidth, 25); n++;
    SetPositionArgs(wargs, &n, 52, 98, 4, -1, XmATTACH_NONE);
    XtSetArg(wargs[n],XmNarrowDirection, XmARROW_DOWN); n++;
    ap->down = XtCreateManagedWidget(str, xmArrowButtonWidgetClass, 
	    aform, wargs, n);	
    XtAddCallback(ap->down, XmNactivateCallback, update_arrows,ap);	
    
    n = 0;
    XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(wargs[n], XmNleftWidget, ap->up); n++;
    XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
    XtSetArg(wargs[n], XmNbottomPosition, 48); n++;
    XtCreateManagedWidget(label, xmLabelGadgetClass, 
	    aform, wargs, n);
    
    sprintf(str, "%d", def);
    n = 0;
    XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(wargs[n], XmNleftWidget, ap->down); n++;
    XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg(wargs[n], XmNtopPosition, 52); n++;
    ap->txt = XtCreateManagedWidget(str, xmLabelWidgetClass, 
	    aform, wargs, n);	


    return (aform);

}

