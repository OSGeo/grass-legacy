
/* make_label:
** makes a label for the panel which called it
*/

#include "interface.h"

Widget 
make_title (int who, data_cell *dc, int super)
{

    Widget lab, sep;
    Arg wargs[15];
    int n;


    n = 0;
    XtSetArg(wargs[n],XmNtopAttachment,XmATTACH_FORM); n++;
    XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_NONE); n++;
    XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_FORM); n++;
    XtSetArg(wargs[n],XmNrightAttachment,XmATTACH_FORM); n++;
    XtSetArg(wargs[n],XmNlabelString,dc->panels[who].name); n++;
    lab = XtCreateManagedWidget("ptitle", xmLabelGadgetClass, 
		dc->panels[who].form, wargs, n);	

    XtSetArg(wargs[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
    XtSetArg(wargs[n],XmNtopWidget,lab); n++;
    XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_NONE); n++;
    XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_FORM); n++;
    XtSetArg(wargs[n],XmNrightAttachment,XmATTACH_FORM); n++;
    sep = XtCreateManagedWidget("pseper",xmSeparatorWidgetClass, 
		dc->panels[who].form, wargs,n);
    
    return(sep);
}


