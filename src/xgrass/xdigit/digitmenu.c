#include <stdio.h>
#include "digit.h"

Widget dig, mode, neat, reg;
Widget digdev, pntdev, windev;
Widget numlines, numareas, numsites, numpoints;

make_digit_menu (parent)
    Widget parent;
{
    Widget type, begin, autolab;
    Widget frame, amt, lines, areas, sites, total;
    int    x, y;
    int    n = 0;
    Arg    wargs[10];
    Pixel  fg, bg;
    char   digmap[10];


    x = 15;
    y = 5;

    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y+5); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    autolab = XtCreateManagedWidget ("Auto\nLabel", xmToggleButtonWidgetClass, 
                                          parent, wargs, n);
    XtAddCallback (autolab, XmNarmCallback, showtext, "auto labeling");
    XtAddCallback (autolab, XmNvalueChangedCallback, autolabel, NULL); 

   
    n = 0;
    XtSetArg (wargs[n], XtNforeground, &fg); n++;
    XtSetArg (wargs[n], XtNbackground, &bg); n++;
    XtGetValues (parent, wargs, n);

    x += 10;
    dig =
    make_button (parent, "", digdevcb, digdev, "digitizer",  fg, bg, 
				   "Change digitizing device");
    add_inverse_map (dig, "mouse", fg, bg);
    
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (dig, wargs, n); 
    
    XtAddCallback (digdev, XmNactivateCallback, digdevcb, dig);
    XtSetSensitive(dig, Dig_Enabled);

    n = 0;
    XtSetArg (wargs[n], XmNmarginHeight, 0); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, dig); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtCreateManagedWidget ("digitizer", xmLabelGadgetClass, parent, 
								wargs, n);
    x +=10;

    mode =
    make_button (parent, "", NULL, NULL, "points", fg, bg, 
				 "Change digitizing mode");
    XtAddCallback (mode, XmNactivateCallback, Digitize, MDC_MODE);

    add_inverse_map (mode,  "points", fg, bg);
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (mode, wargs, n); 
  
    n = 0;
    XtSetArg (wargs[n], XmNmarginHeight, 0); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, mode); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x+1); n++;
    XtCreateManagedWidget ("mode", xmLabelGadgetClass, parent, 
								wargs, n);
    type =
    make_button (parent, "", typecb, parent, "area", fg, bg, 
				"Change element type" );
    
    x += 10;
    n = 0;
    XtSetArg (wargs[n], XmNmarginHeight, 0); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (type, wargs, n); 
   
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, type); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x + 1); n++;
    XtCreateManagedWidget ("type", xmLabelGadgetClass, parent, 
								wargs, n);
    begin = make_button (parent, "Begin Digitizing", NULL, NULL,
	    NULL, fg, bg, "Begin digitizing");
    XtAddCallback (begin, XmNactivateCallback, Digitize, MDC_DIGIT);
    
    n = 0;
    x +=10;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y + 5); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetValues (begin, wargs, n);
    
    x += 20;
    y = 1;
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNrightAttachment, XmATTACH_FORM); n++; 
    frame = XtCreateManagedWidget ("frame", xmFrameWidgetClass,
                                                      parent, wargs, n);
   amt = XtCreateManagedWidget ("amount digitized", xmLabelWidgetClass,
                                                      frame, NULL, 0);
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, frame); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    lines = XtCreateManagedWidget ("lines:", xmLabelWidgetClass,
                                                      parent, wargs, n);
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, frame); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNleftWidget, lines); n++;
    numlines = XtCreateManagedWidget ("0", xmLabelWidgetClass,
                                                      parent, wargs, n);
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, lines); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    areas = XtCreateManagedWidget ("areas:", xmLabelWidgetClass,
                                                      parent, wargs, n);
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, numlines); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNleftWidget, areas); n++;
    numareas = XtCreateManagedWidget ("0", xmLabelWidgetClass,
                                                      parent, wargs, n);
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, frame); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x+10); n++;
    sites= XtCreateManagedWidget ("sites:", xmLabelWidgetClass,
                                                      parent, wargs, n);
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, frame); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNleftWidget, sites); n++;
    numsites= XtCreateManagedWidget ("0", xmLabelWidgetClass,
                                                      parent, wargs, n);
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, sites); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x+10); n++;
    total = XtCreateManagedWidget ("total points:", xmLabelWidgetClass,
                                                      parent, wargs, n);
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, numsites); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNleftWidget, total); n++;
    numpoints = XtCreateManagedWidget ("0", xmLabelWidgetClass,
                                                      parent, wargs, n);


}
autolabel (w)
    Widget w;
{
    XEvent event;

    if (XmToggleButtonGetState (w))
        Digitize (w, MDC_LABEL);
    else
        Digitize (w, MDC_UNLABEL);

}





void
modecb()
{
    static int i = 0;
    char str[50];

    static char *name[] = { 
	"points",
	"stream"
	};
    
    i = (i+1)%2;
    
    sprintf (str, "Change digitizing mode to %s", name[i]);
    showtext (mode, str, NULL);
    
    change_pix (mode, name[i], mode); 

}

void
typecb(w, parent, call_data)
    Widget w;
    Widget parent;
    caddr_t call_data;
{
    static int i = 1;
    char   str[40];

    static char *name[] = {
        "line",
        "area",
        "site",
	};
    
    i = (i+1)%3;
    sprintf (str,"Change element type to %s", name[i]);
    showtext (w, str, NULL);
    Digitize (w, MDC_TYPE);

    change_pix (w, name[i], parent); 

}
void
autolabcb(w, parent, call_data)
    Widget w;
    Widget parent;
    caddr_t call_data;
{
    int i;
    Arg wargs;



    XtSetArg (wargs,XmNset, &i);
    XtGetValues (w, &wargs, 1);
    if (i)
	showtext (w, "Turn auto labeling off", NULL);
    else
        showtext (w, "Turn auto labeling on", NULL);
    Digitize (w, MDC_LABEL);

}
void 
dig_on_off()
{
    char digmap[10];

    
    XtSetSensitive (digdev, Dig_Enabled);
    XtSetSensitive (pntdev, Dig_Enabled);
    XtSetSensitive (windev, Dig_Enabled);
    XtSetSensitive (dig, Dig_Enabled);
    XtSetSensitive (mode, Dig_Enabled);
    XtSetSensitive (neat, Dig_Enabled);
    XtSetSensitive (reg, Dig_Enabled);
    Digitize (0, MDC_MODE);


}


write_type_info ()
{
    char buf[100];
    XmString num;

    sprintf (buf, "%d", CM->n_llines);
    num = XmStringCreateSimple (buf);
    XtVaSetValues (numlines, XmNlabelString, num, NULL);
    XtFree (num);

    sprintf (buf, "%d", CM->n_alines);
    num = XmStringCreateSimple (buf);
    XtVaSetValues (numareas, XmNlabelString, num, NULL);
    XtFree (num);

    sprintf (buf, "%d", CM->n_plines);
    num = XmStringCreateSimple (buf);
    XtVaSetValues (numsites, XmNlabelString, num, NULL);
    XtFree (num);

    sprintf (buf, "%d", CM->n_points);
    num = XmStringCreateSimple (buf);
    XtVaSetValues (numpoints, XmNlabelString, num, NULL);
    XtFree (num);
}
