#include "digit.h"

int displ_defs[] = {0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0};
static int change = 0;

void 
make_changes ()
{
   if (change)
       redraw();
   change = 0;

}

void
displaycb(w, name, call_data)
    Widget w;
    char *name;
    caddr_t call_data;
{
    char str[50];

    if (!XmToggleButtonGetState (w))
        sprintf (str, "Display %s ", name);
    else
        sprintf (str, "Do not display  %s" , name);
    showtext (w, str, NULL);
    change = 1;
}

void
Reset2(w, toggles, call_data)
    Widget w;
    Widget toggles[];
    caddr_t call_data;
{
    int i;
    Arg wargs[2];

    showtext (w, "Reset Display Options to defaults", NULL);
    for (i = 0; i < 12; i++)
    {
        XtSetArg (wargs[0], XmNset, displ_defs[i]);
        XtSetValues (toggles[i], wargs, 1);
    }
    change = 1;
    set_default_display();
}
void
togglecb(w, name, call_data)
    Widget w;
    char *name;
    caddr_t call_data;
{
    char str[50];

    if (!XmToggleButtonGetState (w))
        sprintf (str, "Turn %s on", name);
    else
        sprintf (str, "Turn %s off", name);
    showtext (w, str, NULL);
}
void
toggle (w, item)
    Widget w;
    char    *item;
{
    *item = (*item )? 0:1;
}

Widget
make_options_menu (parent)
    
    Widget parent;
{
    Widget opform, sh, done,  reset, label, frame;
    static Widget toggles[12]; 
    int i, n, x, y, tmpx;
    Arg wargs[10];
    Pixel fg, bg;

    static char *names[] = {
	"arealabs", "linelabs", "sitelabs",
	"markers",  "area",    "labline",
	"line",    "site",    "node",
	"points"   
	};
    static char *strings[] = {
	"area labels", "line labels", "site labels",
	"area markers",  "areas",    "labeled lines",
	"lines",    "sites",    "nodes",
	"points in lines"   
	};
   tmpx = Wdth * 0.6 +Winx;

    n = 0;
    XtSetArg (wargs[n], XtNx, tmpx); n++;
    XtSetArg (wargs[n], XtNy, Winy); n++;
    XtSetArg (wargs[n], XmNsaveUnder, True); n++;
    sh = XtCreatePopupShell ("options", transientShellWidgetClass,
					    parent, wargs, n);
    n = 0;
    XtSetArg (wargs[n], XmNwidth, 230); n++;
    XtSetArg (wargs[n], XmNheight, 350); n++;
    opform = XtCreateManagedWidget ("opform", xmFormWidgetClass,
					    sh, wargs, n);
	
    x = 30;
    y = 1;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtCreateManagedWidget ("Display Options", xmLabelGadgetClass,
					    opform, wargs, n);
    n = 0;
    XtSetArg (wargs[n], XtNforeground, &fg); n++;
    XtSetArg (wargs[n], XtNbackground, &bg); n++;
    XtGetValues (opform, wargs, n);


    x = 10;
    y = 10;
    for (i = 0; i < 10; i++, y += 12)    
    {
        if (i == 5)
	{
            x = 60;
    	    y = 10;
	}
	n = 0;
	XtSetArg (wargs[n], XmNset, displ_defs[i]); n++;
        XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
        XtSetArg (wargs[n], XmNleftPosition, x); n++;
        XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
        XtSetArg (wargs[n], XmNtopPosition, y); n++;
        XtSetArg (wargs[n], XmNlabelType, XmPIXMAP); n++;
        XtSetArg (wargs[n], XmNlabelPixmap, 
                XmGetPixmap (XtScreen(parent), names[i], fg, bg));  n++;
        toggles[i] =
	XtCreateManagedWidget (names[i], xmToggleButtonGadgetClass, opform, 
								wargs, n);
        XtAddCallback (toggles[i], XmNarmCallback, displaycb, strings[i]);
    }
   
    XtAddCallback (toggles[0], XmNarmCallback, toggle, &Disp_labels);
    XtAddCallback (toggles[1], XmNarmCallback, toggle, &Disp_llabels);
    XtAddCallback (toggles[2], XmNarmCallback, toggle, &Disp_slabels);
    XtAddCallback (toggles[3], XmNarmCallback, toggle, &Disp_markers);
    XtAddCallback (toggles[4], XmNarmCallback, toggle, &Disp_outline);
    XtAddCallback (toggles[5], XmNarmCallback, toggle, &Disp_llines);
    XtAddCallback (toggles[6], XmNarmCallback, toggle, &Disp_lines);
    XtAddCallback (toggles[7], XmNarmCallback, toggle, &Disp_sites);
    XtAddCallback (toggles[8], XmNarmCallback, toggle, &Disp_nodes);
    XtAddCallback (toggles[9], XmNarmCallback, toggle, &Disp_points);
    
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg (wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtCreateManagedWidget ("", xmSeparatorGadgetClass, opform, wargs, n);

    y += 5;
    x = 10; 
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNlabelType, XmPIXMAP); n++;
    XtSetArg (wargs[n], XmNlabelPixmap, 
    		XmGetPixmap (XtScreen(parent), "beep", fg, bg) ); n++;
    toggles[i] = 
        XtCreateManagedWidget ("beep", xmToggleButtonGadgetClass, opform, 
								wargs, n);
    XtAddCallback (toggles[i], XmNarmCallback, togglecb, "beep");
    XtAddCallback (toggles[i], XmNarmCallback, toggle, &Beep_On);
   
    i++;
    x = 60;
    toggles[i]  = 
	XtVaCreateManagedWidget ("Auto\nWindow", xmToggleButtonGadgetClass, 
				 opform, 
				    XmNtopAttachment, XmATTACH_POSITION,
				    XmNtopPosition, y - 1,
				    XmNleftAttachment, XmATTACH_POSITION,
				    XmNleftPosition, x,
				    XmNindicatorSize, 15,
				    NULL);
    XtAddCallback (toggles[i], XmNarmCallback, togglecb, "auto windowing");
    XtAddCallback (toggles[i], XmNarmCallback, toggle, &Auto_Window);

    y = 88;
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg (wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtCreateManagedWidget ("", xmSeparatorGadgetClass, opform, wargs, n);
   
   x = 10;
    y = 90;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    reset = XtCreateManagedWidget ("Reset Defaults", xmPushButtonGadgetClass, 
							     opform, wargs, n);
    XtAddCallback (reset, XmNarmCallback, Reset2, toggles);
    
    x = 60;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNwidth, 45); n++;
    done = XtCreateManagedWidget ("Done", xmPushButtonGadgetClass, opform, wargs, n);
    XtAddCallback (done, XmNactivateCallback, downcb, sh);
    XtAddCallback (done, XmNactivateCallback, make_changes, NULL);

    return sh;
}

