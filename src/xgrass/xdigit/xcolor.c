/*
** Written Fall 1992 by Terry Baker
** US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "color.h"




#define NCOLORS 12


static char *names[] = {
    "area", "line", "site", "node1line", "bg", 
    "arealabs",  "labline", "sitelabs",
    "nodeline", "overlay", "hilight" 
    };

static char *strings[] = {
    "areas", "lines", "sites", "nodes with one line", "background",
    "labeled areas",  "labeled lines", "labeled sites", 
    "nodes with 2 or more lines",
    "overlay map", "highlight" 
    };

static char *colornames[] = {
    "yellow", "orange", "brown",
    "red", "magenta", "violet", 
    "blue", "aquamarine", 
    "green", "grey", "white", 
    "black"};

static int icolor[] = { 0,
    XD_YELLOW, XD_ORANGE, XD_BROWN,
    XD_RED, XD_MAGENTA, XD_VIOLET, 
    XD_BLUE, XD_AQUA, 
    XD_GREEN, XD_GREY, XD_WHITE, 
    XD_BLACK};

static int defaults[] = {
    XD_GREY, XD_BLUE, XD_GREEN, XD_GREEN,
    XD_BLACK, XD_ORANGE, XD_MAGENTA, XD_AQUA,
    XD_RED, XD_WHITE, XD_YELLOW};


static int icurr = 1;
static int change = 0;

show_changes()
{
    if (change)
	redraw();
    change = 0;
}
Widget
make_colors_menu (parent)

    Widget parent;
{
    Widget tmp, colorform, sh, done, ok, reset, label, frame;
    static Widget btns[11], labels[11]; 
    int i, n, x, y, tmpx;
    Arg wargs[10];
    Pixel fg, bg;
    int colorid[12];
    Display *dpy = XtDisplay (parent);
    int     scr = DefaultScreen (dpy);
    Widget   currcolor; 

    tmpx = Wdth * 0.7 +Winx;
    init_colors();
    alloc_colors (parent, colornames);
    init_graphics();

    n = 0;
    XtSetArg (wargs[n], XtNx, tmpx); n++;
    XtSetArg (wargs[n], XtNy, Winy); n++;
    XtSetArg (wargs[n], XmNsaveUnder, True); n++;
    sh = XtCreatePopupShell ("colors", transientShellWidgetClass,
                                            parent, wargs, n);
    n = 0;
    XtSetArg (wargs[n], XmNwidth, 230); n++;
    XtSetArg (wargs[n], XmNheight, 450); n++;
    colorform = XtCreateManagedWidget ("colorform", xmFormWidgetClass,
                                            sh, wargs, n);

    x = 32;
    y = 1;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtCreateManagedWidget ("Set Colors", xmLabelGadgetClass,
                                            colorform, wargs, n);

    n = 0;
    XtSetArg (wargs[n], XmNforeground, &fg); n++;
    XtSetArg (wargs[n], XmNbackground, &bg); n++;
    XtGetValues (parent, wargs, n);
    x = 10;
    y = 5;
    for (i = 0; i < 11; i++, y += 10)
    {  
	if (i == 5)  /* start second column of buttons */
	{
	    x = 60;
	    y = 5;
	}
	if (i == 10)
	    x = 10;
        n = 0;
        XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
        XtSetArg (wargs[n], XmNleftPosition, x); n++;
        XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
        XtSetArg (wargs[n], XmNtopPosition, y); n++;
        XtSetArg (wargs[n], XmNlabelType, XmPIXMAP); n++;
        XtSetArg (wargs[n], XmNlabelPixmap,
                XmGetPixmap (XtScreen(parent), names[i], 
			     fg, bg)); n++;
        btns[i] =
        XtCreateManagedWidget (names[i], xmPushButtonWidgetClass, colorform,
                                                                wargs, n);
        n = 0;
        XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
        XtSetArg (wargs[n], XmNleftWidget, btns[i]); n++;
        XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
        XtSetArg (wargs[n], XmNtopPosition, y + 3); n++;
        XtSetArg (wargs[n], XmNbackground, dcolors[defaults[i]]); n++;
        labels[i] = 
	     XtCreateManagedWidget (" ", xmLabelWidgetClass, colorform, wargs, n);
        XtAddCallback (btns[i], XmNactivateCallback, toggle_color, labels[i]);
        XtAddCallback (btns[i], XmNarmCallback, new_button_color, 
								  strings[i]);
    }
    XtAddCallback (btns[0], XmNactivateCallback, change_color_value, &CLR_AREA);
    XtAddCallback (btns[1], XmNactivateCallback, change_color_value, &CLR_LINE);
    XtAddCallback (btns[2], XmNactivateCallback, change_color_value, &CLR_SITE);
    XtAddCallback (btns[3], XmNactivateCallback, change_color_value, 
								 &CLR_1_NODE);
    XtAddCallback (btns[4], XmNactivateCallback, change_color_value, 
								  &CLR_ERASE);
    XtAddCallback (btns[5], XmNactivateCallback, change_color_value, 
								  &CLR_ALABEL);
    XtAddCallback (btns[6], XmNactivateCallback, change_color_value, 
								   &CLR_LLINE);
    XtAddCallback (btns[7], XmNactivateCallback, change_color_value, &CLR_LSITE);
    XtAddCallback (btns[8], XmNactivateCallback, change_color_value, 
		                                                   &CLR_2_NODE);
    XtAddCallback (btns[9], XmNactivateCallback, change_color_value, 
								  &CLR_OVERLAY);
    XtAddCallback (btns[10], XmNactivateCallback, change_color_value, 
							       &CLR_HIGHLIGHT);
    y += 5;
    x = 30;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x -5); n++;
    XtSetArg (wargs[n], XmNrightAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNrightPosition, x+45); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNalignment, XmALIGNMENT_CENTER); n++;
    XtCreateManagedWidget ("Current Color", xmLabelGadgetClass,
                                            colorform, wargs, n);
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x+10); n++;
    XtSetArg (wargs[n], XmNrightAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNrightPosition, x+30); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y + 5); n++;
    XtSetArg (wargs[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNbottomPosition, y + 10); n++;
    XtSetArg (wargs[n], XmNbackground, dcolors[1]); n++;
    currcolor = XtCreateManagedWidget ("   ", xmLabelWidgetClass, colorform, wargs, n);
    
    x = 10;
    y = 82;
    for (i = 0; i < 12; i++, x += 8)
    {
	colorid[i] = i;
        n = 0;
        XtSetArg (wargs[n], XtNheight, 16); n++;
        XtSetArg (wargs[n], XtNwidth, 16); n++;
        XtSetArg (wargs[n], XmNrightAttachment, XmATTACH_POSITION); n++;
        XtSetArg (wargs[n], XmNrightPosition, x); n++;
        XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
        XtSetArg (wargs[n], XmNtopPosition, y); n++;
        XtSetArg (wargs[n], XmNbackground, dcolors[i+1]); n++;
        tmp = XtCreateManagedWidget ("", xmPushButtonWidgetClass,
                                            colorform, wargs, n);
	XtAddCallback (tmp, XmNactivateCallback, change_curr_color, currcolor);
	XtAddCallback (tmp, XmNarmCallback, write_curr_color, colorid[i]);
    }
    y = 88;
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg (wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtCreateManagedWidget ("", xmSeparatorGadgetClass, colorform, wargs, n);
	
   
    x = 15;
    y = 90;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    reset = XtCreateManagedWidget ("Reset Defaults", xmPushButtonWidgetClass,
                                                             colorform, wargs, n);
    XtAddCallback (reset, XmNactivateCallback, Reset, labels);
    XtAddCallback (reset, XmNarmCallback, showtext, "Reset colors to defaults");
    x = 65;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNwidth, 45); n++;
    done = XtCreateManagedWidget ("Done", xmPushButtonGadgetClass, colorform, wargs, n);
    XtAddCallback (done, XmNactivateCallback, downcb, sh);
    XtAddCallback (done, XmNactivateCallback, show_changes, NULL);

    return sh;
}


void
alloc_colors (parent, colornames)
    Widget parent;
    char   *colornames[];
{
    Display   *dpy;
    int       screen, i;
    Colormap  cmap;
    XColor    dbcolor;
    XColor    color;
    char buf[100];

    dpy = XtDisplay (parent);
    screen = DefaultScreen (dpy);
    cmap = DefaultColormap (dpy, screen);

    for (i = 0; i < NCOLORS; i++)
    {
	if(!XAllocNamedColor(dpy, cmap, colornames[i], &color, &dbcolor))
	{
	    if (!strcmp (colornames[i], "black"))
	        dcolors[i+1] = BlackPixel(dpy, screen);
	    else
	    {
	        dcolors[i+1] = WhitePixel(dpy, screen);
	        sprintf (buf,
	            "Cannot allocate %s.  Using white instead", colornames[i]);
	    	make_monolog (1, buf);
	    }
	 }
	 else
	    dcolors[i+1] = color.pixel;
    }

    icurr = 1;
}

void 
toggle_color (w, label, call_data)
    Widget w;
    Widget label;
    caddr_t *call_data;
{
    Arg   wargs;

    
    XtSetArg (wargs, XmNbackground, dcolors[icurr]); 
    XtSetValues (label, &wargs, 1);

}
void 
change_color_value(w, element, call_data)
    Widget w;
    char  *element;
    caddr_t *call_data;
{
    *element = icolor[icurr];
    if (*element != CLR_OVERLAY || Disp_overlay)
        change = 1;
}


void 
new_button_color (w, name, call_data)
    Widget w;
    char *name;
    caddr_t *call_data;
{
    char str[80];

    sprintf (str,"Change color of %s to %s", name, colornames[icurr - 1]);
    showtext (w, str, NULL);
}

void 
write_curr_color (w, curr)
    Widget w;
    int    curr;
{
    char str[80];
    
    icurr = curr + 1;
    sprintf (str,"Change current color to %s", colornames[curr]);
    showtext (w, str, NULL); 
}

void 
change_curr_color (w, currcolor)
    Widget w, currcolor;
{
    Arg warg;
    Pixel current;

    XtSetArg (warg, XmNbackground, &current);
    XtGetValues (w, &warg, 1); 
    XtSetArg (warg, XmNbackground, current);
    XtSetValues (currcolor, &warg, 1); 
}

void
Reset (w, labels, call_data)
    Widget w;
    Widget labels[];
    caddr_t *call_data;

{
    int i;

    init_colors();
    for (i = 0; i < 11; i++)
    {
        XtVaSetValues (labels[i],
               XmNbackground, dcolors[defaults[i]], NULL);
    }
    change = 1;
	
}
