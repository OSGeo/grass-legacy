#include "digit.h"
#include "Browser.h"
#include "Interact.h"
#include "xgrass_lib.h"
#include "gis.h"

extern Widget digdev, windev, pntdev;
static Widget snapth, digth;


make_custom_menu (parent, top)
    Widget parent, top;
{
    Widget opmenu, tmp;
    Widget options, colors, over, back, beep;
    Widget dig, cmenu;
    int    x, y, tmpx;
    int    n = 0;
    Arg    wargs[10];
    Pixel  fg, bg;




    n = 0;
    XtSetArg (wargs[n], XtNforeground, &fg); n++;
    XtSetArg (wargs[n], XtNbackground, &bg); n++;
    XtGetValues (parent, wargs, n);

    x = 20;
    y = 5;
 
    /* pushbutton to change digitizing device */
    digdev =
    make_button(parent, "digitizer", NULL, NULL, "digitizer", fg, bg, 
						  "Change digitizing device");
    add_inverse_map (digdev, "mouse", fg, bg);
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (digdev, wargs, n); 
    XtSetSensitive(digdev, Dig_Enabled);
  
  /* label for button */ 
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, digdev); n++;
    XtSetArg (wargs[n], XmNmarginHeight, 0); n++;
    XtCreateManagedWidget ("digitize", xmLabelGadgetClass, parent, 
								wargs, n);
    /* pushbutton to change windowing device */
    x += 8;
    windev =
    make_button(parent, "windowing", windevcb, parent, "digitizer", fg, bg, 
						"Change windowing device");
    add_inverse_map (windev, "mouse", fg, bg);
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (windev, wargs, n); 
    XtSetSensitive(windev, Dig_Enabled);
  
  /* label for button */ 
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, windev); n++;
    XtSetArg (wargs[n], XmNmarginHeight, 0); n++;
    XtCreateManagedWidget ("window", xmLabelGadgetClass, parent, wargs, n);

    /* pushbutton to change pointing device */
    x += 8;
    pntdev =
    make_button(parent, "pointing", pntdevcb, parent, "digitizer", fg, bg, 
					      "Change pointing device");
    add_inverse_map (pntdev, "mouse", fg, bg);
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (pntdev, wargs, n); 
    XtSetSensitive(pntdev, Dig_Enabled);

    /* label for button */ 
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, pntdev); n++;
    XtSetArg (wargs[n], XmNmarginHeight, 0); n++;
    XtCreateManagedWidget ("point", xmLabelGadgetClass, parent, 
								wargs, n);

    x += 8;
    over =
    make_button(parent, "overlay", NULL, parent, "overlay", fg, bg,
						 "Select overlay vector map");
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (over, wargs, n); 

    XtAddCallback (over, XmNactivateCallback, make_vbrowse, NULL); 
   /* 
    x += 8;
    back =
    make_button(parent, "backdrop", NULL, parent, "backdrop", fg, bg,
						 "Select backdrop raster map");
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (back, wargs, n); 
    XtAddCallback (back, XmNactivateCallback, make_rbrowse, NULL); 
    */
    
    x += 8;
    options =
    make_button(parent, "options", NULL, "", "options", fg, bg,
						 "Display Options menu");
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (options, wargs, n); 

    opmenu = make_options_menu (parent);
    XtAddCallback (options, XmNactivateCallback, showcb, opmenu); 


    x += 8;
    colors=
    make_button(parent, "colors", NULL, parent, "colors", fg, bg,
						 "Display Colors menu");
    cmenu = make_colors_menu (parent);
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (colors, wargs, n); 
    XtAddCallback (colors, XmNactivateCallback, showcb, cmenu); 

/* editable txt widget for snapping threshhold */
    x += 15;
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNcolumns, 5); n++;
    snapth = XtCreateManagedWidget ("snapthresh", xmTextFieldWidgetClass, 
					     parent, wargs, n);
    XtAddCallback (snapth, XmNactivateCallback, threshcb, 
					      "snapping");
    XtAddCallback (snapth, XmNactivateCallback, reset_snap_thresh, NULL);
    show_snapth();
/* label for above widget */
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, snapth); n++;
    XtSetArg (wargs[n], XmNmarginHeight, 0); n++;
    tmp = XtCreateManagedWidget ("snapping", xmLabelGadgetClass, parent, 
                                                                    wargs, n);
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, tmp); n++;
    XtSetArg (wargs[n], XmNmarginHeight, 0); n++;
    XtCreateManagedWidget ("threshold", xmLabelGadgetClass, parent, 
                                                                    wargs, n);
 
/* editable txt widget for digitizing threshhold */

    x += 9;
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNcolumns, 5); n++;
    digth = XtCreateManagedWidget ("digthresh", xmTextFieldWidgetClass, parent,
                                                         wargs, n);
    XtAddCallback (digth, XmNactivateCallback, threshcb, "digitizing");
    XtAddCallback (digth, XmNactivateCallback, digthreshcb, NULL);

    show_digth();
/* label for above widget */
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, digth); n++;
    XtSetArg (wargs[n], XmNmarginHeight, 0); n++;
    tmp = XtCreateManagedWidget ("digitizing", xmLabelGadgetClass, parent, 
                                                                    wargs, n);
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, tmp); n++;
    XtSetArg (wargs[n], XmNmarginHeight, 0); n++;
    XtCreateManagedWidget ("threshold", xmLabelGadgetClass, parent, 
                                                                    wargs, n);
}


void
digdevcb(w, w2, call_data)
    Widget w;
    Widget w2;
    caddr_t call_data;
{
    static int i = 0;
    char str[50];
    extern Widget mode;

    static char *name[] = {
	"digitizer",
	"mouse"
	};
    
    i = (i+1)%2;
    sprintf (str, "Change digitizing device to %s", name[i]);
    showtext (w, str, NULL);
        change_pix (w, name[i], w); 
    if (w2 != NULL)
        change_pix (w2, name[i], w2); 
    TOGGLE (Digtiz_Device);
    XtSetSensitive (mode, (!i));

}

void
threshcb(w, type, call_data)
    Widget w;
    char *type;
    caddr_t call_data;
{
    char *tmp, str[50];

    tmp = XmTextGetString (w);
    sprintf (str, "Change %s threshold to %s", type, tmp);

    XtFree (tmp);
    showtext (w, str, NULL);

}



void
textcb(w, name, call_data)
    Widget w;
    char *name;
    caddr_t call_data;
{
    char str[50], *s;

    
    sprintf (str, "Change %s to %s", name, XmTextGetString(w));
    showtext (w, str, NULL);
}

void
windevcb(w, parent, call_data)
    Widget w;
    Widget parent;
    caddr_t call_data;
{
    static int i = 0;
    char  str[50];


    static char *name[] = {
	"digitizer",
	"mouse"
	};
    
    i = (i+1)%2;
    sprintf (str, "Change windowing device to %s", name[i]);
    showtext (w, str, NULL);
    change_pix (w, name[i], parent); 
    TOGGLE (Window_Device);
}

void
pntdevcb(w, parent, call_data)
    Widget w;
    Widget parent;
    caddr_t call_data;
{
    static int i = 0;
    char   str[50];

    static char *name[] = {
	"digitizer",
	"mouse"
	};
    
    i = (i+1)%2;
    sprintf (str, "Change pointing device to %s", name[i]);
    showtext (w, str, NULL);
    change_pix (w, name[i], parent); 
    TOGGLE (Point_Device);
}
void 
downcb (w, w2, call_data)
    Widget w, w2;
    caddr_t *call_data;
{
    XtUnmanageChild (w2);
}

void 
showcb (w, w2, call_data)
    Widget w, w2;
    caddr_t *call_data;
{
    if (XtIsManaged (w2))
	XtUnmanageChild (w2);
    else
       XtManageChild ( w2);
}


void
OkCallback(w, fields)
Widget w, *fields;
{
  char *s;
  XmString result;

  XtVaGetValues(w,XmNresultString,&result,NULL);
  XmStringGetLtoR(result,XmSTRING_DEFAULT_CHARSET,&s);
  printf("%s\n",s);
  XFlush(XtDisplay(w));
}

char *
get_browser_string(w)
Widget w;
{
    char *s;
    XmString result;
    XtVaGetValues(w,XmNresultString,&result,NULL);
    XmStringGetLtoR(result,XmSTRING_DEFAULT_CHARSET,&s);
    return s;
}
void 
make_vbrowse(w)
    Widget w;
{
    Widget vbrowse;

    vbrowse = make_browser_popup (w, "overlay", XG_VECTOR, 
				 ask_overlay, 0, no_overlay, 0, FALSE);
				 /*
    XtManageChild (XtParent (vbrowse));
    */

}
void 
make_rbrowse(w)
    Widget w;
{
    Widget rbrowse;

    rbrowse = make_browser_popup (w, "backdrop", XG_RASTER, 
				    ask_backdrop, 0, no_backdrop, 0, FALSE);
}

Widget
make_browser_popup (parent, name, type, okcb, okdata, applycb, applydata, stat)

    
    Widget parent;
    char *name;
    int type;
    void (*okcb)();
    void *okdata;
    void (*applycb)();
    void *applydata;
    int  stat;
{
    Widget browser;
    int n;
    Arg wargs[10];
    XmString none, ok;


    none = XmStringCreateSimple ("None");
    ok = XmStringCreateSimple ("Ok");
    n = 0;
    XtSetArg (wargs[n], XmNbrowseMode, type); n++;
    XtSetArg (wargs[n], XmNapplyLabelString, none); n++;
    XtSetArg (wargs[n], XmNokLabelString, ok); n++;
    XtSetArg (wargs[n], XmNnumLists, 1); n++;
    if (stat)
    {
        XtSetArg (wargs[n], XmNlist1IsStatic, TRUE); n++;
        XtSetArg (wargs[n], XmNinitialMapset1, 
        		    XmStringCreateSimple(G_mapset())); n++;
    }
    browser = XgCreateBrowserDialog (parent, "browser",  wargs, n);
    XtAddCallback(browser, XmNapplyCallback, downcb, browser); 

    if (okcb != NULL)
	XtAddCallback(browser, XmNokCallback, okcb, okdata);
    if (applycb != NULL)
    {
	XtAddCallback(browser, XmNapplyCallback, applycb, applydata);
        XtManageChild (XgInteractorGetChild(browser, XmINTERACT_APPLY_BUTTON));
    }

    XtFree (none);
    XtFree (ok);

    XtUnmanageChild (XgInteractorGetChild(browser, XmINTERACT_HELP_BUTTON));
    XtManageChild (browser);
    return browser;
}

show_digth ()
{
    char str[80];
    if (CM)
    {
	sprintf (str, "%7.4lf",  CM->head.digit_thresh);
        XmTextSetString (digth, str);
    }
}
show_snapth ()
{
    char str[80];
    if (CM)
    {
	sprintf (str, "%7.4lf", _map_to_dig_thresh(CM->snap_thresh));
        XmTextSetString (snapth, str);
    }
}
void
digthreshcb (w)
    Widget w;
{
    reset_thresh(w);
    Vect__write_head_binary (CM, &(CM->head));
}
