
/* make_slider:
** makes a slider. Originally written by Terry Baker, modified
** by the Wolverine
*/

#include "interface.h"

Widget 
make_slider (Widget parent, int max, int min, int start, int decimal, char *name, void (*cb1)(void), void (*cb2)(void), data_cell *dc, int show_name, int orientation, Arg wargs[30], int n)
{
    Widget w;
    XmString   str;

    
    str = XmStringCreate (name, XmSTRING_DEFAULT_CHARSET);
    XtSetArg (wargs[n], XmNvalue, start); n++;
    XtSetArg (wargs[n], XmNshowValue, FALSE); n++;
    XtSetArg (wargs[n], XmNminimum, min); n++;
    XtSetArg (wargs[n], XmNmaximum, max); n++;
    XtSetArg (wargs[n], XmNdecimalPoints, decimal); n++;

    if(show_name)
	XtSetArg (wargs[n], XmNtitleString, str); n++;

    XtSetArg (wargs[n], XmNorientation, orientation); n++;
    w = XtCreateManagedWidget (name, xmScaleWidgetClass, parent, wargs, n);

    if (cb1 != NULL)
	XtAddCallback(w, XmNvalueChangedCallback, cb1, dc); 
    if (cb2 != NULL)
	XtAddCallback(w, XmNdragCallback, cb2, dc);

    return (w);

}


