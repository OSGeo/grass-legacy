
/* make_text:
** creates and returns a TextField widget 
*/

#include "interface.h"


Widget 
make_text (Widget parent, char *str, int cols, char *name, void (*cb)(void), data_cell *dc, Arg wargs[], int n)
{
    Widget w;

    XtSetArg (wargs[n], XmNvalue, str); n++;

    if (cols != 0)
	XtSetArg (wargs[n], XmNcolumns, cols); n++;
/*
    w = XmCreateTextField (parent, name, wargs, n);
*/

    XtSetArg (wargs[n], XmNmarginHeight, 1); n++;
    XtSetArg (wargs[n], XmNmarginWidth, 1); n++;
    w = XmCreateText(parent, name, wargs, n);

    if (cb != NULL){
        XtAddCallback(w, XmNactivateCallback, cb, dc); 
    }

    XtManageChild (w);

    return w;

}


