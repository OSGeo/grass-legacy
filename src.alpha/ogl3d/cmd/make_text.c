
/* make_text:
** creates and returns a TextField widget 
*/

#include "interface.h"


Widget 
make_text (parent, str, cols, name, cb, dc, wargs, n)
    Widget parent;
    char *str;
    int cols;
    char *name;
    void (*cb)();
    data_cell *dc;
    Arg wargs[];
    int n;
{
    Widget w;

    XtSetArg (wargs[n], XmNvalue, str); n++;

    if (cols != 0)
	XtSetArg (wargs[n], XmNcolumns, cols); n++;
/*
    w = XmCreateTextField (parent, name, wargs, n);
*/
    w = XmCreateText(parent, name, wargs, n);

    if (cb != NULL){
        XtAddCallback(w, XmNactivateCallback, cb, dc); 
    }

    XtManageChild (w);

    return w;

}


