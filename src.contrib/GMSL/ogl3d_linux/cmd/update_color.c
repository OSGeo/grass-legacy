
/* update_color:
** updates the "current_color" widget for the panel
** that called it
*/

#include "interface.h"

void 
update_color (Widget w, data_cell *dc, caddr_t client_data)
{
}
/*
    Widget parent;
    Arg wargs[3];
    int n, i;
    long int color;

    parent = XtParent(w);
    
    for(i = 0; i < MAX_DYN_COLORS; i++) {

	if(parent == dc->color_bars[i]) {
	    n = 0;
	    XtSetArg(wargs[n], XmNbackground, &color); n++;
	    XtGetValues(w, wargs, n);
	    XtSetArg(wargs[n], XmNbackground, color); n++;
	    XtSetValues(dc->current_color[i], wargs, n);
	}
    }

*/

