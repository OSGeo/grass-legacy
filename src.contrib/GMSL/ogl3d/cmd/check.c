
/* check_space:
** this function checks the auxiallry panel_form to see which 
** half (top or bottom)
** is free for the panel in waiting to occupy. If both halves are full, it
** closes the one that was opened first and places the one in waiting there
*/



#include "interface.h"

void 
check_space (data_cell *dc, int child, int super)
{
    static int child1 = -1, child2 = -1;
/*
    if(!dc->Wset){
	Arg wargs[3];
	int n;
	n = 0;
	XtSetArg(wargs[n], XmNset, FALSE); n++;
	XtSetValues(dc->Wwhatshere, wargs,n);
	dc->Wset = 1;
	XtRemoveEventHandler(dc->monitor, ButtonPressMask, 
		    FALSE, show_it, dc);
    }
*/

    if(super){
	dc->previous = 1;

	if(child1 >= 0){
	    dc->panels[child1].im_already_open = 0;
	    XtUnmanageChild(dc->panels[child1].form);
	}
	if(child2 >= 0){
	    dc->panels[child2].im_already_open = 0;
	    XtUnmanageChild(dc->panels[child2].form);
	}

	child1 = child;
	dc->here.toph = TOP;
	dc->here.botth = FORM;
	dc->panels[child].where = ALL;
	dc->top = dc->bott = 0;
	return;
    }


    if(!dc->top) {
	if(dc->previous) {
	    dc->panels[child1].im_already_open = 0;
	    XtUnmanageChild(dc->panels[child1].form);
	}

	dc->here.toph = TOP;
	dc->here.botth = MIDDLE;
	dc->panels[child].where = TOP;
	child1 = child;
	dc->top = dc->bott + 1;
	dc->previous = 0;
    }

    else if(!dc->bott) {
	dc->here.toph = BOTTOM;
	dc->here.botth = FORM;
	dc->panels[child].where = BOTTOM;
	child2 = child;
	dc->bott = dc->top + 1;
    }

    else if(dc->top < dc->bott) {
	dc->here.toph = TOP;
	dc->here.botth = MIDDLE;
	dc->panels[child].where = TOP;
	dc->panels[child1].im_already_open = 0;
	XtUnmanageChild(dc->panels[child1].form); 
	child1 = child;
	dc->top = dc->bott + 1;
    }

    else if(dc->bott < dc->top) {
	dc->here.toph = BOTTOM;
	dc->here.botth = FORM;
	dc->panels[child].where = BOTTOM;
	dc->panels[child2].im_already_open = 0;
	XtUnmanageChild(dc->panels[child2].form);
	child2 = child;
	dc->bott = dc->top + 1;
    }

}
