
/* reshow:
** reshows a panel after it has been created and closed
*/


#include "interface.h"

void 
reshow (data_cell *dc, int sub, int super)
{
    int n;
    Arg wargs[5];

    check_space(dc,sub, super);

    n = 0;
    XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg(wargs[n], XmNtopPosition, dc->here.toph);	 n++;
    XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
    XtSetArg(wargs[n], XmNbottomPosition, dc->here.botth);	 n++;
    XtSetValues(dc->panels[sub].form, wargs, n);

    dc->panels[sub].im_already_open = 1;
    XtManageChild(dc->panels[sub].form);

}

	
