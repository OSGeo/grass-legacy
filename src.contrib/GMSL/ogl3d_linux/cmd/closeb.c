
/* closeb:
** this function adds a close button to panel which called it
*/

#include "interface.h"

void 
closeb (int who, data_cell *dc, int destroy)
{

	Arg wargs[10];
	int n;

	n = 0;
	SetPositionArgs(wargs, &n, -1, 98, -1, 98, XmATTACH_NONE);
	dc->panels[who].close = XtCreateManagedWidget("Close", 
		  xmPushButtonWidgetClass, dc->panels[who].form, wargs, n);
	if(destroy)
	    XtAddCallback(dc->panels[who].close,XmNactivateCallback,
			    close_destroy_me,dc);	
	else
	    XtAddCallback(dc->panels[who].close,XmNactivateCallback,
			    close_me,dc);	

}	


