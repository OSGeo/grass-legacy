#include "xgen.h"

void 
helpCB(w,cld,cad)
	Widget w;
	caddr_t cld;
	caddr_t cad;
{
    XmString help = XmStringCreateLtoR((char *)cld,SDC);
	int n;

	n = 0;
	XtSetArg(args[n],XmNmessageString,help); n++;
	XtSetValues(xgenGD.help,args,n);
	XtManageChild(xgenGD.help);
}
