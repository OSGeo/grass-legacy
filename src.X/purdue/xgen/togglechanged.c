#include "xgen.h"

void
ToggleChangedCB(w,cld,cad)
	Widget w;
	caddr_t cld;
	caddr_t cad;
{
	InterfaceObject *object = (InterfaceObject *)cld;
	XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)cad;
    int n;
	Resource *resource;
    XmString xmlabel;
	ToggleData *tdp;
    char *text;

	n = 0;
	XtSetArg(args[n],XmNlabelString,&xmlabel); n++;
    XtGetValues(w,args,n);

    XmStringGetLtoR(xmlabel,SDC,&text);

	if ( NULL == (tdp = IndexToggleData(object->name))) {
		XgenFatalError("indexing toggle data", "no such data");
	}

	while(tdp) {
		if ( !strcmp(tdp->name,text) )
				tdp->set = cbs->set;
		tdp = tdp->next;
	}
}

void
RadioChangedCB(w,cld,cad)
	Widget w;
	caddr_t cld;
	caddr_t cad;
{
	InterfaceObject *object = (InterfaceObject *)cld;
	XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)cad;
    int n;
	Resource *resource;
    XmString xmlabel;
	ToggleData *tdp;
    char *text;

	n = 0;
	XtSetArg(args[n],XmNlabelString,&xmlabel); n++;
    XtGetValues(w,args,n);

    XmStringGetLtoR(xmlabel,SDC,&text);

	if ( NULL == (tdp = IndexToggleData(object->name))) {
		XgenFatalError("indexing toggle data", "no such data");
	}

	while(tdp) {
		if ( !strcmp(tdp->name,text) ) {
			if ( tdp->set && cbs->set ) {
				tdp->radioIgnoreOff = True;
			    tdp->set = cbs->set;
			} else if ( tdp->set && !(cbs->set) && tdp->radioIgnoreOff) {
				tdp->radioIgnoreOff = False;
			} else
				tdp->set = cbs->set;
		}
		tdp = tdp->next;
	}
}
