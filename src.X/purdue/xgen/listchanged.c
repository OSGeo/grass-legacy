#include "xgen.h"

void
SSListChangedCB(w,cld,cad)
	Widget w;
	caddr_t cld;
	caddr_t cad;
{
	InterfaceObject *object = (InterfaceObject *)cld;
	XmListCallbackStruct *cbs = (XmListCallbackStruct *)cad;
    int n;
    XmString xmlabel;
	ListData *ldp;
    char *text;

	if ( NULL == (ldp = IndexListData(object->name))) {
		XgenFatalError("indexing list data", "no such data");
	}

	while(ldp) {
		if ( XmStringCompare(ldp->item,cbs->item) ) ldp->selected = True;
		else ldp->selected = False;
		ldp = ldp->next;
	}
}

void
MSListChangedCB(w,cld,cad)
	Widget w;
	caddr_t cld;
	caddr_t cad;
{
	InterfaceObject *object = (InterfaceObject *)cld;
	XmListCallbackStruct *cbs = (XmListCallbackStruct *)cad;
    int n;
    XmString xmlabel;
	ListData *ldp;
    char *text;

	n = 0;
	XtSetArg(args[n],XmNlabelString,&xmlabel); n++;
    XtGetValues(w,args,n);

	if ( NULL == (ldp = IndexListData(object->name))) {
		XgenFatalError("indexing list data", "no such data");
	}

	while(ldp) {
		if ( XmStringCompare(ldp->item,cbs->item) ) {
			if ( ldp->selected ) ldp->selected = False;
			else ldp->selected = True;
		}
		ldp = ldp->next;
	}

}

void
ESListChangedCB(w,cld,cad)
	Widget w;
	caddr_t cld;
	caddr_t cad;
{
	InterfaceObject *object = (InterfaceObject *)cld;
	XmListCallbackStruct *cbs = (XmListCallbackStruct *)cad;
    int n;
    int i;
    XmString xmlabel;
	ListData *ldp;
    char *text;

	if ( NULL == (ldp = IndexListData(object->name))) {
		XgenFatalError("indexing list data", "no such data");
	}

	while(ldp) {
		ldp->selected = False;
		for ( i = 0; i < cbs->selected_item_count;i++ ) 
		    if ( XmStringCompare(ldp->item,cbs->selected_items[i]) )
			    ldp->selected = True;
		ldp = ldp->next;
	}
}

void
BSListChangedCB(w,cld,cad)
	Widget w;
	caddr_t cld;
	caddr_t cad;
{
	SSListChangedCB(w,cld,cad);
}
