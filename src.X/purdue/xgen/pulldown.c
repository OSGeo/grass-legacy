/***************************************************************
 * pulldown.c
 *
 * This file contains the pulldown creation routine.
 **************************************************************/

#include "xgen.h"

/***************************************************************
 * CreatePulldown - creates a pulldown in menu bar menuBar.
 **************************************************************/
Widget
CreatePulldown(object,menuBar)
	InterfaceObject *object;
	Widget menuBar;
{
	InterfaceObject *o = object->objects;
	int n;
	Resource *resource;
	Widget pulldownW, cb;

	n = 0;
	SetGlobalArgs(&n,FONTS);
	SetObjectColorArgs(object,&n);
    pulldownW = XmCreatePulldownMenu(menuBar, "pulldown", args,n);
	/*XtManageChild(pulldownW);*/
	SetObjectFont(object,&n);
	XtSetArg(args[n],XmNsubMenuId,pulldownW); n++;
	if ( (resource = IndexResource(object,OBJECT,"titlestring")))  {
		if ( resource->variable ) ExpandVariable(resource);
	    cb = XmCreateCascadeButton(menuBar, resource->val.cval, args,n);
	} else
	    cb = XmCreateCascadeButton(menuBar, object->name, args,n);
	XtManageChild(cb);

	while ( o ) {
		switch( o->type ) {
			case SEPARATOR:
				o->widget = CreateSeparator(o,pulldownW);
				break;
			case PUSHBUTTON:
				o->widget = CreateButton(o,pulldownW);
				break;
		}
		o = o->next;
	}
}
