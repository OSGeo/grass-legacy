#include "xgen.h"

Widget
CreateToggle(object,widget)
	InterfaceObject *object;
	Widget widget;
{
	static int numtoggles = 0;
	char togglename[80];
	static int numtoggleEls = 0;
	char toggleElname[80];
	int n;
	Widget toggleW;
	Resource *resource;
	ToggleData *toggleData;
	ToggleData *curToggle;
	Boolean radio = False;
	Boolean first = True;

	numtoggles++;
	sprintf(togglename,"toggle%03d",numtoggles);
	curToggle = toggleData = (ToggleData *)XtMalloc(sizeof(ToggleData));
	bzero((char *)curToggle,sizeof(ToggleData));

	n = 0;
	SetGlobalArgs(&n,FONTS);
	/* KAB - add other resources in here... */
	SetObjectGeometryArgs(object,&n);
	SetObjectColorArgs(object,&n);
	SetObjectAlignmentArgs(object,&n);
	if ( (NULL != (resource = IndexResource(object,OBJECT,"toggletype"))) &&
	   ( !strcmp(resource->val.cval,"radio") )) {
		XtSetArg(args[n],XmNentryClass,xmToggleButtonWidgetClass); n++;
		toggleW = XmCreateRadioBox(widget,togglename,args,n);
		radio = True;
	} else {
		toggleW = XmCreateRowColumn(widget,togglename,args,n);
	}
	XtManageChild(toggleW);
	XmAddTabGroup(toggleW);
	if (NULL != (resource = IndexResource(object,OBJECT,"listelement"))) {
	    while(resource) {
			if ( !strcmp(resource->name,"listelement") ) {
		    	XmString xmstring;

				if ( !first ) {
					curToggle->next =(ToggleData *)XtMalloc(sizeof(ToggleData));
					bzero((char *)curToggle->next,sizeof(ToggleData));
					curToggle = curToggle->next;
				}
				first = False;
	            numtoggleEls++;
	            sprintf(toggleElname,"toggle%03d",numtoggleEls);

	        	n = 0;
	        	SetGlobalArgs(&n,FONTS);
	        	/* KAB - add other resources in here... */
	        	SetObjectGeometryArgs(object,&n);
	        	SetObjectColorArgs(object,&n);
	        	SetObjectAlignmentArgs(object,&n);
	        	XtSetArg(args[n],XmNlabelType,XmSTRING); n++;
				curToggle->name = XtMalloc(strlen(resource->val.cval)+1);
				strcpy(curToggle->name,resource->val.cval);
				curToggle->set = False;
				xmstring = XmStringLtoRCreate(resource->val.cval,SDC);
	        	XtSetArg(args[n],XmNlabelString,xmstring); n++;
	        	XtSetArg(args[n],XmNfillOnSelect,True); n++;
	        	XtSetArg(args[n],XmNindicatorOn,True); n++;
				if ( !radio ) {
					curToggle->radioIgnoreOff = False;
					XtSetArg(args[n],XmNindicatorType,XmN_OF_MANY); n++;
				}
				curToggle->widget = XmCreateToggleButton(toggleW,
													resource->val.cval,args,n);
				XtManageChild(curToggle->widget);
				XmAddTabGroup(curToggle->widget);
	
				if ( radio ) 
				    XtAddCallback(curToggle->widget,XmNvalueChangedCallback,
					    RadioChangedCB,(caddr_t)object);
				else
				    XtAddCallback(curToggle->widget,XmNvalueChangedCallback,
					    ToggleChangedCB,(caddr_t)object);
	
			}
			resource = resource->next;
		}
	} else {
		XgenFatalError("while creating toggle object","no toggle list elements");
	}
	AddToggleInfo(object->name,toggleData);
    return toggleW;
}

