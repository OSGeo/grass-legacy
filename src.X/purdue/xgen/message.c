#include "xgen.h"

Widget
CreateMessage(object,widget)
	InterfaceObject *object;
	Widget widget;
{
	Resource *resource;
	static int nummessages = 0;
	char messagename[80];
	XmFontList fontList;
	XmString xmmessage;
	int columns;
	int n;
	int i;
	Widget messageW;
	Boolean usedTitle;
	MessageInfo *mi, *miHead;

	/* check for columns specd */
	if ( NULL != (resource = IndexResource(object,OBJECT,"columns"))) {
		if ( resource->variable ) ExpandVariable(resource);
		columns = resource->val.ival;
	} else 
		columns = 20;

	/* check for font specd */
	if ( NULL != (resource = IndexResource(object,OBJECT,"font"))) {
		if ( resource->variable ) ExpandVariable(resource);
		fontList = XmFontListCreate(
					XLoadQueryFont(xgenGD.display,resource->val.cval),SDC);
	} else 
		fontList = NULL;

	usedTitle = False;
	if ( NULL != (resource = IndexResource(object,OBJECT,"titlestring"))) {
		if ( resource->variable ) ExpandVariable(resource);
	    miHead = mi = ProcessMessage(resource->val.cval,columns);
	    usedTitle = True;
	} else
	    miHead = mi = ProcessMessage(object->name,columns);
	

	n = 0;
	SetGlobalArgs(&n,NOFONTS);
	if ( fontList != NULL ) {
		XtSetArg(args[n],XmNfontList,fontList); n++;
	}
	/* KAB - add other resources in here... */
	SetObjectGeometryArgs(object,&n);
	SetObjectColorArgs(object,&n);
	SetObjectAlignmentArgs(object,&n);
	XtSetArg(args[n],XmNlabelType,XmSTRING);  n++;
	while ( mi ) {
		char *ptr;
		
		nummessages++;
		sprintf(messagename,"message%03d",nummessages);
	
		if ( usedTitle )
		    ptr = strpart(resource->val.cval,mi->startpos,mi->endpos);
		else
		    ptr = strpart(object->name,mi->startpos,mi->endpos);
		xmmessage = XmStringLtoRCreate(ptr,SDC);
		XtSetArg(args[n],XmNlabelString,xmmessage);
		messageW = XmCreateLabel(widget,messagename,args,n + 1);
		XtManageChild(messageW);
		mi = mi->next;
	}
	FreeMIList(miHead);
	return messageW;
}
