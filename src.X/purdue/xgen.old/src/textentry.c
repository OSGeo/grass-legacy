#include "xgen.h"

Widget
CreateTextEntry(object,widget)
	InterfaceObject *object;
	Widget widget;
{
	static int numtextentries = 0;
	char textentryname[80];
	Resource *resource;
	int n;
	Widget textentryW;

	numtextentries++;
	sprintf(textentryname,"textentry%03d",numtextentries);

	n = 0;
	SetGlobalArgs(&n,FONTS);
	/* KAB - add other resources in here... */
	SetObjectGeometryArgs(object,&n);
	SetObjectColorArgs(object,&n);
	if ( NULL != (resource = IndexResource(object,OBJECT,"valuestring"))) {
		XtSetArg(args[n],XmNvalue,resource->val.cval); n++;
	}
	textentryW = XmCreateText(widget,textentryname,args,n);
	XtManageChild(textentryW);
	XmAddTabGroup(textentryW);
	return textentryW;
}
