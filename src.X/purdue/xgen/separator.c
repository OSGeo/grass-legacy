#include "xgen.h"

Widget
CreateSeparator(object,widget)
    InterfaceObject *object;
    Widget widget;
{
    static int numseparators = 0;
    char separatorname[80];
    Resource *resource;
    XmString xmstring;
    int n;
    Widget separatorW;

    numseparators++;
    sprintf(separatorname,"separator%03d",numseparators);

    n = 0;
    SetGlobalArgs(&n,NOFONTS);
    /* KAB - add other resources in here... */
    SetObjectGeometryArgs(object,&n);
    SetObjectColorArgs(object,&n);
    if ( NULL != (resource = IndexResource(object,OBJECT,"orientation"))) {
		if ( resource->variable ) ExpandVariable(resource);
		if ( !strcmp(resource->val.cval,"horizontal")) {
			XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
		} else if ( !strcmp(resource->val.cval,"vertical")) {
			XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
		} else {

			sprintf(errorbuf,"illegal separator orientation in object [%s]",
				object->name);
			XgenWarning("create separator",errorbuf);
		}
	}
    if ( NULL != (resource = IndexResource(object,OBJECT,"separatortype"))) {
		if ( resource->variable ) ExpandVariable(resource);
		if ( !strcmp(resource->val.cval,"singleline")) {
			XtSetArg(args[n],XmNseparatorType,XmSINGLE_LINE); n++;
		} else if ( !strcmp(resource->val.cval,"doubleline")) {
			XtSetArg(args[n],XmNseparatorType,XmDOUBLE_LINE); n++;
		} else if ( !strcmp(resource->val.cval,"singledashedline")) {
			XtSetArg(args[n],XmNseparatorType,XmSINGLE_DASHED_LINE); n++;
		} else if ( !strcmp(resource->val.cval,"doubledashedline")) {
			XtSetArg(args[n],XmNseparatorType,XmDOUBLE_DASHED_LINE); n++;
		} else if ( !strcmp(resource->val.cval,"noline")) {
			XtSetArg(args[n],XmNseparatorType,XmNO_LINE); n++;
		} else if ( !strcmp(resource->val.cval,"shadowetchedin")) {
			XtSetArg(args[n],XmNseparatorType,XmSHADOW_ETCHED_IN); n++;
		} else if ( !strcmp(resource->val.cval,"shadowetchedout")) {
			XtSetArg(args[n],XmNseparatorType,XmSHADOW_ETCHED_OUT); n++;
		} else {

			sprintf(errorbuf,"illegal separatortype in object [%s]",
				object->name);
			XgenWarning("create separator",errorbuf);
			XtSetArg(args[n],XmNseparatorType,XmSINGLE_LINE); n++;
		}
	}
    separatorW = XmCreateSeparator(widget,separatorname,args,n);
    XtManageChild(separatorW);
    return separatorW;
}
