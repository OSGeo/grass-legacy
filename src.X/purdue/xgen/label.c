#include "xgen.h"

Widget
CreateLabel(object,widget)
    InterfaceObject *object;
    Widget widget;
{
    static int numlabels = 0;
    char labelname[80];
    Resource *resource;
    XmString xmstring;
    int n;
    Widget labelW;

    numlabels++;
    sprintf(labelname,"label%03d",numlabels);

    n = 0;
    SetGlobalArgs(&n,FONTS);
    /* KAB - add other resources in here... */
    SetObjectGeometryArgs(object,&n);
    SetObjectColorArgs(object,&n);
    SetObjectAlignmentArgs(object,&n);
    SetObjectFont(object,&n);

    if ( NULL != (resource = IndexResource(object,OBJECT,"labelpixmap"))) {
        Pixmap pixmap;

		if ( resource->variable ) ExpandVariable(resource);
        pixmap = XmGetPixmap(xgenGD.scrptr,resource->val.cval,
            xgenGD.g_fgs.pixel,xgenGD.g_bgs.pixel);
        if ( pixmap == XmUNSPECIFIED_PIXMAP ) {

            sprintf(errorbuf,"labelpixmap [%s] not found",resource->val.cval);
            XgenWarning("create label",errorbuf);
        }
        XtSetArg(args[n],XmNlabelType,XmPIXMAP); n++;
        XtSetArg(args[n],XmNlabelPixmap,pixmap); n++;
    } else {
        if ( NULL != (resource = IndexResource(object,OBJECT,"titlestring"))) {
		    if ( resource->variable ) ExpandVariable(resource);
            xmstring = XmStringLtoRCreate(resource->val.cval,SDC);
        } else
            xmstring = XmStringLtoRCreate(object->name,SDC);
        XtSetArg(args[n],XmNlabelType,XmSTRING); n++;
        XtSetArg(args[n],XmNlabelString,xmstring); n++;
    }
    labelW = XmCreateLabel(widget,labelname,args,n);
    XtManageChild(labelW);
    return labelW;
}
