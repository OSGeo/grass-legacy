#include "xgdisp.h"

void 
#ifdef _NO_PROTO
StdSiteIconToggleCallBack(w, cld, cbs)
Widget w;
XtPointer cld;
XmToggleButtonCallbackStruct *cbs;
#else
StdSiteIconToggleCallBack(Widget w, XtPointer cld, XmToggleButtonCallbackStruct *cbs)
#endif
{
  Global.stdsiteattr.icontype = (int) cld;	 
  if ( Global.selectedObjects == NULL)
    return;

  Global.setSite = XGD_SITE_STANDARD;
  
  if ( Global.selectedObjects->object &&
      SelectedObjectCount()==1 &&
      Global.selectedObjects->object->type == XGD_GEOFRAME && 
      Global.selectedObjects->object->Obj.GeoFrame.sites.site != NULL){
    XgdSiteInfo *tmp = NULL;
    XgdSiteInfo *sinfo = NULL;
    tmp = Global.selectedObjects->object->Obj.GeoFrame.sites.site;
    
    while (tmp!=NULL){
      if (tmp->type==XGD_SITE_STANDARD)
	if (strcmp (tmp->sname, Global.sname)==NULL &&
	    strcmp(tmp->smapset, Global.smapset) ==NULL )
	  sinfo = tmp;
      tmp=tmp->next;
    }

	if (sinfo == NULL) return;

	XgdSetStdSiteIconType(sinfo, Global.stdsiteattr.icontype);
	_xgdDrawSiteStd(Global.selectedObjects->object, sinfo);
	XgdRedrawGeoframe(Global.selectedObjects->object, True, True, NULL);
    }
    
}




Widget
#ifdef _NO_PROTO
CreateSiteIcons(parent, string)
Widget parent;
char *string;
#else
CreateSiteIcons( Widget parent, char *string)
#endif
{
    Widget caption;
    Widget frame;
    Widget form;
    Widget rc;
    Widget tbutton;
    XmString xms;
    int offset = 7;

    xms = XmStringCreateSimple(string);
    caption = XtVaCreateManagedWidget("barunit_caption", 
                                    xbaeCaptionWidgetClass,
                                    parent,
                                    XmNlabelPosition, XbaePositionTop,
                                    XmNlabelAlignment, XbaeAlignmentCenter,
                                    XmNlabelString, xms,
				    XmNlabelOffset, -offset,
                                    NULL);
    XmStringFree(xms);

    frame = XtVaCreateManagedWidget("barunit_frame",
                                  xmFrameWidgetClass, caption,
                                  XmNmarginWidth, offset,
                                  XmNmarginHeight, offset, NULL);

    form = XtVaCreateManagedWidget("linewidth_form",
        xmFormWidgetClass, frame, NULL);

    rc = XtVaCreateManagedWidget("rc",
	xmRowColumnWidgetClass, form,
	XmNorientation,	XmHORIZONTAL,
	XmNtopAttachment, XmATTACH_FORM,
	XmNleftAttachment, XmATTACH_FORM,
	XmNradioBehavior, XmONE_OF_MANY,
	XmNradioAlwaysOne, True,
	XmNnumColumns, 2,
	NULL);

    tbutton= XtVaCreateManagedWidget("cross_button",
        xmToggleButtonWidgetClass, rc,
	XmNlabelString, XmStringCreateSimple("Cross"),
        NULL);
    XtAddCallback(tbutton, XmNvalueChangedCallback, StdSiteIconToggleCallBack, CROSS);
    stdsitecrossw = tbutton;

    tbutton= XtVaCreateManagedWidget("diamond_button",
        xmToggleButtonWidgetClass, rc,
	XmNlabelString, XmStringCreateSimple("Diamond"),
        NULL);
    XtAddCallback(tbutton, XmNvalueChangedCallback, StdSiteIconToggleCallBack, DIAMOND);
    stdsitediamondw = tbutton;

    tbutton= XtVaCreateManagedWidget("rect_button",
        xmToggleButtonWidgetClass, rc,
	XmNlabelString, XmStringCreateSimple("Rectangle"),
        NULL);
    XtAddCallback(tbutton, XmNvalueChangedCallback, StdSiteIconToggleCallBack, RECT);
    stdsiterectw = tbutton;

    tbutton= XtVaCreateManagedWidget("plus_button",
        xmToggleButtonWidgetClass, rc,
	XmNlabelString, XmStringCreateSimple("Plus"),
        NULL);
    XtAddCallback(tbutton, XmNvalueChangedCallback, StdSiteIconToggleCallBack, PLUS);
    stdsiteplusw = tbutton;

    return caption;
}

