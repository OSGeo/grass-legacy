#include "xgdisp.h"

Widget
#ifdef _NO_PROTO
CreateStdSitesPanel()
#else
CreateStdSitesPanel(void)
#endif
{
    Widget xgi;
    Widget form;
    Widget stdsite;
    Widget stdsiteSizeFrame;
    Widget stdsitelineWidthFrame;
    Widget fgOptMenuFrame;
    Widget fgOptMenu ;
    Arg al[15];
    int ac = 0;
    XmString xms;

     /* interactor dialog parent */
    xms = XmStringCreateSimple("Dismiss");
    XtSetArg(al[ac], XmNenableWorkAreaStretch, True); ac++;
    XtSetArg(al[ac], XmNcancelLabelString, xms); ac++;
    xgi = XgCreateInteractorDialog(Global.applShell, "XGRASS Standard Sites Panel",
        al, ac);
    XmStringFree(xms);
    XtUnmanageChild(XgInteractorGetChild(xgi,XmINTERACT_OK_BUTTON));

    form = XtVaCreateManagedWidget("stdsitespl_form",
        xmFormWidgetClass, xgi,
        NULL);

    /* do the standard icons option */

    stdsite = CreateSiteIcons(form, "Standard Icons");

    stdsiteSizeFrame= CreateStdSiteGadget(form, "Set Site Size:", XGD_SITESIZE);

    XtVaSetValues(stdsiteSizeFrame, 
        XmNleftAttachment, XmATTACH_FORM,
        XmNtopAttachment, XmATTACH_WIDGET,
        XmNtopWidget,     stdsite,
	NULL);

    /* do the line width gadget */
    stdsitelineWidthFrame = CreateStdSiteGadget(form, "Set Site Width:", XGD_SITELW);

    XtVaSetValues(stdsitelineWidthFrame, 
        XmNleftAttachment, XmATTACH_WIDGET,
	XmNleftWidget, stdsiteSizeFrame,
        XmNtopAttachment, XmATTACH_WIDGET,
        XmNtopWidget, stdsite,
	NULL);

    /* do the foreground color option menus for bar lines */
    fgOptMenuFrame = CreateColorOptionMenu(form, &fgOptMenu, "Site Color:",
        SiteForegroundColorCallBack);

    XtVaSetValues(fgOptMenuFrame, 
        XmNtopAttachment, XmATTACH_WIDGET,
        XmNtopWidget, stdsitelineWidthFrame,
        XmNleftAttachment, XmATTACH_FORM,
	NULL);

    SetOptMenuToItem(fgOptMenu, "black");

    XtManageChild(xgi);

    return(xgi);
}

void 
#ifdef _NO_PROTO
SiteForegroundColorCallBack(w, cld, cad)
Widget w;
Pixel cld; 
XtPointer cad;
#else
SiteForegroundColorCallBack(Widget w, Pixel cld, XtPointer cad)
#endif
{

  Global.stdsiteattr.color = cld;

  if (Global.selectedObjects == NULL)
    return;
  
  if ( SelectedObjectCount() == 1 &&
      Global.selectedObjects->object->type==XGD_GEOFRAME &&
      Global.selectedObjects->object->Obj.GeoFrame.sites.site != NULL)
    {
      XgdSiteInfo *tmp;
      XgdSiteInfo *sinfo;
      
      tmp = Global.selectedObjects->object->Obj.GeoFrame.sites.site;
      while (tmp != NULL){
	if (tmp->type == XGD_SITE_STANDARD)
	  if (strcmp(tmp->sname,Global.sname)==NULL &&
	      strcmp(tmp->smapset,Global.smapset)==NULL)
	    sinfo = tmp;
	tmp = tmp->next;
      }
      
      if (sinfo == NULL) return; 
      
      
      XgdSetStdSiteColor(sinfo, Global.stdsiteattr.color);
      _xgdDrawSiteStd(Global.selectedObjects->object, sinfo);
      XgdRedrawGeoframe(Global.selectedObjects->object, False, True, NULL);
      XgdDrawObject(Global.selectedObjects->object->objgc, 
		    Global.selectedObjects->object, True, NULL);
    } 
}
