#include "xgdisp.h"

Widget
#ifdef _NO_PROTO
CreateToolBox()
#else
CreateToolBox(void)
#endif
{
    Widget xgi;
    Widget form;
    Widget fillPatMenuFrame;
    Widget linePatMenuFrame;
    Widget lineWidthFrame;
    Widget fgOptMenuFrame, bgOptMenuFrame;
    Widget fgOptMenu, bgOptMenu;
    Arg al[15];
    int ac = 0;
    XmString xms;

     /* interactor dialog parent */
    xms = XmStringCreateSimple("Dismiss");
    XtSetArg(al[ac], XmNenableWorkAreaStretch, True); ac++;
    XtSetArg(al[ac], XmNcancelLabelString, xms); ac++;
    xgi = XgCreateInteractorDialog(Global.applShell,"XGRASS Graphics Tool Box",
				   al, ac);
    XmStringFree(xms);
    XtUnmanageChild(XgInteractorGetChild(xgi,XmINTERACT_OK_BUTTON));

    form = XtVaCreateManagedWidget("tool_box_form",
        xmFormWidgetClass, xgi,
        NULL);

    /* do the fill pattern buttons */
    fillPatMenuFrame = XgdCreateFillPatternPad(form, "Set Fill Pattern:",
        2, XmHORIZONTAL, FillPatternCallBack);

    XtVaSetValues(fillPatMenuFrame, 
        XmNleftAttachment, XmATTACH_FORM,
        XmNtopAttachment, XmATTACH_FORM,
	NULL);

    /* do the line pattern buttons */
    linePatMenuFrame = XgdCreateLinePatternPad(form, "Set Line Pattern:",
        2, XmHORIZONTAL, LinePatternCallBack);

    XtVaSetValues(linePatMenuFrame, 
        XmNleftAttachment, XmATTACH_FORM,
        XmNtopAttachment, XmATTACH_WIDGET,
        XmNtopWidget, fillPatMenuFrame,
	NULL);

    /* do the line width gadget */
    lineWidthFrame = CreateLineWidthGadget(form, "Set Line Width:");

    XtVaSetValues(lineWidthFrame, 
        XmNleftAttachment, XmATTACH_FORM,
        XmNtopAttachment, XmATTACH_WIDGET,
        XmNtopWidget, linePatMenuFrame,
	NULL);

    /* do the foreground/background color option menus */
    fgOptMenuFrame = CreateColorOptionMenu(form, &fgOptMenu, "Foregound:",
        ForegroundColorCallBack);
    bgOptMenuFrame = CreateColorOptionMenu(form, &bgOptMenu, "Background:",
        BackgroundColorCallBack);

    XtVaSetValues(fgOptMenuFrame, 
        XmNtopAttachment, XmATTACH_WIDGET,
        XmNtopWidget, lineWidthFrame,
        XmNleftAttachment, XmATTACH_FORM,
	NULL);

    SetOptMenuToItem(fgOptMenu, "black");

    XtVaSetValues(bgOptMenuFrame, 
        XmNleftAttachment, XmATTACH_FORM,
        XmNtopAttachment, XmATTACH_WIDGET,
        XmNtopWidget, fgOptMenuFrame,
	NULL);

    SetOptMenuToItem(bgOptMenu, "white");

    XtManageChild(xgi);
    return(xgi);
}

void 
#ifdef _NO_PROTO
FillPatternCallBack(w, cld, cad)
Widget w;
int cld; 
XtPointer cad;
#else
FillPatternCallBack(Widget w, int cld, XtPointer cad)
#endif
{
  ObjectList addlist = NULL, exclude = NULL;
  XgdBox *bb = GetBBoxOfSelectedObjects();
  
  Global.fillPattern = cld;
  if ( Global.selectedObjects != NULL ) {
    ObjectList list = Global.selectedObjects;
    
    for ( ; list; list = list->next ) {
      XgdSetObjectFillPattern(list->object,
	           Global.foreground, Global.background, Global.fillPattern);
      if (list->object->type == XGD_GEOFRAME)
	XgdRedrawGeoframe(list->object, True, FALSE, NULL);
    }

    GetBoxAffectedObjects(bb, &addlist, &exclude);
    UnDrawObjectsInList(addlist, True);
    ReorderObjects(&addlist);
    DrawObjectsInList(addlist, NULL);
  }
}

void 
#ifdef _NO_PROTO
LinePatternCallBack(w, cld, cad)
Widget w;
int cld; 
XtPointer cad;
#else
LinePatternCallBack(Widget w, int cld, XtPointer cad)
#endif
{
    Global.linePattern = cld;
    if ( Global.selectedObjects != NULL ) {
        ObjectList list = Global.selectedObjects;

        for ( ; list; list = list->next ) {
	    XgdSetObjectLinePattern(list->object, 
		Global.lineWidth, Global.linePattern);
	}
	UnDrawObjectsInList(Global.selectedObjects, True);
        DrawObjectsInList(Global.selectedObjects, NULL);
    }
}

void 
#ifdef _NO_PROTO
ForegroundColorCallBack(w, cld, cad)
Widget w;
Pixel cld; 
XtPointer cad;
#else
ForegroundColorCallBack(Widget w, Pixel cld, XtPointer cad)
#endif
{
    Global.foreground = cld;
    if ( Global.selectedObjects != NULL ) {
        ObjectList list = Global.selectedObjects;

        for ( ; list; list = list->next ) {
	    XgdSetObjectForeground(list->object, Global.foreground);
	}
	UnDrawObjectsInList(Global.selectedObjects, True);
        DrawObjectsInList(Global.selectedObjects, NULL);
    }
}

void 
#ifdef _NO_PROTO
BackgroundColorCallBack(w, cld, cad)
Widget w;
Pixel cld; 
XtPointer cad;
#else
BackgroundColorCallBack(Widget w, Pixel cld, XtPointer cad)
#endif
{
    Global.background = cld;
    if ( Global.selectedObjects != NULL ) {
        ObjectList list = Global.selectedObjects;

        for ( ; list; list = list->next ) {
	    XgdSetObjectBackground(list->object, Global.background);
	}
	UnDrawObjectsInList(Global.selectedObjects, True);
        DrawObjectsInList(Global.selectedObjects, NULL);
    }
}
