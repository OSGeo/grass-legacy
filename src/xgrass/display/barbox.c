#include "xgdisp.h"

Widget
#ifdef _NO_PROTO
CreateBarToolBox()
#else
CreateBarToolBox(void)
#endif
{
    Widget      xgi;
    Widget      form;
    Widget      button;
    Widget      barLengthFrame, barIntervalFrame;
    Widget      lineWidthFrame;
    Widget      fgOptMenuFrame, textOptMenuFrame;
    Widget      barstyle, barunit;
    Arg         al[15];
    int         ac = 0;
    XmString    xms;

    /* interactor dialog parent */
    xms = XmStringCreateSimple("Dismiss");
    XtSetArg(al[ac], XmNenableWorkAreaStretch, True);
    ac++;
    XtSetArg(al[ac], XmNcancelLabelString, xms);
    ac++;
    xgi = XgCreateInteractorDialog(Global.applShell, "XGRASS Barscale Attributes Box",
                       al, ac);
    XmStringFree(xms);
    XtUnmanageChild(XgInteractorGetChild(xgi, XmINTERACT_OK_BUTTON));

    form = XtVaCreateManagedWidget("bar_box_form",
                       xmFormWidgetClass, xgi,
                       NULL);

    /* do the barscale style option */

    barstyle = CreateBarStyle(form, "Barscale Style");

    barunit = CreateBarUnit(form, "Barscale Unit");

    XtVaSetValues(barunit,
              XmNleftAttachment, XmATTACH_FORM,
              XmNtopAttachment, XmATTACH_WIDGET,
              XmNtopWidget, barstyle,
              NULL);

    barLengthFrame = CreateBarGadget(form, "Set Bar Length:", XGD_BARLENGTH);

    XtVaSetValues(barLengthFrame,
              XmNleftAttachment, XmATTACH_FORM,
              XmNtopAttachment, XmATTACH_WIDGET,
              XmNtopWidget, barunit,
              NULL);

    barIntervalFrame = CreateBarGadget(form, "Set Bar Interval:", XGD_BARINTERVAL);

    XtVaSetValues(barIntervalFrame,
              XmNleftAttachment, XmATTACH_WIDGET,
              XmNleftWidget, barLengthFrame,
              XmNtopAttachment, XmATTACH_WIDGET,
              XmNtopWidget, barunit,
              NULL);

    /* do the line width gadget */
    lineWidthFrame = CreateBarGadget(form, "Set Bar Thickness:", XGD_BARTHICK);

    XtVaSetValues(lineWidthFrame,
              XmNleftAttachment, XmATTACH_FORM,
              XmNtopAttachment, XmATTACH_WIDGET,
              XmNtopWidget, barIntervalFrame,
              NULL);

    /* do the foreground color option menus for bar lines */
    fgOptMenuFrame = CreateColorOptionMenu(form, &barfgw, "Bar Color:",
                           BarForegroundColorCallBack);
    /* do the text color option menus for bar label text */
    textOptMenuFrame = CreateColorOptionMenu(form, &bartcw, "Label Color:",
                         BarTextColorCallBack);

    XtVaSetValues(fgOptMenuFrame,
              XmNtopAttachment, XmATTACH_WIDGET,
              XmNtopWidget, lineWidthFrame,
              XmNleftAttachment, XmATTACH_FORM,
              NULL);

    SetOptMenuToItem(barfgw, "black");

    XtVaSetValues(textOptMenuFrame,
              XmNleftAttachment, XmATTACH_FORM,
              XmNtopAttachment, XmATTACH_WIDGET,
              XmNtopWidget, fgOptMenuFrame,
              NULL);

    SetOptMenuToItem(bartcw, "black");

    button = XtVaCreateManagedWidget("Barscale Font",
                     xmPushButtonGadgetClass, form,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, textOptMenuFrame, NULL);

    XtAddCallback(button, XmNactivateCallback, SetFontCallBack, XGD_BARSCALE);


    XtManageChild(xgi);

    return (xgi);
}

void
#ifdef _NO_PROTO
BarForegroundColorCallBack(w, cld, cad)
    Widget      w;
    Pixel       cld;
    XtPointer       cad;
#else
BarForegroundColorCallBack(Widget w, Pixel cld, XtPointer cad)
#endif
{
    Global.barattr.color = (int) cld;
    if (Global.selectedObjects == NULL)
        return;

    if (SelectedObjectCount() == 1 &&
        Global.selectedObjects->object->type == XGD_BARSCALE) {
        XgdSetBarscaleColor(Global.selectedObjects->object, Global.barattr.color);
        XgdDrawBarscale(Global.selectedObjects->object, False, NULL);
    }
}

void
#ifdef _NO_PROTO
BarTextColorCallBack(w, cld, cad)
    Widget      w;
    Pixel       cld;
    XtPointer       cad;
#else
BarTextColorCallBack(Widget w, Pixel cld, XtPointer cad)
#endif
{
    Global.barattr.textcolor = cld;

    if (Global.selectedObjects == NULL)
        return;

    if (SelectedObjectCount() == 1 &&
        Global.selectedObjects->object->type == XGD_BARSCALE) {
        XgdSetBarscaleTextColor(Global.selectedObjects->object, Global.barattr.textcolor);
        XgdDrawBarscale(Global.selectedObjects->object, False, NULL);
    }
}
