#include "xgdisp.h"

void 
#ifdef _NO_PROTO
QRCancelCallBack(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
QRCancelCallBack(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    SetMode(XGD_MODE_SELECT);
}

Widget
#ifdef _NO_PROTO
CreateQueryRasterPanel()
#else
CreateQueryRasterPanel(void)
#endif
{
    Widget xgi;
    Widget form;
    Widget objRC;
    Widget objSep;
    Widget objLabel;
    Widget nameRC, mapsetRC;
    Widget textField;
    Widget QRasterWhereFrame, QRasterWhatFrame;
    Arg al[15];
    int ac = 0;
    XmString xms;

     /* interactor dialog parent */
    xms = XmStringCreateSimple("Dismiss");
    XtSetArg(al[ac], XmNenableWorkAreaStretch, True); ac++;
    XtSetArg(al[ac], XmNcancelLabelString, xms); ac++;
    xgi = XgCreateInteractorDialog(Global.applShell, "XGRASS Query Raster Panel",
        al, ac);
    XmStringFree(xms);
    XtAddCallback(xgi, XmNcancelCallback, QRCancelCallBack, NULL);
    XtUnmanageChild(XgInteractorGetChild(xgi,XmINTERACT_OK_BUTTON));

    form = XtVaCreateManagedWidget("rasterpanel_form",
        xmFormWidgetClass, xgi,
        NULL);

    objRC = XtVaCreateManagedWidget("rasterpanel_rc",
	xmRowColumnWidgetClass, form,
	XmNtopAttachment, XmATTACH_FORM,
	XmNleftAttachment, XmATTACH_FORM,
	XmNrightAttachment, XmATTACH_FORM,
	XmNorientation,	XmHORIZONTAL,
	XmNspacing,	10,
	NULL);

    nameRC = XtVaCreateManagedWidget("rasterpanel_namerc",
	xmRowColumnWidgetClass, objRC,
	XmNorientation, XmHORIZONTAL,
	XmNspacing,	5,
	NULL);

    objLabel = XtVaCreateManagedWidget("rasterpanel_namelbl",
	xmLabelGadgetClass, nameRC,
	XmNlabelString, XmStringCreateSimple("Map Name"),
	NULL);

    textField = XtVaCreateManagedWidget("rasterpanel_mapname",
	xmTextFieldWidgetClass, nameRC,
	XmNresizeWidth,	True,
	XmNeditable,	False,
	NULL);
    qrastnamew = textField;
    XmTextFieldInsert(textField, 0, Global.selectedObjects->object->Obj.GeoFrame.rname);

    mapsetRC = XtVaCreateManagedWidget("rastpanel_mapsetrc", 
	xmRowColumnWidgetClass, objRC,
	XmNorientation,	XmHORIZONTAL,
	XmNspacing,	5,
	NULL); 

    objLabel = XtVaCreateManagedWidget("rasterpanel_mapsetlbl",
	xmLabelGadgetClass, mapsetRC,
	XmNlabelString, XmStringCreateSimple("Mapset"),
	NULL);

    textField = XtVaCreateManagedWidget("rastpanel_mapset",
	xmTextFieldWidgetClass, mapsetRC,
	XmNresizeWidth,	True,
	XmNeditable,	False,
	NULL);
     qrastmapsetw = textField;
    XmTextFieldInsert(textField, 0, Global.selectedObjects->object->Obj.GeoFrame.rmapset);

    objSep = XtVaCreateManagedWidget("rasterpanel_sep",
	xmSeparatorGadgetClass, form,
	XmNtopAttachment, XmATTACH_WIDGET,
	XmNtopWidget,	objRC,
	XmNleftAttachment, XmATTACH_FORM,
	XmNrightAttachment, XmATTACH_FORM,
	NULL);

    QRasterWhereFrame= CreateRWhereGadget(form, "Where:", NULL);

    XtVaSetValues(QRasterWhereFrame, 
        XmNleftAttachment, XmATTACH_FORM,
        XmNtopAttachment, XmATTACH_WIDGET,
        XmNtopWidget, objSep,
	NULL);

    QRasterWhatFrame= CreateRWhatGadget(form, "What:", NULL);

    XtVaSetValues(QRasterWhatFrame, 
        XmNleftAttachment, XmATTACH_WIDGET,
	XmNleftWidget, QRasterWhereFrame,
        XmNtopAttachment, XmATTACH_WIDGET,
        XmNtopWidget,     objSep,
	NULL);

    XtManageChild(xgi);

    return(xgi);
}
