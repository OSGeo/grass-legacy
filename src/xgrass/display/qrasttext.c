#include "xgdisp.h"

Widget
#ifdef _NO_PROTO
CreateRWhereGadget(parent, string, type)
Widget parent;
char *string;
int  type;
#else
CreateRWhereGadget( Widget parent, char *string, int type)
#endif
{
    Widget caption;
    Widget frame;
    Widget form;
    Widget northrc,eastrc,label;
    Widget textField;
    XmString xms;
    int offset = 7;

    xms = XmStringCreateSimple(string);
    caption = XtVaCreateManagedWidget("qrastwhere_caption", 
                                    xbaeCaptionWidgetClass,
                                    parent,
                                    XmNlabelPosition, XbaePositionTop,
                                    XmNlabelAlignment, XbaeAlignmentCenter,
                                    XmNlabelString, xms,
				    XmNlabelOffset, -offset,
                                    NULL);
    XmStringFree(xms);

    frame = XtVaCreateManagedWidget("qrastwhere",
                                  xmFrameWidgetClass, caption,
                                  XmNmarginWidth, offset,
                                  XmNmarginHeight, offset, NULL);

    form = XtVaCreateManagedWidget("qrastwhere_form",
        xmFormWidgetClass, frame, NULL);


    northrc = XtVaCreateManagedWidget("qrastwhere_rc",
	xmRowColumnWidgetClass, form, 
	XmNorientation, XmHORIZONTAL,
	XmNtopAttachment, XmATTACH_FORM,
	XmNrightAttachment, XmATTACH_FORM,
	XmNleftAttachment, XmATTACH_FORM,
	XmNspacing,	2,
	NULL);

    label = XtVaCreateManagedWidget("qrastwhere_lbl",
	xmLabelGadgetClass, northrc,
	XmNlabelString, XmStringCreateSimple("North"),
	NULL);

    textField = XtVaCreateManagedWidget("qrastwhere_north",
        xmTextFieldWidgetClass, northrc,
	XmNresizeWidth, True,
	XmNeditable,	False,
        NULL);
    qrastnorthw = textField;

     eastrc = XtVaCreateManagedWidget("qrastwhere_rc",
	xmRowColumnWidgetClass, form,
	XmNorientation, XmHORIZONTAL,
	XmNtopAttachment, XmATTACH_WIDGET,
	XmNtopWidget,	northrc,
	XmNrightAttachment, XmATTACH_FORM,
	XmNleftAttachment, XmATTACH_FORM,
	NULL);

     label = XtVaCreateManagedWidget("qrastwhere_label",
	xmLabelGadgetClass, eastrc,
	XmNlabelString, XmStringCreateSimple("East "),
	NULL);

    textField = XtVaCreateManagedWidget("qrastwhere_east",
        xmTextFieldWidgetClass, eastrc,
	XmNresizeWidth, True,
	XmNeditable,	False,
        NULL);
    qrasteastw = textField;


    return caption;
}


Widget
#ifdef _NO_PROTO
CreateRWhatGadget(parent, string, type)
Widget parent;
char *string;
int  type;
#else
CreateRWhatGadget( Widget parent, char *string, int type)
#endif
{
    Widget caption;
    Widget frame;
    Widget form;
    Widget catnumrc,catnamerc,label;
    Widget textField;
    XmString xms;
    int offset = 7;

    xms = XmStringCreateSimple(string);
    caption = XtVaCreateManagedWidget("qrastwhat_caption", 
                                    xbaeCaptionWidgetClass,
                                    parent,
                                    XmNlabelPosition, XbaePositionTop,
                                    XmNlabelAlignment, XbaeAlignmentCenter,
                                    XmNlabelString, xms,
				    XmNlabelOffset, -offset,
                                    NULL);
    XmStringFree(xms);

    frame = XtVaCreateManagedWidget("qrastwhat",
                                  xmFrameWidgetClass, caption,
                                  XmNmarginWidth, offset,
                                  XmNmarginHeight, offset, NULL);

    form = XtVaCreateManagedWidget("qrastwhat_form",
        xmFormWidgetClass, frame, NULL);


    catnumrc = XtVaCreateManagedWidget("qrastwhat_rc",
	xmRowColumnWidgetClass, form, 
	XmNorientation, XmHORIZONTAL,
	XmNtopAttachment, XmATTACH_FORM,
	XmNrightAttachment, XmATTACH_FORM,
	XmNleftAttachment, XmATTACH_FORM,
	XmNspacing,	2,
	NULL);

    label = XtVaCreateManagedWidget("qrastwhat_lbl",
	xmLabelGadgetClass, catnumrc,
	XmNlabelString, XmStringCreateSimple("Category "),
	NULL);

    textField = XtVaCreateManagedWidget("qrastwhat_catnum",
        xmTextFieldWidgetClass, catnumrc,
	XmNresizeWidth, True,
	XmNeditable, False,
        NULL);
    qrastcatnumw = textField;


     catnamerc = XtVaCreateManagedWidget("qrastwhat_rc",
	xmRowColumnWidgetClass, form,
	XmNorientation, XmHORIZONTAL,
	XmNtopAttachment, XmATTACH_WIDGET,
	XmNtopWidget,	catnumrc,
	XmNrightAttachment, XmATTACH_FORM,
	XmNleftAttachment, XmATTACH_FORM,
	NULL);

     label = XtVaCreateManagedWidget("qrastwhat_label",
	xmLabelGadgetClass, catnamerc,
	XmNlabelString, XmStringCreateSimple("Label"),
	NULL);

    textField = XtVaCreateManagedWidget("qrastwhat_catname",
        xmTextFieldWidgetClass, catnamerc,
	XmNresizeWidth, True,
	XmNeditable, False,
        NULL);
    qrastcatnamew = textField;

    return caption;
}
