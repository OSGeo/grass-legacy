#include "xgdisp.h"

Widget
#ifdef _NO_PROTO
CreateLineWidthGadget(parent, string)
Widget parent;
char *string;
#else
CreateLineWidthGadget( Widget parent, char *string)
#endif
{
    Widget caption;
    Widget frame;
    Widget form;
    Widget upArrow;
    Widget downArrow;
    Widget textField;
    XmString xms;
    int offset = 7;

    xms = XmStringCreateSimple(string);
    caption = XtVaCreateManagedWidget("line_width_caption", 
                                    xbaeCaptionWidgetClass,
                                    parent,
                                    XmNlabelPosition, XbaePositionTop,
                                    XmNlabelAlignment, XbaeAlignmentCenter,
                                    XmNlabelString, xms,
				    XmNlabelOffset, -offset,
                                    NULL);
    XmStringFree(xms);

    frame = XtVaCreateManagedWidget("linewidth_frame",
                                  xmFrameWidgetClass, caption,
                                  XmNmarginWidth, offset,
                                  XmNmarginHeight, offset, NULL);

    form = XtVaCreateManagedWidget("linewidth_form",
        xmFormWidgetClass, frame, NULL);


    upArrow = XtVaCreateManagedWidget("up_arrow_button",
        xmArrowButtonWidgetClass, form,
        XmNarrowDirection, XmARROW_UP,
	XmNtopAttachment, XmATTACH_FORM,
	XmNleftAttachment, XmATTACH_FORM,
        NULL);

    downArrow = XtVaCreateManagedWidget("down_arrow_button",
        xmArrowButtonWidgetClass, form,
        XmNarrowDirection, XmARROW_DOWN,
	XmNtopAttachment, XmATTACH_WIDGET,
	XmNtopWidget, upArrow,
	XmNleftAttachment, XmATTACH_FORM,
	XmNbottomAttachment, XmATTACH_FORM,
        NULL);

    textField = XtVaCreateManagedWidget("line_width_text",
        xmTextFieldWidgetClass, form,
	XmNtopAttachment, XmATTACH_FORM,
	XmNrightAttachment, XmATTACH_FORM,
	XmNleftAttachment, XmATTACH_WIDGET,
	XmNleftWidget, upArrow,
	XmNbottomAttachment, XmATTACH_FORM,
	XmNcolumns, 5,
        NULL);
    XmTextFieldInsert(textField, 0, "0");

    XtAddCallback(upArrow, XmNactivateCallback, 
        LineWidthIncrementCallBack, (XtPointer)textField);

    XtAddCallback(downArrow, XmNactivateCallback, 
        LineWidthDecrementCallBack, (XtPointer)textField);

    XtAddCallback(textField, XmNactivateCallback,
                  LineWidthTextCallback, (XtPointer) NULL);
    XtAddCallback(textField, XmNvalueChangedCallback,
                  LineWidthTextCallback, (XtPointer) NULL);
    XtAddCallback(textField, XmNmodifyVerifyCallback,
                  LineWidthTextVerifyCallback, (XtPointer) NULL);


/*    fprintf(stderr, "Line Width\n");*/

    return caption;
}

static XtCallbackRec valueCB[] = {
    {(XtCallbackProc)LineWidthTextCallback, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};
static XtCallbackRec verifyCB[] = {
    {(XtCallbackProc)LineWidthTextVerifyCallback, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};

void
#ifdef _NO_PROTO
LineWidthTextVerifyCallback(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
LineWidthTextVerifyCallback( Widget w, XtPointer cld, XtPointer cad)
#endif
{
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *) cad;
    int i;

    for (i = 0; i < cbs->text->length; i++) {
        if ((cbs->startPos + i) == 0) {
            if (!isdigit(*(cbs->text->ptr + i))) {
		cbs->doit = False;
                XBell(XtDisplay(w), 0);
                return;
            }
	} else {
            if (!isdigit(*(cbs->text->ptr + i))) {
                cbs->doit = False;
                XBell(XtDisplay(w), 0);
                return;
            }
	}
    }
}

void
#ifdef _NO_PROTO
LineWidthTextCallback(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
LineWidthTextCallback( Widget w, XtPointer cld, XtPointer cad)
#endif
{
    char *text = XmTextFieldGetString(w);
    int value = atoi(text);

    if ( value > Global.vHeight || value > Global.hWidth ) {
        char            new_text[10];

        XgWarningDialog(Global.applShell, "Line width exceeds size of page");
	sprintf(new_text, "%d", Global.lineWidth);
	XtRemoveCallbacks(w, XmNvalueChangedCallback, valueCB);
	XtRemoveCallbacks(w, XmNmodifyVerifyCallback, verifyCB);
	XmTextFieldSetString(w, new_text);
	XtAddCallback(w, XmNvalueChangedCallback,
		      LineWidthTextCallback, (XtPointer) NULL);
	XtAddCallback(w, XmNmodifyVerifyCallback,
		      LineWidthTextVerifyCallback, (XtPointer) NULL);
        return;
    }
    Global.lineWidth = value;
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
LineWidthDecrementCallBack(w, cld, cad)
Widget w;
Widget cld;
XtPointer cad;
#else
LineWidthDecrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
    char           *text;
    int             value;
    char            new_text[10];


    text = XmTextFieldGetString(cld);
    value = atoi(text);
    if ( value > Global.vHeight || value > Global.hWidth ) {
        XgWarningDialog(Global.applShell, "Line width exceeds size of page");
	sprintf(new_text, "%d", Global.lineWidth);
	XtRemoveCallbacks(cld, XmNvalueChangedCallback, valueCB);
	XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
	XmTextFieldSetString(cld, new_text);
	XtAddCallback(cld, XmNvalueChangedCallback,
		      LineWidthTextCallback, (XtPointer) NULL);
	XtAddCallback(cld, XmNmodifyVerifyCallback,
		      LineWidthTextVerifyCallback, (XtPointer) NULL);
        return;
    }
    if ( value == 0 ) {
	XBell(XtDisplay(w), 0);
        return;
    } else {
        value--;
    }
    sprintf(new_text, "%d", value);
    XtRemoveCallbacks(cld, XmNvalueChangedCallback, valueCB);
    XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
                  LineWidthTextCallback, (XtPointer) NULL);
    XtAddCallback(cld, XmNmodifyVerifyCallback,
                  LineWidthTextVerifyCallback, (XtPointer) NULL);
    Global.lineWidth = value;
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
LineWidthIncrementCallBack(w, cld, cad)
Widget w;
Widget cld;
XtPointer cad;
#else
LineWidthIncrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
    char           *text;
    int             value;
    char            new_text[10];


    text = XmTextFieldGetString(cld);
    value = atoi(text);
    if ( value > Global.vHeight || value > Global.hWidth ) {
        XgWarningDialog(Global.applShell, "Line width exceeds size of page");
	sprintf(new_text, "%d", Global.lineWidth);
	XtRemoveCallbacks(cld, XmNvalueChangedCallback, valueCB);
	XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
	XmTextFieldSetString(cld, new_text);
	XtAddCallback(cld, XmNvalueChangedCallback,
		      LineWidthTextCallback, (XtPointer) NULL);
	XtAddCallback(cld, XmNmodifyVerifyCallback,
		      LineWidthTextVerifyCallback, (XtPointer) NULL);
        return;
    }

    value++;
    sprintf(new_text, "%d", value);
    XtRemoveCallbacks(cld, XmNvalueChangedCallback, valueCB);
    XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
                  LineWidthTextCallback, (XtPointer) NULL);
    XtAddCallback(cld, XmNmodifyVerifyCallback,
                  LineWidthTextVerifyCallback, (XtPointer) NULL);
    Global.lineWidth = value;
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

