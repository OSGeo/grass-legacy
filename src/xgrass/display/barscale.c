#include "xgdisp.h"

void
#ifdef _NO_PROTO
InitBarscaleAttributes(dpy, scrn, gc)
    Display    *dpy;
    int         scrn;
    GC          gc;
#else
InitBarscaleAttributes(Display * dpy, int scrn, GC gc)
#endif
{
    unsigned long   mask;
    XGCValues       gcv;

    mask = GCFont;
    XGetGCValues(dpy, gc, mask, &gcv);

    Global.barattr.length = 10000.0;
    Global.barattr.intervals = 5;
    Global.barattr.linewidth = 2;
    Global.barattr.color = XgdGetVectColorPixelByName("black");
    Global.barattr.style = DASHED;
    Global.barattr.unit = XGD_METERS;
    Global.barattr.fid = gcv.font;
    Global.barattr.textcolor = XgdGetVectColorPixelByName("black");
    Global.barattr.winwidth = 0;
    Global.barattr.gfobj = NULL;
}

void
#ifdef _NO_PROTO
todrawbarscale(obj, x, y)
    XgdObject      *obj;
    int         x, y;
#else
todrawbarscale(XgdObject *obj, int x, int y)
#endif
{
    if (Global.selectedObjects->object->Obj.GeoFrame.gridOn &&
        Global.selectedObjects->object->Obj.GeoFrame.grid.labelon)
        Global.barattr.winwidth =
        Global.selectedObjects->object->width - Global.selectedObjects->object->Obj.GeoFrame.grid.xoff;
    else
        Global.barattr.winwidth = Global.selectedObjects->object->width;


    obj->x = x;
    obj->y = y;
    obj->Obj.Barscale.gfobj = Global.selectedObjects->object;

    /* add it to the barscale array */
    if (Global.selectedObjects->object->Obj.GeoFrame.numbarscales != 0) {
        int i; 
        XgdObject *object = Global.selectedObjects->object;
        int oldcount = object->Obj.GeoFrame.numbarscales;
        XgdObject **newbs = 
            (XgdObject **)XtCalloc(oldcount + 1, sizeof(XgdObject *));
        
        for ( i = 0; i < object->Obj.GeoFrame.numbarscales; i++ ) 
            newbs[i] = object->Obj.GeoFrame.barscales[i];
	newbs[i] = obj;
        XtFree(object->Obj.GeoFrame.barscales);
        object->Obj.GeoFrame.barscales = newbs;
        object->Obj.GeoFrame.numbarscales++;
    } else {
        XgdObject *object = Global.selectedObjects->object;

        object->Obj.GeoFrame.barscales = (XgdObject **)
            XtCalloc(1, sizeof(XgdObject*));
        object->Obj.GeoFrame.barscales[0] = obj;
        object->Obj.GeoFrame.numbarscales++;
    }

    XgdUpdateBarscale(obj,
              Global.barattr.length,
              Global.barattr.intervals,
              Global.barattr.linewidth,
              Global.barattr.color,
              Global.barattr.style,
              Global.barattr.unit,
              Global.barattr.fid,
              Global.barattr.textcolor,
              Global.barattr.winwidth
        );

}

void
#ifdef _NO_PROTO
BarLengthTextVerifyCallBack(w, cld, cad)
    Widget      w;
    XtPointer       cld, cad;
#else
BarLengthTextVerifyCallBack(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *) cad;
    int         i;

    for (i = 0; i < cbs->text->length; i++) {
        if ((cbs->startPos + i) == 0) {
            if (!isdigit(*(cbs->text->ptr + i)) && *(cbs->text->ptr + i) != '.') {
                cbs->doit = False;
                XBell(XtDisplay(w), 0);
                return;
            }
        } else {
            if (!isdigit(*(cbs->text->ptr + i)) && *(cbs->text->ptr + i) != '.') {
                cbs->doit = False;
                XBell(XtDisplay(w), 0);
                return;
            }
        }
    }
}


void
#ifdef _NO_PROTO
redrawbarscale(type)
    int         type;
#else
redrawbarscale(int type)
#endif
{
    if (Global.selectedObjects == NULL)
        return;

    if (Global.selectedObjects->object &&
        SelectedObjectCount() == 1 &&
        Global.selectedObjects->object->type == XGD_BARSCALE) {
        if ( Global.barattr.length <= 0.0 ) return;
        XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
        RedrawArea(Global.selectedObjects->object);
        XgdUpdateBarscale(Global.selectedObjects->object,
                  Global.barattr.length,
                  Global.barattr.intervals,
                  Global.barattr.linewidth,
                  Global.barattr.color,
                  Global.barattr.style,
                  Global.barattr.unit,
                  Global.barattr.fid,
                  Global.barattr.textcolor,
                  Global.barattr.winwidth);
        XgdUpdateBoxForObject(
                      Global.selectedObjects->object,
                      Global.selectedObjects->object->y,
                      Global.selectedObjects->object->y + Global.selectedObjects->object->height,
                      Global.selectedObjects->object->x,
                      Global.selectedObjects->object->x + Global.selectedObjects->object->width);
        XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
    }
}

Widget
#ifdef _NO_PROTO
CreateBarGadget(parent, string, type)
    Widget      parent;
    char       *string;
    int         type;
#else
CreateBarGadget(Widget parent, char *string, int type)
#endif
{
    Widget      caption;
    Widget      frame;
    Widget      form;
    Widget      upArrow;
    Widget      downArrow;
    Widget      textField;
    XmString    xms;
    int         offset = 7;
    char        lineThickText[256], intervalText[256], lengthText[256];

    sprintf(lineThickText, "%d", Global.barattr.linewidth);
    sprintf(intervalText, "%d", Global.barattr.intervals);
    sprintf(lengthText, "%8.2", Global.barattr.length);

    xms = XmStringCreateSimple(string);
    caption = XtVaCreateManagedWidget("bartext_caption",
                      xbaeCaptionWidgetClass,
                      parent,
                      XmNlabelPosition, XbaePositionTop,
                     XmNlabelAlignment, XbaeAlignmentCenter,
                      XmNlabelString, xms,
                      XmNlabelOffset, -offset,
                      NULL);
    XmStringFree(xms);

    frame = XtVaCreateManagedWidget("bartext",
                    xmFrameWidgetClass, caption,
                    XmNmarginWidth, offset,
                    XmNmarginHeight, offset, NULL);

    if (type != XGD_BARLENGTH) {
        form = XtVaCreateManagedWidget("bartext_form",
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
                            XmNcolumns, 8,
                            NULL);
    } else {
        textField = XtVaCreateManagedWidget("line_width_text",
                          xmTextFieldWidgetClass, frame,
                            XmNcolumns, 8,
                            NULL);
    }

    switch (type) {
    case XGD_BARLENGTH:
        XmTextFieldInsert(textField, 0, lengthText);
        XtAddCallback(textField, XmNactivateCallback,
                  BarLengthTextCallBack, (XtPointer) NULL);

        XtAddCallback(textField, XmNvalueChangedCallback,
                  BarLengthTextCallBack, (XtPointer) NULL);

        XtAddCallback(textField, XmNmodifyVerifyCallback,
                  BarLengthTextVerifyCallBack, (XtPointer) NULL);
        barlengthw = textField;
        break;

    case XGD_BARINTERVAL:
        XmTextFieldInsert(textField, 0, intervalText);
        XtAddCallback(upArrow, XmNactivateCallback,
               BarIntervalIncrementCallBack, (XtPointer) textField);
        XtAddCallback(downArrow, XmNactivateCallback,
               BarIntervalDecrementCallBack, (XtPointer) textField);
        XtAddCallback(textField, XmNvalueChangedCallback,
                  BarIntervalTextCallBack, (XtPointer) NULL);
        XtAddCallback(textField, XmNmodifyVerifyCallback,
                  GridGapTextVerifyCallback, (XtPointer) NULL);
        barintervalw = textField;
        break;

    case XGD_BARTHICK:
        XmTextFieldInsert(textField, 0, lineThickText);
        XtAddCallback(upArrow, XmNactivateCallback,
              BarThickIncrementCallBack, (XtPointer) textField);
        XtAddCallback(downArrow, XmNactivateCallback,
              BarThickDecrementCallBack, (XtPointer) textField);
        XtAddCallback(textField, XmNvalueChangedCallback,
                  BarThickTextCallBack, (XtPointer)
                  NULL);
        XtAddCallback(textField, XmNmodifyVerifyCallback,
                  GridGapTextVerifyCallback, (XtPointer) NULL);
        barthickw = textField;

        break;
    }
    return caption;
}


static XtCallbackRec lvalueCB[] = {
    {(XtCallbackProc) BarLengthTextCallBack, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};




void
#ifdef _NO_PROTO
BarLengthTextCallBack(w, cld, cad)
    Widget      w;
    XtPointer       cld, cad;
#else
BarLengthTextCallBack(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    char       *text = XmTextFieldGetString(w);
    double      value = atof(text);

    if ( Global.selectedObjects->object == NULL ) return;
    if (Global.selectedObjects->object->type != XGD_BARSCALE ) {
        XgWarningDialog(Global.applShell,"Selected object must be a barscale.");
        return;
    }
    if (value < 0.0) {
        char        new_text[10];

        XgWarningDialog(Global.applShell, "Line width exceeds size of page");
        sprintf(new_text, "%8.2f", Global.barattr.length);
        XtRemoveCallbacks(w, XmNvalueChangedCallback, lvalueCB);
        /*
         * XtRemoveCallbacks(w, XmNmodifyVerifyCallback, verifyCB);
         */
        XmTextFieldSetString(w, new_text);
        XtAddCallback(w, XmNvalueChangedCallback,
                  BarLengthTextCallBack, (XtPointer) NULL);
        XtAddCallback(w, XmNmodifyVerifyCallback,
                  BarLengthTextVerifyCallBack, (XtPointer) NULL);
        return;
    }
    Global.barattr.length = value;
    if (Global.selectedObjects->object->type == XGD_BARSCALE)
        XgdSetBarscaleLength(Global.selectedObjects->object, Global.barattr.length);

     redrawbarscale(XGD_BARLENGTH);
    
}


void
#ifdef _NO_PROTO
BarLengthDecrementCallBack(w, cld, cad)
    Widget      w;
    Widget      cld;
    XtPointer       cad;
#else
BarLengthDecrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
    char       *text;
    double      value;
    char        new_text[10];

    text = XmTextFieldGetString(cld);
    value = atof(text);

    if (value < 0) {
        XgWarningDialog(Global.applShell, "Bar Length is less than zero");
        sprintf(new_text, "%8.2f", Global.barattr.length);
        XtRemoveCallbacks(cld, XmNvalueChangedCallback, lvalueCB);
        /*
         * XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
         */
        XmTextFieldSetString(cld, new_text);
        XtAddCallback(cld, XmNvalueChangedCallback,
                  BarLengthTextCallBack, (XtPointer) NULL);
        XtAddCallback(cld, XmNmodifyVerifyCallback,
                  BarLengthTextVerifyCallBack, (XtPointer) NULL);
        return;
    }
    if (value == 0) {
        XBell(XtDisplay(w), 0);
        return;
    } else {
        value--;
    }
    sprintf(new_text, "%d", value);
    XtRemoveCallbacks(cld, XmNvalueChangedCallback, lvalueCB);
    /*
     * XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
     */
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
              BarLengthTextCallBack, (XtPointer) NULL);
    XtAddCallback(cld, XmNmodifyVerifyCallback,
              BarLengthTextVerifyCallBack, (XtPointer) NULL);
    Global.barattr.length = value;
    if (Global.selectedObjects->object->type == XGD_BARSCALE)
        XgdSetBarscaleLength(Global.selectedObjects->object, Global.barattr.length);

     redrawbarscale(XGD_BARLENGTH);
}



void
#ifdef _NO_PROTO
BarLengthIncrementCallBack(w, cld, cad)
    Widget      w;
    Widget      cld;
    XtPointer       cad;
#else
BarLengthIncrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
    char       *text;
    int         value;
    char        new_text[10];

    text = XmTextFieldGetString(cld);
    value = atoi(text);
    value++;

    sprintf(new_text, "%d", value);
    XtRemoveCallbacks(cld, XmNvalueChangedCallback, lvalueCB);
    /*
     * XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
     */
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
              BarLengthTextCallBack, (XtPointer) NULL);
    XtAddCallback(cld, XmNmodifyVerifyCallback,
              BarLengthTextVerifyCallBack, (XtPointer) NULL);
    Global.barattr.length = value;
    if (Global.selectedObjects->object->type == XGD_BARSCALE)
        XgdSetBarscaleLength(Global.selectedObjects->object, Global.barattr.length);

     redrawbarscale(XGD_BARLENGTH);
    
}


static XtCallbackRec ivalueCB[] = {
    {(XtCallbackProc) BarIntervalTextCallBack, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};

void
#ifdef _NO_PROTO
BarIntervalTextCallBack(w, cld, cad)
    Widget      w;
    XtPointer       cld, cad;
#else
BarIntervalTextCallBack(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    char       *text = XmTextFieldGetString(w);
    int         value = atoi(text);

    if (value < 0) {
        char        new_text[10];

        XgWarningDialog(Global.applShell, "Line width exceeds size of page");
        sprintf(new_text, "%d", Global.barattr.intervals);
        XtRemoveCallbacks(w, XmNvalueChangedCallback, ivalueCB);
        /*
         * XtRemoveCallbacks(w, XmNmodifyVerifyCallback, verifyCB);
         */
        XmTextFieldSetString(w, new_text);
        XtAddCallback(w, XmNvalueChangedCallback,
                  BarIntervalTextCallBack, (XtPointer) NULL);
        XtAddCallback(w, XmNmodifyVerifyCallback,
                  GridGapTextVerifyCallback, (XtPointer) NULL);
        return;
    }
    Global.barattr.intervals = value;
    if (Global.selectedObjects->object->type == XGD_BARSCALE)
        XgdSetBarscaleInterval(Global.selectedObjects->object,
                       Global.barattr.intervals);

    
    redrawbarscale(XGD_BARINTERVAL);
  
}


void
#ifdef _NO_PROTO
BarIntervalDecrementCallBack(w, cld, cad)
    Widget      w;
    Widget      cld;
    XtPointer       cad;
#else
BarIntervalDecrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
    char       *text;
    int         value;
    char        new_text[10];

    text = XmTextFieldGetString(cld);
    value = atoi(text);

    if (value < 0) {
        XgWarningDialog(Global.applShell, "Bar Interval is less than zero");
        sprintf(new_text, "%d", Global.barattr.intervals);
        XtRemoveCallbacks(cld, XmNvalueChangedCallback, ivalueCB);
        /*
         * XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
         */
        XmTextFieldSetString(cld, new_text);
        XtAddCallback(cld, XmNvalueChangedCallback,
                  BarIntervalTextCallBack, (XtPointer) NULL);
        XtAddCallback(cld, XmNmodifyVerifyCallback,
                  GridGapTextVerifyCallback, (XtPointer) NULL);
        return;
    }
    if (value == 0) {
        XBell(XtDisplay(w), 0);
        return;
    } else {
        value--;
    }
    sprintf(new_text, "%d", value);
    XtRemoveCallbacks(cld, XmNvalueChangedCallback, ivalueCB);
    /*
     * XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
     */
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
              BarIntervalTextCallBack, (XtPointer) NULL);
    XtAddCallback(cld, XmNmodifyVerifyCallback,
              GridGapTextVerifyCallback, (XtPointer) NULL);
    Global.barattr.intervals = value;
    if (Global.selectedObjects->object->type == XGD_BARSCALE)
        XgdSetBarscaleInterval(Global.selectedObjects->object,
                       Global.barattr.intervals);
    
    redrawbarscale(XGD_BARINTERVAL);
   
}



void
#ifdef _NO_PROTO
BarIntervalIncrementCallBack(w, cld, cad)
    Widget      w;
    Widget      cld;
    XtPointer       cad;
#else
BarIntervalIncrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
    char       *text;
    int         value;
    char        new_text[10];

    text = XmTextFieldGetString(cld);
    value = atoi(text);
    value++;

    sprintf(new_text, "%d", value);
    XtRemoveCallbacks(cld, XmNvalueChangedCallback, ivalueCB);
    /*
     * XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
     */
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
              BarIntervalTextCallBack, (XtPointer) NULL);
    XtAddCallback(cld, XmNmodifyVerifyCallback,
              GridGapTextVerifyCallback, (XtPointer) NULL);
    Global.barattr.intervals = value;
    if (Global.selectedObjects->object->type == XGD_BARSCALE)
        XgdSetBarscaleInterval(Global.selectedObjects->object,
                       Global.barattr.intervals);
   
    redrawbarscale(XGD_BARINTERVAL);
   
}

static XtCallbackRec tvalueCB[] = {
    {(XtCallbackProc) BarThickTextCallBack, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};

void
#ifdef _NO_PROTO
BarThickTextCallBack(w, cld, cad)
    Widget      w;
    XtPointer       cld, cad;
#else
BarThickTextCallBack(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    char       *text = XmTextFieldGetString(w);
    int         value = atoi(text);

    if (value < 0) {
        char        new_text[10];

        XgWarningDialog(Global.applShell, "Line width exceeds size of page");
        sprintf(new_text, "%d", Global.barattr.linewidth);
        XtRemoveCallbacks(w, XmNvalueChangedCallback, tvalueCB);
        /*
         * XtRemoveCallbacks(w, XmNmodifyVerifyCallback, verifyCB);
         */
        XmTextFieldSetString(w, new_text);
        XtAddCallback(w, XmNvalueChangedCallback,
                  BarThickTextCallBack, (XtPointer) NULL);
        XtAddCallback(w, XmNmodifyVerifyCallback,
                  GridGapTextVerifyCallback, (XtPointer) NULL);
        return;
    }
    Global.barattr.linewidth = value;
    redrawbarscale(XGD_BARTHICK);
}


void
#ifdef _NO_PROTO
BarThickDecrementCallBack(w, cld, cad)
    Widget      w;
    Widget      cld;
    XtPointer       cad;
#else
BarThickDecrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
    char       *text;
    int         value;
    char        new_text[10];

    text = XmTextFieldGetString(cld);
    value = atoi(text);

    if (value < 0) {
        XgWarningDialog(Global.applShell, "Bar Thick is less than zero");
        sprintf(new_text, "%d", Global.barattr.linewidth);
        XtRemoveCallbacks(cld, XmNvalueChangedCallback, tvalueCB);
        /*
         * XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
         */
        XmTextFieldSetString(cld, new_text);
        XtAddCallback(cld, XmNvalueChangedCallback,
                  BarThickTextCallBack, (XtPointer) NULL);
        XtAddCallback(cld, XmNmodifyVerifyCallback,
                  GridGapTextVerifyCallback, (XtPointer) NULL);
        return;
    }
    if (value == 0) {
        XBell(XtDisplay(w), 0);
        return;
    } else {
        value--;
    }
    sprintf(new_text, "%d", value);
    XtRemoveCallbacks(cld, XmNvalueChangedCallback, tvalueCB);
    /*
     * XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
     */
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
              BarThickTextCallBack, (XtPointer) NULL);
    XtAddCallback(cld, XmNmodifyVerifyCallback,
              GridGapTextVerifyCallback, (XtPointer) NULL);

    Global.barattr.linewidth = value;
    redrawbarscale(XGD_BARTHICK);
}



void
#ifdef _NO_PROTO
BarThickIncrementCallBack(w, cld, cad)
    Widget      w;
    Widget      cld;
    XtPointer       cad;
#else
BarThickIncrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
    char       *text;
    int         value;
    char        new_text[10];

    text = XmTextFieldGetString(cld);
    value = atoi(text);
    value++;

    sprintf(new_text, "%d", value);
    XtRemoveCallbacks(cld, XmNvalueChangedCallback, tvalueCB);
    /*
     * XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
     */
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
              BarThickTextCallBack, (XtPointer) NULL);
    XtAddCallback(cld, XmNmodifyVerifyCallback,
              GridGapTextVerifyCallback, (XtPointer) NULL);
    Global.barattr.linewidth = value;

    redrawbarscale(XGD_BARTHICK);
}
