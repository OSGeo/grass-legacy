#include "xgdisp.h"

void 
#ifdef _NO_PROTO
redrawgrid(type)
int type;
#else
redrawgrid(int type)
#endif
{
  if (Global.selectedObjects == NULL){
    /*
      XgWarningDialog(Global.applShell, "No GeoFrame selected.");
      */
    return;
  } else if (SelectedObjectCount() != 1){
    XgWarningDialog(Global.applShell, "Select 1 GeoFrame only.");
    return;
  } else if (Global.selectedObjects->object->type != XGD_GEOFRAME){
    XgWarningDialog(Global.applShell, "Selected object must be a GeoFrame.");
    return;
  } else {
    if ( Global.selectedObjects->object && 
	Global.selectedObjects->object->Obj.GeoFrame.gridOn) {
      XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
      switch (type) {
      case XGD_GRIDGAP:
	XgdSetGridGap(Global.selectedObjects->object, Global.gridattr.gridgap);
	break;
      case XGD_GRIDSPACING:
	XgdSetGridSpacing(Global.selectedObjects->object,
			  Global.gridattr.spacing);
	break;	
      case XGD_GRIDLINEWIDTH:
	XgdSetGridLinePattern(Global.selectedObjects->object,
			      Global.gridattr.linewidth,
			      Global.gridattr.linepattern);
	break;
      }

      if (!Global.selectedObjects->object->Obj.GeoFrame.grid.labelon){
	XgdUnDrawObject(Global.selectedObjects->object,
			Global.selectedObjects->object->bg, True);
	XgdRedrawGeoframe(Global.selectedObjects->object,True, True, NULL);
        XgdUpdateGrid(Global.selectedObjects->object,
		      XgdIsGridOn(Global.selectedObjects->object),
		      Global.gridattr.labelon,
		      Global.gridattr.gridgap,
		      Global.gridattr.spacing, Global.gridattr.color,
		      Global.gridattr.linewidth, Global.gridattr.linepattern,
		      Global.gridattr.fid, Global.gridattr.textcolor,
		      Global.gridattr.xoff, Global.gridattr.yoff);
      } else {
	XgdUnDrawObject(Global.selectedObjects->object,
			Global.selectedObjects->object->bg, True);
	XgdResizePixmap(Global.selectedObjects->object,
			Global.selectedObjects->object->width-Global.selectedObjects->object->Obj.GeoFrame.grid.xoff,
			Global.selectedObjects->object->height-Global.selectedObjects->object->Obj.GeoFrame.grid.yoff,
			Global.selectedObjects->object->Obj.GeoFrame.grid.xoff,
			Global.selectedObjects->object->Obj.GeoFrame.grid.yoff,
			False);
	
        XgdUpdateGrid(Global.selectedObjects->object,
		      XgdIsGridOn(Global.selectedObjects->object),
		      Global.gridattr.labelon,
		      Global.gridattr.gridgap,
		      Global.gridattr.spacing, Global.gridattr.color,
		      Global.gridattr.linewidth, Global.gridattr.linepattern,
		      Global.gridattr.fid, Global.gridattr.textcolor,
		      0, 0);
	
        DoGridLabel(Global.selectedObjects->object, 0, &Global.gridattr.xoff,
		    &Global.gridattr.yoff);
        DoGridLabel(Global.selectedObjects->object, 1, &Global.gridattr.xoff,
		    &Global.gridattr.yoff);
	
	XgdRedrawGeoframe(Global.selectedObjects->object, True, True, NULL);
      }
      XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
      
    }
  }
}


Widget
#ifdef _NO_PROTO
CreateGridGadget(parent, string, type)
Widget parent;
char *string;
int  type;
#else
CreateGridGadget( Widget parent, char *string, int type)
#endif
{
    Widget caption;
    Widget frame;
    Widget form;
    Widget upArrow;
    Widget downArrow;
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

/*
    textField = XtVaCreateManagedWidget("line_width_text",
        xmTextFieldWidgetClass, form,
	XmNtopAttachment, XmATTACH_FORM,
	XmNrightAttachment, XmATTACH_FORM,
	XmNleftAttachment, XmATTACH_WIDGET,
	XmNleftWidget, upArrow,
	XmNbottomAttachment, XmATTACH_FORM,
	XmNcolumns, 8,
        NULL);
*/

    switch (type)
    {
    case XGD_GRIDGAP:
    gridgapw= XtVaCreateManagedWidget("line_width_text",
        xmTextFieldWidgetClass, form,
	XmNtopAttachment, XmATTACH_FORM,
	XmNrightAttachment, XmATTACH_FORM,
	XmNleftAttachment, XmATTACH_WIDGET,
	XmNleftWidget, upArrow,
	XmNbottomAttachment, XmATTACH_FORM,
	XmNcolumns, 8,
        NULL);
    XmTextFieldInsert(gridgapw, 0, "1000.0");

    XtAddCallback(upArrow, XmNactivateCallback, 
        GridGapIncrementCallBack, (XtPointer)gridgapw);

    XtAddCallback(downArrow, XmNactivateCallback, 
        GridGapDecrementCallBack, (XtPointer)gridgapw);

    XtAddCallback(gridgapw, XmNactivateCallback,
                  GridGapTextCallback, (XtPointer) NULL);
    XtAddCallback(gridgapw, XmNvalueChangedCallback,
                  GridGapTextCallback, (XtPointer) NULL);
/*
    XtAddCallback(gridgapw, XmNmodifyVerifyCallback,
                  GridGapTextVerifyCallback, (XtPointer) NULL);
*/

/*    fprintf(stderr, "Line Width\n");*/
     break;

    case XGD_GRIDSPACING:
    gridspacew= XtVaCreateManagedWidget("line_width_text",
        xmTextFieldWidgetClass, form,
	XmNtopAttachment, XmATTACH_FORM,
	XmNrightAttachment, XmATTACH_FORM,
	XmNleftAttachment, XmATTACH_WIDGET,
	XmNleftWidget, upArrow,
	XmNbottomAttachment, XmATTACH_FORM,
	XmNcolumns, 8,
        NULL);

    XmTextFieldInsert(gridspacew, 0, "5");
    XtAddCallback(upArrow, XmNactivateCallback,
	GridSpacingIncrementCallBack, (XtPointer)gridspacew);

    XtAddCallback(downArrow, XmNactivateCallback,
	GridSpacingDecrementCallBack, (XtPointer)gridspacew);

    XtAddCallback(gridspacew, XmNactivateCallback,
	GridSpacingTextCallback, (XtPointer)NULL);

    XtAddCallback(gridspacew, XmNvalueChangedCallback,
	GridSpacingTextCallback, (XtPointer)NULL);
    
    XtAddCallback(gridspacew, XmNmodifyVerifyCallback,
	GridGapTextVerifyCallback, (XtPointer)NULL);

    break;


   case XGD_GRIDLINEWIDTH:
    gridlww= XtVaCreateManagedWidget("line_width_text",
        xmTextFieldWidgetClass, form,
	XmNtopAttachment, XmATTACH_FORM,
	XmNrightAttachment, XmATTACH_FORM,
	XmNleftAttachment, XmATTACH_WIDGET,
	XmNleftWidget, upArrow,
	XmNbottomAttachment, XmATTACH_FORM,
	XmNcolumns, 8,
        NULL);
    XmTextFieldInsert(gridlww, 0, "1");

    XtAddCallback(upArrow, XmNactivateCallback,
	GridLineWidthIncrementCallBack, (XtPointer)gridlww);

    XtAddCallback(downArrow, XmNactivateCallback,
	GridLineWidthDecrementCallBack, (XtPointer)gridlww);

    XtAddCallback(gridlww, XmNactivateCallback,
	GridLineWidthTextCallback, (XtPointer)NULL);

    XtAddCallback(gridlww, XmNvalueChangedCallback,
	GridLineWidthTextCallback, (XtPointer)NULL);
    
    XtAddCallback(gridlww, XmNmodifyVerifyCallback,
	GridGapTextVerifyCallback, (XtPointer)NULL);
    break;

   }

    return caption;
}

static XtCallbackRec valueCB[] = {
    {(XtCallbackProc)GridGapTextCallback, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};
static XtCallbackRec verifyCB[] = {
    {(XtCallbackProc)GridGapTextVerifyCallback, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};

void
#ifdef _NO_PROTO
GridGapTextVerifyCallback(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
GridGapTextVerifyCallback( Widget w, XtPointer cld, XtPointer cad)
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

extern double atof();

void
#ifdef _NO_PROTO
GridGapTextCallback(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
GridGapTextCallback( Widget w, XtPointer cld, XtPointer cad)
#endif
{
double value;
    char *text = XmTextFieldGetString(w);
  
    value = atof(text);

    if ( value < 0.0 ) {
        char            new_text[10];

        XgWarningDialog(Global.applShell, "Line width exceeds size of page");
	sprintf(new_text, "%8.2f", Global.gridattr.gridgap);
	XtRemoveCallbacks(w, XmNvalueChangedCallback, valueCB);
	XtRemoveCallbacks(w, XmNmodifyVerifyCallback, verifyCB);
	XmTextFieldSetString(w, new_text);
	XtAddCallback(w, XmNvalueChangedCallback,
		      GridGapTextCallback, (XtPointer) NULL);
/*
	XtAddCallback(w, XmNmodifyVerifyCallback,
		      GridGapTextVerifyCallback, (XtPointer) NULL);
*/
        return;
    }
    Global.gridattr.gridgap = value;

    redrawgrid(XGD_GRIDGAP);
}

void
#ifdef _NO_PROTO
GridGapDecrementCallBack(w, cld, cad)
Widget w;
Widget cld;
XtPointer cad;
#else
GridGapDecrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
    char           *text;
    double 	   value;
    char           new_text[10];

    text = XmTextFieldGetString(cld);
    value = atof (text);

    if ( value < 0.0 ) {

        XgWarningDialog(Global.applShell, "Grid Gap is less than zero");
	sprintf(new_text, "%d", Global.gridattr.gridgap);
	XtRemoveCallbacks(cld, XmNvalueChangedCallback, valueCB);
	XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
	XmTextFieldSetString(cld, new_text);
	XtAddCallback(cld, XmNvalueChangedCallback,
		      GridGapTextCallback, (XtPointer) NULL);
/*
	XtAddCallback(cld, XmNmodifyVerifyCallback,
		      GridGapTextVerifyCallback, (XtPointer) NULL);
*/
        return;
    }
    if ( value == 0.0 ) {
	XBell(XtDisplay(w), 0);
        return;
    } else {
        value = value - 10.0;
    }
    sprintf(new_text, "%f", value);
    XtRemoveCallbacks(cld, XmNvalueChangedCallback, valueCB);
    XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
                  GridGapTextCallback, (XtPointer) NULL);
/*
    XtAddCallback(cld, XmNmodifyVerifyCallback,
                  GridGapTextVerifyCallback, (XtPointer) NULL);
*/
    Global.gridattr.gridgap = value;

    redrawgrid(XGD_GRIDGAP);
}






void
#ifdef _NO_PROTO
GridGapIncrementCallBack(w, cld, cad)
Widget w;
Widget cld;
XtPointer cad;
#else
GridGapIncrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
    char           *text;
    double 	   value;
    char           new_text[10];

    text = XmTextFieldGetString(cld);
    value = atof(text);

    value = value + 10.0;
    sprintf(new_text, "%8.2f", value);
    XtRemoveCallbacks(cld, XmNvalueChangedCallback, valueCB);
    XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
                  GridGapTextCallback, (XtPointer) NULL);
/*
    XtAddCallback(cld, XmNmodifyVerifyCallback,
                  GridGapTextVerifyCallback, (XtPointer) NULL);
*/
    Global.gridattr.gridgap = value;

    redrawgrid(XGD_GRIDGAP);
}



static XtCallbackRec svalueCB[] = {
    {(XtCallbackProc)GridSpacingTextCallback, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};




void
#ifdef _NO_PROTO
GridSpacingTextCallback(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
GridSpacingTextCallback( Widget w, XtPointer cld, XtPointer cad)
#endif
{
    char *text = XmTextFieldGetString(w);
    int value = atoi(text);

    if ( value < 0) {
        char            new_text[10];

        XgWarningDialog(Global.applShell, "Line width exceeds size of page");
	sprintf(new_text, "%d", Global.gridattr.gridgap);
	XtRemoveCallbacks(w, XmNvalueChangedCallback, valueCB);
	XtRemoveCallbacks(w, XmNmodifyVerifyCallback, verifyCB);
	XmTextFieldSetString(w, new_text);
	XtAddCallback(w, XmNvalueChangedCallback,
		      GridSpacingTextCallback, (XtPointer) NULL);
	XtAddCallback(w, XmNmodifyVerifyCallback,
		      GridGapTextVerifyCallback, (XtPointer) NULL);
        return;
    }
    Global.gridattr.spacing = value;

    redrawgrid(XGD_GRIDSPACING);
}


void
#ifdef _NO_PROTO
GridSpacingDecrementCallBack(w, cld, cad)
Widget w;
Widget cld;
XtPointer cad;
#else
GridSpacingDecrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
    char           *text;
    int		    value;
    char            new_text[10];

    text = XmTextFieldGetString(cld);
    value = atoi (text);

    if ( value < 0 ) {
        XgWarningDialog(Global.applShell, "Grid Spacing is less than zero");
	sprintf(new_text, "%d", Global.gridattr.spacing);
	XtRemoveCallbacks(cld, XmNvalueChangedCallback, svalueCB);
	XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
	XmTextFieldSetString(cld, new_text);
	XtAddCallback(cld, XmNvalueChangedCallback,
		      GridSpacingTextCallback, (XtPointer) NULL);
	XtAddCallback(cld, XmNmodifyVerifyCallback,
		      GridGapTextVerifyCallback, (XtPointer) NULL);
        return;
    }
    if ( value == 0 ) {
	XBell(XtDisplay(w), 0);
        return;
    } else {
        value--;
    }
    sprintf(new_text, "%d", value);
    XtRemoveCallbacks(cld, XmNvalueChangedCallback, svalueCB);
    XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
                  GridSpacingTextCallback, (XtPointer) NULL);
    XtAddCallback(cld, XmNmodifyVerifyCallback,
                  GridGapTextVerifyCallback, (XtPointer) NULL);
    Global.gridattr.spacing = value;

    redrawgrid(XGD_GRIDSPACING);
}



void
#ifdef _NO_PROTO
GridSpacingIncrementCallBack(w, cld, cad)
Widget w;
Widget cld;
XtPointer cad;
#else
GridSpacingIncrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
    char           *text;
    int		   value;
    char           new_text[10];

    text = XmTextFieldGetString(cld);
    value = atoi(text);
    value++;

    sprintf(new_text, "%d", value);
    XtRemoveCallbacks(cld, XmNvalueChangedCallback, svalueCB);
    XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
                  GridSpacingTextCallback, (XtPointer) NULL);
    XtAddCallback(cld, XmNmodifyVerifyCallback,
                  GridGapTextVerifyCallback, (XtPointer) NULL);
    Global.gridattr.spacing = value;

    redrawgrid(XGD_GRIDSPACING);
}




static XtCallbackRec lwvalueCB[] = {
    {(XtCallbackProc)GridLineWidthTextCallback, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};




void
#ifdef _NO_PROTO
GridLineWidthTextCallback(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
GridLineWidthTextCallback( Widget w, XtPointer cld, XtPointer cad)
#endif
{
    char *text = XmTextFieldGetString(w);
    int value = atoi(text);

    if ( value < 0) {
        char            new_text[10];

        XgWarningDialog(Global.applShell, "Line width exceeds size of page");
	sprintf(new_text, "%d", Global.gridattr.gridgap);
	XtRemoveCallbacks(w, XmNvalueChangedCallback, valueCB);
	XtRemoveCallbacks(w, XmNmodifyVerifyCallback, verifyCB);
	XmTextFieldSetString(w, new_text);
	XtAddCallback(w, XmNvalueChangedCallback,
		      GridLineWidthTextCallback, (XtPointer) NULL);
	XtAddCallback(w, XmNmodifyVerifyCallback,
		      GridGapTextVerifyCallback, (XtPointer) NULL);
        return;
    }
    Global.gridattr.linewidth= value;

    redrawgrid(XGD_GRIDLINEWIDTH);
}


void
#ifdef _NO_PROTO
GridLineWidthDecrementCallBack(w, cld, cad)
Widget w;
Widget cld;
XtPointer cad;
#else
GridLineWidthDecrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
    char           *text;
    int		    value;
    char            new_text[10];

    text = XmTextFieldGetString(cld);
    value = atoi (text);

    if ( value < 0 ) {
        XgWarningDialog(Global.applShell, "Grid Line Width is less than zero");
	sprintf(new_text, "%d", Global.gridattr.spacing);
	XtRemoveCallbacks(cld, XmNvalueChangedCallback, lwvalueCB);
	XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
	XmTextFieldSetString(cld, new_text);
	XtAddCallback(cld, XmNvalueChangedCallback,
		      GridLineWidthTextCallback, (XtPointer) NULL);
	XtAddCallback(cld, XmNmodifyVerifyCallback,
		      GridGapTextVerifyCallback, (XtPointer) NULL);
        return;
    }
    if ( value == 0 ) {
	XBell(XtDisplay(w), 0);
        return;
    } else {
        value--;
    }
    sprintf(new_text, "%d", value);
    XtRemoveCallbacks(cld, XmNvalueChangedCallback, lwvalueCB);
    XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
                  GridLineWidthTextCallback, (XtPointer) NULL);
    XtAddCallback(cld, XmNmodifyVerifyCallback,
                  GridGapTextVerifyCallback, (XtPointer) NULL);
    Global.gridattr.linewidth= value;

    redrawgrid(XGD_GRIDLINEWIDTH);
}



void
#ifdef _NO_PROTO
GridLineWidthIncrementCallBack(w, cld, cad)
Widget w;
Widget cld;
XtPointer cad;
#else
GridLineWidthIncrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
    char           *text;
    int 	   value;
    char           new_text[10];

    text = XmTextFieldGetString(cld);
    value = atoi(text);
    value++;

    sprintf(new_text, "%d", value);
    XtRemoveCallbacks(cld, XmNvalueChangedCallback, lwvalueCB);
    XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
                  GridLineWidthTextCallback, (XtPointer) NULL);
    XtAddCallback(cld, XmNmodifyVerifyCallback,
                  GridGapTextVerifyCallback, (XtPointer) NULL);
    Global.gridattr.linewidth= value;

    redrawgrid(XGD_GRIDLINEWIDTH);
}

