#include "xgdisp.h"

void
#ifdef _NO_PROTO
redrawstdsite(type)
     int type;
#else
redrawstdsite(int type)
#endif
{
  if ( Global.selectedObjects == NULL)
    return;
  
  if ( Global.selectedObjects->object && SelectedObjectCount() == 1 &&
      Global.selectedObjects->object->type == XGD_GEOFRAME &&
      Global.selectedObjects->object->Obj.GeoFrame.sites.site != NULL) {
    XgdSiteInfo *tmp;
    XgdSiteInfo *sinfo;
    tmp = Global.selectedObjects->object->Obj.GeoFrame.sites.site;
    
    sinfo = NULL;
    while (tmp != NULL) {
      if (tmp->type == XGD_SITE_STANDARD)
	if (!strcmp(tmp->sname,Global.sname) &&
	    !strcmp(tmp->smapset, Global.smapset))
	  sinfo = tmp;
      
      tmp=tmp->next;
    }
    
    if (sinfo == NULL) return;
    
    switch (type)
      {
      case XGD_SITESIZE:
	XgdSetStdSiteSize(sinfo, Global.stdsiteattr.size);
	break;
      case XGD_SITELW:
	XgdSetStdSiteWidth(sinfo, Global.stdsiteattr.width);
	break;
      }

    _xgdDrawSiteStd(Global.selectedObjects->object, sinfo);
    XgdRedrawGeoframe(Global.selectedObjects->object, True, True, NULL);
    XgdDrawObject(Global.selectedObjects->object->objgc,
		  Global.selectedObjects->object, True, NULL);
  }
}

Widget
#ifdef _NO_PROTO
CreateStdSiteGadget(parent, string, type)
     Widget parent;
     char *string;
     int  type;
#else
CreateStdSiteGadget( Widget parent, char *string, int type)
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
  
  textField = XtVaCreateManagedWidget("text",
				      xmTextFieldWidgetClass, form,
				      XmNtopAttachment, XmATTACH_FORM,
				      XmNrightAttachment, XmATTACH_FORM,
				      XmNleftAttachment, XmATTACH_WIDGET,
				      XmNleftWidget, upArrow,
				      XmNbottomAttachment, XmATTACH_FORM,
				      XmNcolumns, 8,
				      NULL);
  
  switch (type)
    {
    case XGD_SITESIZE:
      {
	stdsitesizew = textField;
	
	XmTextFieldInsert(stdsitesizew, 0, "5");
	
	XtAddCallback(upArrow, XmNactivateCallback, 
		      SiteSizeIncrementCallBack, (XtPointer)stdsitesizew);
	
	XtAddCallback(downArrow, XmNactivateCallback, 
		      SiteSizeDecrementCallBack, (XtPointer)stdsitesizew);
	
	XtAddCallback(stdsitesizew, XmNactivateCallback,
		      SiteSizeTextCallBack, (XtPointer) NULL);
	XtAddCallback(stdsitesizew, XmNvalueChangedCallback,
		      SiteSizeTextCallBack, (XtPointer) NULL);
	/*
	  XtAddCallback(stdsitesizew, XmNmodifyVerifyCallback,
	  GridGapTextVerifyCallback, (XtPointer) NULL);
	  */
	
      }
      break;
      
    case XGD_SITELW:
      stdsitelww = textField;
      
      XmTextFieldInsert(stdsitelww, 0, "1");
      XtAddCallback(upArrow, XmNactivateCallback,
		    SiteLWIncrementCallBack, (XtPointer)stdsitelww);
      
      XtAddCallback(downArrow, XmNactivateCallback,
		    SiteLWDecrementCallBack, (XtPointer)stdsitelww);
      
      XtAddCallback(stdsitelww, XmNactivateCallback,
		    SiteLWTextCallBack, (XtPointer)NULL);
      
      XtAddCallback(stdsitelww, XmNvalueChangedCallback,
		    SiteLWTextCallBack, (XtPointer)NULL);
      
      XtAddCallback(stdsitelww, XmNmodifyVerifyCallback,
		    GridGapTextVerifyCallback, (XtPointer)NULL);
      
      break;
    }

  return caption;
}

static XtCallbackRec valueCB[] = {
  {(XtCallbackProc)SiteSizeTextCallBack, (XtPointer) NULL},
  {(XtCallbackProc) NULL, NULL}
};

void
#ifdef _NO_PROTO
SiteSizeTextCallBack(w, cld, cad)
     Widget w;
     XtPointer cld, cad;
#else
SiteSizeTextCallBack( Widget w, XtPointer cld, XtPointer cad)
#endif
{
  int value;
  char *text = XmTextFieldGetString(w);
  
  value = atoi(text);
  
  if ( value < 0 ) {
    char   new_text[10];

    XgWarningDialog(Global.applShell, "Line width exceeds size of page");
    sprintf(new_text, "%d", Global.stdsiteattr.size);
    XtRemoveCallbacks(w, XmNvalueChangedCallback, valueCB);
    /*
      XtRemoveCallbacks(w, XmNmodifyVerifyCallback, verifyCB);
      */
    XmTextFieldSetString(w, new_text);
    XtAddCallback(w, XmNvalueChangedCallback,
		  SiteSizeTextCallBack, (XtPointer) NULL);
    /*
      XtAddCallback(w, XmNmodifyVerifyCallback,
      GridGapTextVerifyCallback, (XtPointer) NULL);
      */
    return;
  }
  Global.stdsiteattr.size = value;
  redrawstdsite(XGD_SITESIZE);
}

void
#ifdef _NO_PROTO
SiteSizeDecrementCallBack(w, cld, cad)
     Widget w;
     Widget cld;
     XtPointer cad;
#else
SiteSizeDecrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
  char       *text;
  int		value;
  char       new_text[10];
  
  text = XmTextFieldGetString(cld);
  value = atoi (text);
  
  if ( value < 0 ) {
    
    XgWarningDialog(Global.applShell, "Grid Gap is less than zero");
    sprintf(new_text, "%d", Global.stdsiteattr.size);
    XtRemoveCallbacks(cld, XmNvalueChangedCallback, valueCB);
    /*
      XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
*/
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
		  GridGapTextCallback, (XtPointer) NULL);
    /*
      XtAddCallback(cld, XmNmodifyVerifyCallback,
      GridGapTextVerifyCallback, (XtPointer) NULL);
      */
        return;
  }
  if ( value == 0 ) {
    XBell(XtDisplay(w), 0);
    return;
  } else {
    value = value - 1;
  }
  sprintf(new_text, "%d", value);
  XtRemoveCallbacks(cld, XmNvalueChangedCallback, valueCB);
  /*
    XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
    */
  XmTextFieldSetString(cld, new_text);
  XtAddCallback(cld, XmNvalueChangedCallback,
		SiteSizeTextCallBack, (XtPointer) NULL);
  XtAddCallback(cld, XmNmodifyVerifyCallback,
		GridGapTextVerifyCallback, (XtPointer) NULL);
  Global.stdsiteattr.size = value;
  redrawstdsite(XGD_SITESIZE);
}






void
#ifdef _NO_PROTO
SiteSizeIncrementCallBack(w, cld, cad)
Widget w;
Widget cld;
XtPointer cad;
#else
SiteSizeIncrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
    char        *text;
    int		value;
    char        new_text[10];

    text = XmTextFieldGetString(cld);
    value = atoi(text);

    value = value + 1;
    sprintf(new_text, "%d", value);
    XtRemoveCallbacks(cld, XmNvalueChangedCallback, valueCB);
/*
    XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
*/
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
                  SiteSizeTextCallBack, (XtPointer) NULL);
/*
    XtAddCallback(cld, XmNmodifyVerifyCallback,
                  GridGapTextVerifyCallback, (XtPointer) NULL);
*/
    Global.stdsiteattr.size = value;
    redrawstdsite(XGD_SITESIZE);
}


static XtCallbackRec svalueCB[] = {
    {(XtCallbackProc)SiteLWTextCallBack, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};


void
#ifdef _NO_PROTO
SiteLWTextCallBack(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
SiteLWTextCallBack( Widget w, XtPointer cld, XtPointer cad)
#endif
{
    char *text = XmTextFieldGetString(w);
    int value = atoi(text);

    if ( value < 0) {
        char            new_text[10];

        XgWarningDialog(Global.applShell, "Line width exceeds size of page");
	sprintf(new_text, "%d", Global.stdsiteattr.width);
	XtRemoveCallbacks(w, XmNvalueChangedCallback, valueCB);
/*
	XtRemoveCallbacks(w, XmNmodifyVerifyCallback, verifyCB);
*/
	XmTextFieldSetString(w, new_text);
	XtAddCallback(w, XmNvalueChangedCallback,
		      SiteLWTextCallBack, (XtPointer) NULL);
	XtAddCallback(w, XmNmodifyVerifyCallback,
		      GridGapTextVerifyCallback, (XtPointer) NULL);
        return;
    }
    Global.stdsiteattr.width = value;
    redrawstdsite(XGD_SITELW);
}


void
#ifdef _NO_PROTO
SiteLWDecrementCallBack(w, cld, cad)
Widget w;
Widget cld;
XtPointer cad;
#else
SiteLWDecrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
    char           *text;
    int		    value;
    char            new_text[10];

    text = XmTextFieldGetString(cld);
    value = atoi (text);

    if ( value < 0 ) {
        XgWarningDialog(Global.applShell, "Icon Line Width is less than zero");
	sprintf(new_text, "%d", Global.stdsiteattr.width);
	XtRemoveCallbacks(cld, XmNvalueChangedCallback, svalueCB);
/*
	XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
*/
	XmTextFieldSetString(cld, new_text);
	XtAddCallback(cld, XmNvalueChangedCallback,
		      SiteLWTextCallBack, (XtPointer) NULL);
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
/*
    XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
*/
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
                  SiteLWTextCallBack, (XtPointer) NULL);
    XtAddCallback(cld, XmNmodifyVerifyCallback,
                  GridGapTextVerifyCallback, (XtPointer) NULL);
    Global.stdsiteattr.width = value;
    redrawstdsite(XGD_SITELW);
}



void
#ifdef _NO_PROTO
SiteLWIncrementCallBack(w, cld, cad)
Widget w;
Widget cld;
XtPointer cad;
#else
SiteLWIncrementCallBack(Widget w, Widget cld, XtPointer cad)
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
/*
    XtRemoveCallbacks(cld, XmNmodifyVerifyCallback, verifyCB);
*/
    XmTextFieldSetString(cld, new_text);
    XtAddCallback(cld, XmNvalueChangedCallback,
                  SiteLWTextCallBack, (XtPointer) NULL);
    XtAddCallback(cld, XmNmodifyVerifyCallback,
                  GridGapTextVerifyCallback, (XtPointer) NULL);
    Global.stdsiteattr.width = value;
    redrawstdsite(XGD_SITELW);

}
