#include "xgdisp.h"

void
#ifdef _NO_PROTO
GridToggleCallBack(w, cld, cbs)
Widget w;
XtPointer cld;
XmToggleButtonCallbackStruct *cbs;
#else
GridToggleCallBack( Widget w, XtPointer cld, XmToggleButtonCallbackStruct *cbs)
#endif
{
  if (Global.selectedObjects == NULL){
    XgWarningDialog(Global.applShell, "No GeoFrame selected.");
    return;
  } else if (SelectedObjectCount() != 1){
    XgWarningDialog(Global.applShell, "Select 1 GeoFrame only.");
    return;
  } else if (Global.selectedObjects->object->type != XGD_GEOFRAME){
    XgWarningDialog(Global.applShell, "Selected object must be a GeoFrame.");
    return;
  } else {
    if (Global.selectedObjects->object->Obj.GeoFrame.gridOn &&
	Global.selectedObjects->object->Obj.GeoFrame.grid.labelon){
      XmToggleButtonCallbackStruct cs;
      cbs->set = False;
      GridLabelToggleCallBack(w, cld, &cs);
      XmToggleButtonSetState(gridlabelonw, False, True);      
    }

    Global.selectedObjects->object->Obj.GeoFrame.gridOn = cbs->set;
    XgdRedrawGeoframe(Global.selectedObjects->object, True, True, NULL);
  }
}

void
#ifdef _NO_PROTO
GridLabelToggleCallBack(w, cld, cbs)
Widget w;
XtPointer cld;
XmToggleButtonCallbackStruct *cbs;
#else
GridLabelToggleCallBack( Widget w, XtPointer cld, XmToggleButtonCallbackStruct *cbs)
#endif
{
  if ( Global.selectedObjects == NULL)
    return;

  if ( Global.selectedObjects->object && SelectedObjectCount() == 1) {
    if (cbs->set && Global.selectedObjects->object->Obj.GeoFrame.gridOn) {
      Global.gridattr.labelon = 1;
      XgdSetGridLabelOn(Global.selectedObjects->object,Global.gridattr.labelon);
      XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
      DoGridLabel(Global.selectedObjects->object, 0, &Global.gridattr.xoff,
		  &Global.gridattr.yoff);
      DoGridLabel(Global.selectedObjects->object, 1, &Global.gridattr.xoff,
		  &Global.gridattr.yoff);
      XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
    } else if (!cbs->set &&Global.selectedObjects->object->Obj.GeoFrame.gridOn){
      XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);

      XgdUnDrawObject(Global.selectedObjects->object,
		      Global.selectedObjects->object->bg, True);
      Global.gridattr.labelon = 0;	
      Global.gridattr.xoff= 0;	
      Global.gridattr.yoff= 0;	
      
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
		    Global.gridattr.xoff, Global.gridattr.yoff) ;
      XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
    }
  }
}

	

Widget
#ifdef _NO_PROTO
CreateGridToolBox()
#else
CreateGridToolBox(void)
#endif
{
  Widget xgi;
  Widget form;
  Widget button;
  Widget gridGapFrame, gridSpacingFrame;
  Widget linePatMenuFrame;
  Widget lineWidthFrame;
  Widget fgOptMenuFrame, textOptMenuFrame;
  Widget fgOptMenu, textOptMenu;
  Widget gridon, gridlabel ;
  Arg al[15];
  int ac = 0;
  XmString xms;
  
  /* interactor dialog parent */
  xms = XmStringCreateSimple("Dismiss");
  XtSetArg(al[ac], XmNenableWorkAreaStretch, True); ac++;
  XtSetArg(al[ac], XmNcancelLabelString, xms); ac++;
  xgi = XgCreateInteractorDialog(Global.applShell, "XGRASS Grid Attributes Box",
				 al, ac);
  XmStringFree(xms);
  XtUnmanageChild(XgInteractorGetChild(xgi,XmINTERACT_OK_BUTTON));
  
  form = XtVaCreateManagedWidget("grid_box_form",
				 xmFormWidgetClass, xgi,
				 NULL);
  
  /* do the number label on */
  
  gridon = XtVaCreateManagedWidget(
				   "grid_toggle",
				   xmToggleButtonWidgetClass, form,
				   XmNleftAttachment, XmATTACH_FORM,
				   XmNtopAttachment, XmATTACH_FORM,
				   NULL);
  
  XtAddCallback(gridon, XmNvalueChangedCallback, GridToggleCallBack, NULL);
  
  gridonw = gridon;
  
  
  gridlabel= XtVaCreateManagedWidget(
				     "label_toggle",
				     xmToggleButtonWidgetClass, form,
				     XmNleftAttachment, XmATTACH_WIDGET,
				     XmNleftWidget, gridon,
				     XmNtopAttachment,  XmATTACH_FORM,
				     XmNleftOffset,	10,
				     NULL);
  
  XtAddCallback(gridlabel, XmNvalueChangedCallback,
		GridLabelToggleCallBack, NULL);
  
  
  gridlabelonw = gridlabel;
  
  gridGapFrame= CreateGridGadget(form, "Set Grid Gap:", XGD_GRIDGAP);
  
  XtVaSetValues(gridGapFrame, 
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gridon,
		NULL);
  
  gridSpacingFrame= CreateGridGadget(form, "Set Grid Spacing:",XGD_GRIDSPACING);
  
  XtVaSetValues(gridSpacingFrame, 
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget,     gridGapFrame,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget,     gridon,
		NULL);
  
  /* do the line pattern buttons */
  linePatMenuFrame = XgdCreateLinePatternPad(form, "Set Line Pattern:",
        2, XmHORIZONTAL, GridLinePatternCallBack);
  
  XtVaSetValues(linePatMenuFrame, 
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gridGapFrame,
		NULL);
  
  /* do the line width gadget */
  lineWidthFrame = CreateGridGadget(form, "Set Line Width:", XGD_GRIDLINEWIDTH);

  XtVaSetValues(lineWidthFrame, 
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, linePatMenuFrame,
		NULL);

  /* do the foreground color option menus for grid lines */
  fgOptMenuFrame = CreateColorOptionMenu(form, &fgOptMenu, "Grid Color:",
					 GridForegroundColorCallBack);
  /* do the text color option menus for grid label text */
  textOptMenuFrame = CreateColorOptionMenu(form, &textOptMenu, "Label Color:",
					   GridTextColorCallBack);
  
  XtVaSetValues(fgOptMenuFrame, 
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, lineWidthFrame,
		XmNleftAttachment, XmATTACH_FORM,
		NULL);
  
  SetOptMenuToItem(fgOptMenu, "black");
  
  XtVaSetValues(textOptMenuFrame, 
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, fgOptMenuFrame,
		NULL);
  
  SetOptMenuToItem(textOptMenu, "black");
  
  button = XtVaCreateManagedWidget("grid_font", 
				   xmPushButtonGadgetClass, form, 
				   XmNleftAttachment, XmATTACH_FORM,
				   XmNtopAttachment, XmATTACH_WIDGET,
				   XmNtopWidget, textOptMenuFrame, NULL);
  
  XtAddCallback(button, XmNactivateCallback,  SetFontCallBack, XGD_GRID) ;
  
  XtManageChild(xgi);
  
  return(xgi);
}

void 
#ifdef _NO_PROTO
GridForegroundColorCallBack(w, cld, cad)
Widget w;
Pixel cld; 
XtPointer cad;
#else
GridForegroundColorCallBack(Widget w, Pixel cld, XtPointer cad)
#endif
{
  Global.gridattr.color = cld;
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
    if ( Global.selectedObjects->object)
      XgdUpdateGrid(Global.selectedObjects->object, 
		    XgdIsGridOn(Global.selectedObjects->object),
		    Global.gridattr.labelon,
		    Global.gridattr.gridgap,
		    Global.gridattr.spacing, Global.gridattr.color,
		    Global.gridattr.linewidth, Global.gridattr.linepattern,
		    Global.gridattr.fid, Global.gridattr.textcolor,
		    Global.selectedObjects->object->Obj.GeoFrame.grid.xoff,
		    Global.selectedObjects->object->Obj.GeoFrame.grid.yoff);
  }
}

void 
#ifdef _NO_PROTO
GridTextColorCallBack(w, cld, cad)
Widget w;
Pixel cld; 
XtPointer cad;
#else
GridTextColorCallBack(Widget w, Pixel cld, XtPointer cad)
#endif
{
  Global.gridattr.textcolor = cld;
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
	Global.selectedObjects->object->Obj.GeoFrame.grid.labelon) {

      XgdUpdateGrid(Global.selectedObjects->object,
		    XgdIsGridOn(Global.selectedObjects->object),
		    Global.gridattr.labelon,
		    Global.gridattr.gridgap,
		    Global.gridattr.spacing, Global.gridattr.color,
		    Global.gridattr.linewidth, Global.gridattr.linepattern,
		    Global.gridattr.fid, Global.gridattr.textcolor,
		    Global.gridattr.xoff, Global.gridattr.yoff);
      
      DoGridLabel(Global.selectedObjects->object, 2,&Global.gridattr.xoff,
		  &Global.gridattr.yoff); 
    }
  }
}



void
#ifdef _NO_PROTO
GridLinePatternCallBack(w, cld, cad)
Widget w;
int cld;
XtPointer cad;	
#else
GridLinePatternCallBack(Widget w, int cld, XtPointer cad)
#endif
{
  Global.gridattr.linepattern = cld;
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
    if ( Global.selectedObjects->object->Obj.GeoFrame.gridOn){ 
      XgdSetGridLinePattern(Global.selectedObjects->object,
			    Global.gridattr.linewidth,
			    Global.gridattr.linepattern);
      XgdRedrawGeoframe(Global.selectedObjects->object, True, True, NULL);
      XgdDrawObject(Global.selectedObjects->object->Obj.GeoFrame.grid.gc,
		    Global.selectedObjects->object, True, NULL);
    }
  }
}


