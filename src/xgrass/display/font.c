#include "xgdisp.h"

extern Widget labelAttText;

void
#ifdef _NO_PROTO
SetFontOkCallBack (w,cld,cad)
Widget	w;
int cld;
XtPointer cad;
#else
SetFontOkCallBack(Widget w, int cld, XtPointer cad)
#endif
{
  char		*result;
  XFontStruct 	*fontInfo;
  
  result = (char *)XmTextGetString(XgInteractorGetChild(w,
							XmINTERACT_LIST_TEXT));
  
  if (result == NULL) return;
  
  if ((fontInfo= XLoadQueryFont(Global.display, result)) ==NULL ) {
    char buf[1024];
    
    sprintf (buf, "Sorry, the selected font:\n[%s]\nIs not available.\n", 
	     result);
    XgWarningDialog(Global.applShell,buf);
    return;
  }
  
  switch (cld) {
  case XGD_DEFAULT:
    Global.fontStruct = fontInfo;
    if ( Global.fontName != NULL )  {
      XtFree(Global.fontName);
    }
    Global.fontName = XtNewString(result);
    break;
  case XGD_LEGEND:
    if ( Global.selectedObjects != NULL &&  SelectedObjectCount() == 1 && 
	Global.selectedObjects->object->type == XGD_LEGEND){
      XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
      XgdUnDrawObject(Global.selectedObjects->object,
		      Global.selectedObjects->object->bg, True);
      XgdSetLegendFont(Global.selectedObjects->object, fontInfo, True);
      XgdSetLegendFontName(Global.selectedObjects->object, result);
      XgdDrawObject(Global.selectedObjects->object->objgc,
		    Global.selectedObjects->object, True, NULL);
      XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
    }
    break;
  case XGD_BARSCALE:
    Global.barattr.fid = fontInfo->fid;
    if ( Global.selectedObjects != NULL &&
	SelectedObjectCount() == 1 && 
	Global.selectedObjects->object->type == XGD_BARSCALE) 
      {
	XgdSetBarscaleFont(Global.selectedObjects->object, Global.barattr.fid);
	XgdSetBarscaleFontName(Global.selectedObjects->object, result);
	XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
	RedrawArea(Global.selectedObjects->object);
	XgdDrawBarscale(Global.selectedObjects->object,	
			True);
	XgdUpdateBoxForObject(Global.selectedObjects->object,
			      Global.selectedObjects->object->y,
			      Global.selectedObjects->object->y + Global.selectedObjects->object->height, 
			      Global.selectedObjects->object->x,
			      Global.selectedObjects->object->x + Global.selectedObjects->object->width);
	XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
      }
    break;
  case XGD_GRID:
    {
      Global.gridattr.fid = fontInfo->fid;
      if ( Global.selectedObjects != NULL &&
	  SelectedObjectCount() == 1 &&
	  Global.selectedObjects->object->type == XGD_GEOFRAME ) {
	
	XgdSetGridFont(Global.selectedObjects->object, Global.gridattr.fid);
	XgdSetGridFontName(Global.selectedObjects->object, result);
	XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
	RedrawArea(Global.selectedObjects->object);
	XgdResizePixmap(Global.selectedObjects->object,
			Global.selectedObjects->object->width-Global.selectedObjects->object->Obj.GeoFrame.grid.xoff,
			Global.selectedObjects->object->height-Global.selectedObjects->object->Obj.GeoFrame.grid.yoff,
			Global.selectedObjects->object->Obj.GeoFrame.grid.xoff,
			Global.selectedObjects->object->Obj.GeoFrame.grid.yoff, False);
	
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
	XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
      }
    }
    break;
  case XGD_LABEL:
    {
      XmFontList fontlist;

      fontlist = XmFontListCreate(fontInfo, XmSTRING_DEFAULT_CHARSET);
      XtVaSetValues(labelAttText, XmNfontList, fontlist, NULL);
      if ( Global.selectedObjects != NULL &&  SelectedObjectCount() == 1 && 
	  Global.selectedObjects->object->type == XGD_LABEL){
	XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
	XgdUnDrawObject(Global.selectedObjects->object,
			Global.selectedObjects->object->bg, True, NULL);
	XgdSetLabelFont(Global.selectedObjects->object, fontInfo, True);
	XgdSetLabelFontName(Global.selectedObjects->object, result);
	XgdConfigureResizeHandles(Global.selectedObjects->object);
	XgdDrawObject(XgdGetGCOfObject(Global.selectedObjects->object),
		      Global.selectedObjects->object, True, NULL);
	XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
      }
    }
    break;
  }
  XtDestroyWidget(w);
}

void 
#ifdef _NO_PROTO
SetFontCallBack(w,cld,cad)
Widget w;
int cld;
XtPointer cad;
#else
SetFontCallBack(Widget w, int cld, XtPointer cad)
#endif
{
    Widget xgb ;
    Widget parent;
    int  ac;
    Arg  al[5];
    XmString xms;

/* BUG ALERT: parent should be set to the shell for other
 * than XGD_DEFAULT */
    switch (cld) {
    case XGD_DEFAULT:
	xms = XmStringCreateSimple("Please select a default font.");
        parent = Global.applShell;
        break;
    case XGD_LEGEND:
	xms = XmStringCreateSimple("Please select a legend font.");
        parent = Global.applShell;
	break;
    case XGD_BARSCALE:
	xms = XmStringCreateSimple("Please select a barscale font.");
        parent = Global.applShell;
	break;
    case XGD_GRID:
	xms = XmStringCreateSimple("Please select a grid font.");
        parent = Global.gridbox;
	break;
    case XGD_LABEL:
	xms = XmStringCreateSimple("Please select a label font.");
        parent = Global.applShell;
	break;
    }

    ac= 0;
    XtSetArg(al[ac], XmNpromptLabelString, xms); ac++;
    XtSetArg(al[ac], XmNnumLists, 1); ac++;
    XtSetArg(al[ac], XmNselMode, XG_SINGLE_SELECT); ac++;
    XtSetArg(al[ac], XmNautoUnmanage, True); ac++;
    xgb = XgCreateInteractorFontListDialog(parent,"XGRASS Font Dialog", al, ac);

    XtAddCallback(xgb, XmNokCallback, SetFontOkCallBack, cld);
    XmStringFree(xms);
    XtManageChild(xgb);
}
