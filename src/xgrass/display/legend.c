#include "xgdisp.h"

static char  **SelectedItems;
static int    *SelectedNums;
static Boolean UseSelectedItems = 0;
static Widget  legendList;
static Widget  togButton;
static Widget  fgColor, fgColorFrame;
static Widget  bColor, bColorFrame;  
static Widget  columnSpacing;
static Widget   tog1, tog2, tog3;

static void BorderColorCB(
#ifndef _NO_PROTO
     Widget w,
     Pixel cld,
     XtPointer cad
#endif
);

static void TextColorCB(
#ifndef _NO_PROTO
     Widget w,
     Pixel cld,
     XtPointer cad
#endif
);

void
#ifdef _NO_PROTO
SetLegendObject(obj)
     XgdObject *obj;
#else
SetLegendObject(XgdObject *obj)
#endif
{
  int i, pos = 0;
  XmString xs;
  char *name;
  char val[20];
  
  if (legendList) {
    XmListDeleteAllItems(legendList);
    
    for (i = 0; i <= obj->Obj.Legend.cats.num; i++){
      name = G_get_cat(i, &obj->Obj.Legend.cats);
      xs = XmStringCreateSimple(name);
      XmListAddItem(legendList, xs, 0);
      if (obj->Obj.Legend.catsToList)
	if (obj->Obj.Legend.catNums[pos] == i){
	  ++pos;
	  XmListSelectPos(legendList, i, False);
	  XmStringFree(xs);
	}
    }
    
    SetOptMenuToPixel(fgColor, obj->fg);
    SetOptMenuToPixel(bColor, obj->Obj.Legend.brdrcolor);
    
    sprintf(val, "%d", obj->Obj.Legend.numCols);
    XmTextFieldSetString(columnSpacing , val);
    if (obj->Obj.Legend.displaySelected)
      XmToggleButtonSetState(togButton, True, False);	
    else
      XmToggleButtonSetState(togButton, False, False);

    if (obj->Obj.Legend.toDisplay == XGD_DISPLAY_CAT_NAMES)
      XmToggleButtonSetState(tog1, True, True);
    else if (obj->Obj.Legend.toDisplay == XGD_DISPLAY_CAT_NUMS)
      XmToggleButtonSetState(tog2, True, True);
    else
      XmToggleButtonSetState(tog3, True, True);
  }
}

void
#ifdef _NO_PROTO
LegendBoxCB(w, cli, call)
     Widget    w;
     XtPointer cli;
     XtPointer call;
#else
LegendBoxCB(Widget w, XtPointer cli, XtPointer call)
#endif
{
  Widget box;

  if (SelectedObjectCount() == 1){
     if (Global.selectedObjects->object->type == XGD_LEGEND){

       box = CreateLegendBox(Global.selectedObjects->object);
       
       XtManageChild(box);
     } else 
       XgError(Global.applShell, "Selected Object must be a Legend");
   } else
     XgError(Global.applShell, "Select only 1 legend object.");
}

Widget
#ifdef _NO_PROTO
CreateLegendBox(obj)
     XgdObject *obj;
#else
CreateLegendBoxCB(XgdObject *obj)
#endif
{
  Widget   xgi;
  Widget   form;
  Widget   fontButton;
  Widget   ok;
  XmString xs;
  Arg      args[10];
  int      n = 0, i;
  char    *name;
  Widget   form2, caption, frame, downArrow, upArrow;
  Widget   rowcol1, rowcol2;
  Widget   frame2;
  
  XtSetArg(args[n], XmNcancelLabelString, XmStringCreateSimple("Dismiss"));++n;
  
  xgi = XgCreateInteractorDialog(Global.applShell, "XGRASS Legend Attributes",
				 args, n);

  form = XtVaCreateManagedWidget("LegendAttForm", xmFormWidgetClass, xgi,
				 XmNresizable, False,
				 NULL);

  togButton = XtVaCreateManagedWidget("Display Selected Catagories",
				      xmToggleButtonWidgetClass, form,
				      XmNleftAttachment, XmATTACH_FORM,
				      NULL);
  
  XtAddCallback(togButton, XmNvalueChangedCallback, LegendToggleCallBack,NULL);
  
  n = 0;
  XtSetArg(args[n], XmNitemCount, 0); ++n;
  XtSetArg(args[n], XmNvisibleItemCount,
	   obj->Obj.Legend.cats.num < 10 ? obj->Obj.Legend.cats.num : 10); ++n;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); ++n;
  XtSetArg(args[n], XmNselectionPolicy, XmMULTIPLE_SELECT); ++n;
  XtSetArg(args[n], XmNtopWidget, togButton); ++n;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); ++n;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); ++n;
  legendList = XmCreateScrolledList(form, "LegendList", args, n);
  for (i = 0; i <= obj->Obj.Legend.cats.num; i++){
    name = G_get_cat(i, &obj->Obj.Legend.cats);
    xs = XmStringCreateSimple(name);
    XmListAddItem(legendList, xs, 0);
    XmStringFree(xs);
  }
  XtManageChild(legendList);
  XtAddCallback(legendList, XmNmultipleSelectionCallback, LegendListCallBack, NULL);
  XtAddCallback(legendList, XmNdefaultActionCallback, LegendListCallBack, NULL);
  
  fgColorFrame = CreateColorOptionMenu(form, &fgColor, "Text Color:",
				       TextColorCB);

  XtVaSetValues(fgColorFrame,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, legendList,
		XmNleftAttachment, XmATTACH_FORM,
		NULL);

  SetOptMenuToItem(fgColor, "black");


  bColorFrame = CreateColorOptionMenu(form, &bColor, "Border Color:",
				       BorderColorCB);

  XtVaSetValues(bColorFrame,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, fgColorFrame,
		XmNleftAttachment, XmATTACH_FORM,
		NULL);

  SetOptMenuToItem(bColor, "black");
  
  fontButton = XtVaCreateManagedWidget("Fonts ...",
				       xmPushButtonWidgetClass, form,
				       XmNtopAttachment, XmATTACH_WIDGET,
				       XmNtopWidget, bColorFrame,
				       XmNleftAttachment, XmATTACH_FORM,
				       NULL);

  xs = XmStringCreateSimple("Number of columns:");

  rowcol1 = XtVaCreateManagedWidget("rowcol", xmRowColumnWidgetClass,
				    form,
				    XmNorientation, XmHORIZONTAL,
				    XmNtopAttachment, XmATTACH_WIDGET,
				    XmNtopWidget, fontButton,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNrightAttachment, XmATTACH_FORM,
				    NULL);
  
  caption = XtVaCreateManagedWidget("line_width_caption", 
                                    xbaeCaptionWidgetClass, rowcol1,
                                    XmNlabelPosition, XbaePositionTop,
                                    XmNlabelAlignment, XbaeAlignmentCenter,
                                    XmNlabelString, xs,
                                    NULL);
  XmStringFree(xs);

  
  frame = XtVaCreateManagedWidget("linewidth_frame",
                                  xmFrameWidgetClass, caption,
                                  NULL);

  form2 = XtVaCreateManagedWidget("linewidth_form",
				 xmFormWidgetClass, frame, NULL);


  upArrow = XtVaCreateManagedWidget("up_arrow_button",
				    xmArrowButtonWidgetClass, form2,
				    XmNarrowDirection, XmARROW_UP,
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_FORM,
				    NULL);

  downArrow = XtVaCreateManagedWidget("down_arrow_button",
				      xmArrowButtonWidgetClass, form2,
				      XmNarrowDirection, XmARROW_DOWN,
				      XmNleftAttachment, XmATTACH_FORM,
				      XmNbottomAttachment, XmATTACH_FORM,
				      NULL);

  columnSpacing = XtVaCreateManagedWidget("line_width_text",
					  xmTextFieldWidgetClass, form2,
					  XmNtopAttachment, XmATTACH_FORM,
					  XmNrightAttachment, XmATTACH_FORM,
					  XmNleftAttachment, XmATTACH_WIDGET,
					  XmNleftWidget, upArrow,
					  XmNbottomAttachment, XmATTACH_FORM,
					  XmNcolumns, 8,
					  NULL);
      
  XmTextFieldInsert(columnSpacing, 0, "1");
  XtAddCallback(upArrow, XmNactivateCallback,
		ColumnSpacingIncrementCallBack, (XtPointer)columnSpacing);
      
  XtAddCallback(downArrow, XmNactivateCallback,
		ColumnSpacingDecrementCallBack, (XtPointer)columnSpacing);
      
/*  XtAddCallback(columnSpacing, XmNactivateCallback,
		ColumnSpacingTextCallback, (XtPointer)NULL);
      
  XtAddCallback(columnSpacing, XmNvalueChangedCallback,
		ColumnSpacingTextCallback, (XtPointer)NULL);
      
  XtAddCallback(columnSpacing, XmNmodifyVerifyCallback,
		ColumnTextVerifyCallback, (XtPointer)NULL);
      
*/
  xs = XmStringCreateSimple("Text to display:");
  
  caption = XtVaCreateManagedWidget("line_width_caption", 
                                    xbaeCaptionWidgetClass, rowcol1,
                                    XmNlabelPosition, XbaePositionTop,
                                    XmNlabelAlignment, XbaeAlignmentCenter,
                                    XmNlabelString, xs,
                                    NULL);
  XmStringFree(xs);

  frame2 = XtVaCreateManagedWidget("display_frame",
                                  xmFrameWidgetClass, caption,
                                  NULL);

  rowcol2 = XtVaCreateManagedWidget("rowcol2", xmRowColumnWidgetClass, frame2,
				    XmNorientation, XmVERTICAL,
				    XmNradioBehavior, True,
				    NULL);

  tog1 = XtVaCreateManagedWidget("Category names", xmToggleButtonWidgetClass,
				 rowcol2,
				 NULL);

  XtAddCallback(tog1, XmNvalueChangedCallback, LegendDisplayToggleCallBack,
		XGD_DISPLAY_CAT_NAMES);
  
  XmToggleButtonSetState(tog1, True, True);
  
  tog2 = XtVaCreateManagedWidget("Category numbers", xmToggleButtonWidgetClass,
				 rowcol2,
				 NULL);

  XtAddCallback(tog2, XmNvalueChangedCallback, LegendDisplayToggleCallBack,
		XGD_DISPLAY_CAT_NUMS);

  tog3 = XtVaCreateManagedWidget("Nothing", xmToggleButtonWidgetClass,
				 rowcol2,
				 NULL);
  
  XtAddCallback(tog3, XmNvalueChangedCallback, LegendDisplayToggleCallBack,
		XGD_DISPLAY_CAT_NONE);

  XtAddCallback(fontButton, XmNactivateCallback, SetFontCallBack, XGD_LEGEND);

  ok = XgInteractorGetChild(xgi, XmINTERACT_OK_BUTTON);

  XtUnmanageChild(ok);

  SelectedItems = NULL;
  XgdSetLegendsDisplayList(Global.selectedObjects->object, SelectedItems);
  return(xgi);
}

void
#ifdef _NO_PROTO
LegendDisplayToggleCallBack(w, cli, call)
     Widget    w;
     int       cli;
     XtPointer call;
#else
LegendDisplayToggleCallBack(Widget w, int cli, XtPointer call)
#endif
{
  if (cli != Global.selectedObjects->object->Obj.Legend.toDisplay){
    Global.selectedObjects->object->Obj.Legend.toDisplay = cli;
    XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
    XgdUnDrawObject(Global.selectedObjects->object, 
		    Global.selectedObjects->object->bg, True);
    XgdDrawObject(XgdGetGCOfObject(Global.selectedObjects->object),
		  Global.selectedObjects->object, True, NULL);
    XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
  }
}

void
#ifdef _NO_PROTO
LegendToggleCallBack(w, cli, call)
     Widget    w;
     XtPointer cli;
     XtPointer call;
#else
LegendToggleCallBack(Widget w, XtPointer cli, XtPointer call)
#endif
{
  Global.selectedObjects->object->Obj.Legend.displaySelected =
    !Global.selectedObjects->object->Obj.Legend.displaySelected; 

  XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
  XgdUnDrawObject(Global.selectedObjects->object,
		  XgdGetGCOfObject(Global.selectedObjects->object),
		  True);
  XgdSetLegendsDisplayList(Global.selectedObjects->object,
			   Global.selectedObjects->object->Obj.Legend.displaySelected?SelectedItems:NULL, SelectedNums);
  XgdDrawObject(XgdGetGCOfObject(Global.selectedObjects->object),
		Global.selectedObjects->object, True, NULL);
  XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);

}

void
#ifdef _NO_PROTO
LegendListCallBack(w, cli, call)
     Widget    w;
     XtPointer cli;
     XmListCallbackStruct *call;
#else
LegendListCallBack(Widget w, XtPointer cli, XmListCallbackStruct *call)
#endif
{
  int i;

  if (SelectedItems){
    SelectedItems = NULL;
    SelectedNums = NULL;
  }
  
  if (call->reason == XmCR_MULTIPLE_SELECT){
    SelectedItems =(char **)
      XtMalloc(sizeof(char *) * (call->selected_item_count + 1));
    SelectedNums = (int *) XtMalloc(sizeof(int) * call->selected_item_count);
    for (i = 0; i < call->selected_item_count; i++){
      XmStringGetLtoR(call->selected_items[i], XmSTRING_DEFAULT_CHARSET,
		      &SelectedItems[i]);
      SelectedNums[i] = call->selected_item_positions[i];
    }
    SelectedItems[i] = NULL;
    if (Global.selectedObjects->object->Obj.Legend.displaySelected){
      XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
      XgdUnDrawObject(Global.selectedObjects->object,
		      XgdGetGCOfObject(Global.selectedObjects->object),
		      True);
      
      XgdSetLegendsDisplayList(Global.selectedObjects->object, SelectedItems,
			       SelectedNums);
      
      XgdDrawObject(XgdGetGCOfObject(Global.selectedObjects->object),
		    Global.selectedObjects->object, True, NULL);
      XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
    }
  }
}

static void 
#ifdef _NO_PROTO
TextColorCB(w, cld, cad)
Widget w;
Pixel cld; 
XtPointer cad;
#else
TextColorCB(Widget w, Pixel cld, XtPointer cad)
#endif
{
  if (Global.selectedObjects == NULL)
    return;
  
  if ( SelectedObjectCount() == 1 &&
      Global.selectedObjects->object->type==XGD_LABEL ||
      Global.selectedObjects->object->type==XGD_LEGEND)
    {
      XgdUnDrawObject(Global.selectedObjects->object,
		      Global.selectedObjects->object->bg,
		      True);
      XgdSetObjectForeground(Global.selectedObjects->object, cld);
      XgdDrawObject(XgdGetGCOfObject(Global.selectedObjects->object),
		    Global.selectedObjects->object, True, NULL);
    } 
}

static void 
#ifdef _NO_PROTO
BorderColorCB(w, cld, cad)
     Widget w;
     Pixel cld; 
     XtPointer cad;
#else
BorderColorCB(Widget w, Pixel cld, XtPointer cad)
#endif
{
  if (Global.selectedObjects == NULL)
    return;

  if ( SelectedObjectCount() == 1 &&
      Global.selectedObjects->object->type==XGD_LABEL ||
      Global.selectedObjects->object->type==XGD_LEGEND) {
    XgdUnDrawObject(Global.selectedObjects->object,
		    Global.selectedObjects->object->bg,
		    True);
    XgdSetLegendBorderColor(Global.selectedObjects->object, cld);
    XgdDrawObject(XgdGetGCOfObject(Global.selectedObjects->object),
		  Global.selectedObjects->object, True, NULL);
  } 
}

void
#ifdef _NO_PROTO
ColumnSpacingIncrementCallBack(w, cld, cad)
     Widget w;
     Widget cld;
     XtPointer cad;
#else
ColumnSpacingIncrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
  char           *text;
  int		  value;
  char            new_text[10];
  
  text = XmTextFieldGetString(cld);
  value = atoi(text);
  value++;
  
  sprintf(new_text, "%d", value);
  XmTextFieldSetString(cld, new_text);
  XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
  XgdUnDrawObject(Global.selectedObjects->object,
		  Global.selectedObjects->object->bg, True);
  Global.selectedObjects->object->Obj.Legend.numCols = value;
  XgdDrawObject(XgdGetGCOfObject(Global.selectedObjects->object),
		Global.selectedObjects->object, True, NULL);
  XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
}


void
#ifdef _NO_PROTO
ColumnSpacingDecrementCallBack(w, cld, cad)
Widget w;
Widget cld;
XtPointer cad;
#else
ColumnSpacingDecrementCallBack(Widget w, Widget cld, XtPointer cad)
#endif
{
  char           *text;
  int		    value;
  char            new_text[10];
  
  text = XmTextFieldGetString(cld);
  value = atoi (text);
  
  if ( value < 1 ) {
    XgWarningDialog(Global.applShell, "You must have at least 1 column");
    sprintf(new_text, "%d", 1);
    XmTextFieldSetString(cld, new_text);
    return;
  }

  if ( value == 1 ) {
    XBell(XtDisplay(w), 0);
    return;
  } else {
    value--;
  }
  sprintf(new_text, "%d", value);
  XmTextFieldSetString(cld, new_text);
  XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
  XgdUnDrawObject(Global.selectedObjects->object,
		  Global.selectedObjects->object->bg, True);
  Global.selectedObjects->object->Obj.Legend.numCols = value;
  XgdDrawObject(XgdGetGCOfObject(Global.selectedObjects->object),
		Global.selectedObjects->object, True, NULL);
  XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
}

