#include "xgdisp.h"

static void TextColorCB(
#ifndef _NO_PROTO
    Widget    widget,
    Pixel     cli,
    XtPointer  call
#endif
);

static void LabelOkCB(
#ifndef _NO_PROTO
    Widget    widget,
    XgdObject *obj,
    XmAnyCallbackStruct *call
#endif
);

Widget           labelAttText;
static Boolean   labelAttUp = False;
static XgdObject *labelAttObject;
XFontStruct      *curFont;
static Pixel     curPixel;
static Widget    fgColor, fgColorFrame;

Widget
#ifdef _NO_PROTO
CreateLabelDialog(obj)
     XgdObject *obj;
#else
CreateLabelDialog(XgdObject *obj)
#endif
{
  Widget   xgi;
  Widget   form;
  Widget   fontButton;
  Widget   ok;
  Arg      args[10];
  int      n = 0;
  
  if (labelAttUp)
     return NULL;

  labelAttUp = True;
  
  XtSetArg(args[n], XmNcancelLabelString, XmStringCreateSimple("Dismiss"));++n;
  
  xgi = XgCreateInteractorDialog(Global.applShell, "XGRASS Label Attributes",
				 args, n);

  form = XtVaCreateManagedWidget("LabelAttForm", xmFormWidgetClass, xgi,
				 XmNresizable, False,
				 NULL);

  n = 0;
  XtSetArg(args[n], XmNeditable, True); ++n;
  XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); ++n;
  XtSetArg(args[n], XmNrows, 5); ++n;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); ++n;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); ++n;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); ++n;
  labelAttText = XmCreateScrolledText(form, "LabelText", args, n);

  XtManageChild(labelAttText);
  
  ChangeLabelAttObject(obj);
  
  fgColorFrame = CreateColorOptionMenu(form, &fgColor, "Text Color:",
				       TextColorCB);

  XtVaSetValues(fgColorFrame,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, labelAttText,
		XmNleftAttachment, XmATTACH_FORM,
		NULL);

  SetOptMenuToItem(fgColor, "black");
  
  fontButton = XtVaCreateManagedWidget("Fonts ...",
				       xmPushButtonWidgetClass, form,
				       XmNtopAttachment, XmATTACH_WIDGET,
				       XmNtopWidget, fgColorFrame,
				       XmNleftAttachment, XmATTACH_FORM,
				       NULL);

  XtAddCallback(fontButton, XmNactivateCallback, SetFontCallBack, XGD_LABEL);

  ok = XgInteractorGetChild(xgi, XmINTERACT_OK_BUTTON);

  XtUnmanageChild(ok);

  ok = XgInteractorGetChild(xgi, XmINTERACT_CANCEL_BUTTON);

  XtAddCallback(ok, XmNactivateCallback, LabelAttDismissCB, NULL);
  
  return(xgi);
}

void
#ifdef _NO_PROTO
ChangeLabelAttObject(obj)
     XgdObject *obj;
#else     
ChangeLabelAttObject(XgdObject *obj)
#endif
{
  int len = 0;
  int loop;
  char *str;
  XmFontList fontlist;
  Arg args[2];
  
  XtRemoveCallback(labelAttText, XmNvalueChangedCallback, LabelOkCB,
		   labelAttObject);

  labelAttObject = obj;

  if (obj->Obj.Label.numlines){
    for (loop = 0; loop < obj->Obj.Label.numlines; loop++)
      len += strlen(obj->Obj.Label.lblstr[loop]) + 1;
    
    str = (char *) malloc (len);
    
    sprintf(str, "%s\n", obj->Obj.Label.lblstr[0]);
    for (loop = 1; loop < obj->Obj.Label.numlines; loop++){
      sprintf(str, "%s%s\n", str, obj->Obj.Label.lblstr[loop]);
    }
  } else {
    str = (char *) malloc (1);
    str[0] = '\0';
  }
      
  XmTextSetString(labelAttText, str);
  XtAddCallback(labelAttText, XmNvalueChangedCallback, LabelOkCB,
		labelAttObject);
  
  SetOptMenuToPixel(fgColor, obj->fg);
  if (obj->Obj.Label.font){
    fontlist = XmFontListCreate(obj->Obj.Label.font, XmSTRING_DEFAULT_CHARSET);
    XtVaSetValues(labelAttText, XmNfontList, fontlist, NULL);
  } else {
    XFontStruct *font;
    XmFontContext context;
    XmStringCharSet *foo;
    
    XtSetArg(args[0], XmNfontList, &fontlist);
    XtGetValues(labelAttText, args, 1);
    XmFontListInitFontContext(&context, fontlist);
    XmFontListGetNextFont(context, &foo, &font);
    XgdSetLabelFont(obj, font, True);
    XmFontListFreeFontContext(context);
  }
  
}

void
#ifdef _NO_PROTO
LabelAttCB(widget, obj, call)
     Widget  widget;
     XgdObject *obj;
     caddr_t call;
#else
LabelAttCB(Widget widget, XgdObject *obj, caddr_t call)
#endif
{
  if (SelectedObjectCount() == 1){
     if (Global.selectedObjects->object->type == XGD_LABEL){
       Widget labelbox;

       labelbox = CreateLabelDialog(Global.selectedObjects->object);
       
       XtManageChild(labelbox);
     } else 
       XgError(Global.applShell, "Selected Object must be a Label");
   } else
     XgError(Global.applShell, "Select only 1 label object.");
  
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

    curPixel = cld;
    
    if ( SelectedObjectCount() == 1 &&
	Global.selectedObjects->object->type==XGD_LABEL)
    {
      XgdSetLabelColor(Global.selectedObjects->object, cld);
    } 
}

static void
#ifdef _NO_PROTO
LabelOkCB(widget, obj, call)
     Widget               widget;
     XgdObject           *obj;
     XmAnyCallbackStruct *call;
#else
LabelOkCB(Widget widget, XgdObject *obj, XmAnyCallbackStruct call)
#endif
{
  char *str;
  char **argv;
  int  argc;
  
  if (str = XmTextGetString(labelAttText)){
    ParseString(str, &argc, &argv);
    XgdDrawResizeHandles(obj, Global.xorGC);
    XgdUnDrawObject(obj, obj->bg, True);
    XgdSetLabelString(obj, argv, argc);
    XgdConfigureObject(obj->objgc, obj, obj->x, obj->y, obj->width,
		       obj->height, True);
    XgdDrawLabel(obj, NULL);
    XgdDrawResizeHandles(obj, Global.xorGC);
  }
}


void
#ifdef _NO_PROTO
LabelAttDismissCB(widget, cli, call)
     Widget    widget;
     Widget    cli;
     XtPointer call;
#else     
LabelAttDismissCB(Widget widget, Widget cli, XtPointer call)
#endif
{
  labelAttUp = False;
}

Boolean
#ifdef _NO_PROTO
IsLabelAttUp()
#else     
IsLabelAttUp(void)
#endif
{
  return(labelAttUp);
}

