#include <Xm/Separator.h>
#include <Xm/Form.h>
#include <Xm/PanedW.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Interact.h>
#include <Pixel.h>
#include <gis.h>
#include <X11/Intrinsic.h>
#include "xgdisp.h"

static Widget      label;
struct Categories  cats;
Widget             pixel;
XgdObject          *Gobj;
XColor             *SavedColors;
static void        PixelCancelCallback();
static void        PixelOkCallback();

Widget
#ifdef _NO_PROTO
XgCreateColormapEditor(obj, parent, name, args, numargs, cat)
     XgdObject         *obj;
     Widget             parent;
     char              *name;
     Arg               *args;
     int                numargs;
     struct Categories *cat;
#else
XgCreateColormapEditor(XgdObject *obj, Widget parent, char *name, Arg *args, int numargs, struct Categories * cat)
#endif
{
  Widget          ds;
  Widget          form;
  Widget          rc;
  Widget          *pbs;
  Widget          dummy;
  Widget          sep1, sep2;
  int             i;
  Arg             c_args[10];
  int             n = 0;
  XColor          xcolor;
  Colormap        cmap;
  char            nam[10];
  void           _xgPButtonCB();
  char            str[50];
  
  cats = *cat;
  Gobj = obj;

  SavedColors = (XColor *) calloc(sizeof(XColor), cat->num);
  for (i = 0; i < cat->num; i++){
    SavedColors[i].pixel = obj->Obj.GeoFrame.lookup_tbl[i];
  }
  XQueryColors(XtDisplay(parent), Global.cmap, SavedColors, cat->num);
    
  ds = XgCreateInteractorDialog(parent, name, args, numargs);
  form = XmCreateForm(ds, "Form", NULL, 0);
  cmap = Global.cmap;
  xcolor.pixel = obj->Obj.GeoFrame.lookup_tbl[0];
  XQueryColor(XtDisplay(parent), Global.cmap, &xcolor);
  XgAllocatePixel(XtDisplay(parent), cmap, &xcolor);

  n = 0;
  XtSetArg(c_args[n], XmNorientation, XmVERTICAL); ++n;
  XtSetArg(c_args[n], XmNspacing, 0); ++n;
  XtSetArg(c_args[n], XmNleftAttachment, XmATTACH_FORM); ++n;
  XtSetArg(c_args[n], XmNtopAttachment, XmATTACH_FORM); ++n;
/*  XtSetArg(c_args[n], XmNrightAttachment, XmATTACH_FORM); ++n;*/
  XtSetArg(c_args[n], XmNnumColumns, (int) (cats.num < 30) ? cats.num : 30 ); ++n;
  XtSetArg(c_args[n], XmNpacking, XmPACK_COLUMN); ++n;
  rc = XmCreateRowColumn(form, "PBrc", c_args, n);
  pbs = (Widget *) malloc(sizeof(Widget) * cat->num);
  n = 0;
  XtSetArg(c_args[n], XmNlabelString, XmStringCreateSimple(" ")); ++n;
  for (i = 0; i < cat->num; i++) {
    XtSetArg(c_args[1], XmNbackground, obj->Obj.GeoFrame.lookup_tbl[i]);
    sprintf(nam, "Foo %d", i);
    pbs[i] = XmCreatePushButton(rc, nam, c_args, 2);
    XtAddCallback(pbs[i], XmNactivateCallback, _xgPButtonCB, i);
    XtManageChild(pbs[i]);
  }
  XtManageChild(rc);
  n = 0;
  XtSetArg(c_args[n], XmNtopAttachment, XmATTACH_WIDGET); ++n;
  XtSetArg(c_args[n], XmNtopWidget, rc); ++n;
  XtSetArg(c_args[n], XmNleftAttachment, XmATTACH_FORM); ++n;
  XtSetArg(c_args[n], XmNrightAttachment, XmATTACH_FORM); ++n;
  sep1 = XmCreateSeparator(form, "Sep1", c_args, n);
  XtManageChild(sep1);
  
  n = 0;
  XtSetArg(c_args[n], XmNleftAttachment, XmATTACH_FORM); ++n;
  XtSetArg(c_args[n], XmNtopAttachment, XmATTACH_WIDGET); ++n;
  XtSetArg(c_args[n], XmNtopWidget, sep1); ++n;
  XtSetArg(c_args[n], XmNrightAttachment, XmATTACH_FORM); ++n;
  sprintf(str, "%s [%d] : %s", cats.title, cats.list[0].num,
	  cats.list[0].label);

  XtSetArg(c_args[n], XmNlabelString, XmStringCreateSimple(str)); ++n;
  
  label = XmCreateLabel(form, "label", c_args, n);
  XtManageChild(label);

  n = 0;
  XtSetArg(c_args[n], XmNtopAttachment, XmATTACH_WIDGET); ++n;
  XtSetArg(c_args[n], XmNtopWidget, label); ++n;
  XtSetArg(c_args[n], XmNleftAttachment, XmATTACH_FORM); ++n;
  XtSetArg(c_args[n], XmNrightAttachment, XmATTACH_FORM); ++n;
  sep2 = XmCreateSeparator(form, "Sep2", c_args, n);
  XtManageChild(sep2);

  n = 0;
  XtSetArg(c_args[n], XmNcolormap, cmap); ++n;
  XtSetArg(c_args[n], XmNxColor, &xcolor); ++n;
  XtSetArg(c_args[n], XmNleftAttachment, XmATTACH_FORM); ++n;
  XtSetArg(c_args[n], XmNtopAttachment, XmATTACH_WIDGET); ++n;
  XtSetArg(c_args[n], XmNtopWidget, sep2); ++n;
  XtSetArg(c_args[n], XmNbottomAttachment, XmATTACH_FORM); ++n;
  XtSetArg(c_args[n], XmNrightAttachment, XmATTACH_FORM); ++n;
  XtSetArg(c_args[n], XmNcancelRestore, True); ++n;
  pixel = XgCreatePixel(form, "pixel", c_args, n);

  XtManageChild(pixel);

  dummy = XgInteractorGetChild(pixel, XmINTERACT_OK_BUTTON);
  XtUnmanageChild(dummy);
  dummy = XgInteractorGetChild(pixel, XmINTERACT_CANCEL_BUTTON);
  XtUnmanageChild(dummy);
  dummy = XgInteractorGetChild(pixel, XmINTERACT_HELP_BUTTON);
  XtUnmanageChild(dummy);
  dummy = XgInteractorGetChild(pixel, XmINTERACT_SEPARATOR);
  XtUnmanageChild(dummy);
  XtManageChild(form);

  dummy = XgInteractorGetChild(ds, XmINTERACT_CANCEL_BUTTON);
  XtAddCallback(dummy, XmNactivateCallback, PixelCancelCallback, NULL);

  dummy = XgInteractorGetChild(ds, XmINTERACT_OK_BUTTON);
  XtAddCallback(dummy, XmNactivateCallback, PixelOkCallback, NULL);
  
  return(ds);
}

void
#ifdef _NO_PROTO
_xgPButtonCB(widget, cli, call)
     Widget widget;
     int cli;
     caddr_t call;
#else
_xgPButtonCB(Widget widget, int cli, caddr_t call)
#endif     
{
  char str[50];
  struct Cat_List *list;
  XColor   col;
  
  list = &cats.list[cli];
  
  sprintf(str, "%s [%d] : %s", cats.title, list->num,
	  list->label);

  XtVaSetValues(label, XmNlabelString, XmStringCreateSimple(str),
		NULL);

  col.pixel = Gobj->Obj.GeoFrame.lookup_tbl[cli];

  XQueryColor(XtDisplay(widget), Global.cmap, &col);

  XtVaSetValues(pixel, XmNxColor, &col, NULL);
}

static void
#ifdef _NO_PROTO
PixelCancelCallback(widget, cli, call)
     Widget  widget;
     caddr_t cli;
     caddr_t call;
#else     
PixelCancelCallback(Widget widget, caddr_t cli, caddr_t call)
#endif
{
  XStoreColors(XtDisplay(widget), Global.cmap, SavedColors, cats.num);
}

static void
#ifdef _NO_PROTO
PixelOkCallback(widget, cli, call)
     Widget  widget;
     caddr_t cli;
     caddr_t call;
#else     
PixelOkCallback(Widget widget, caddr_t cli, caddr_t call)
#endif
{
  XColor *colors;
  int i;
  
  colors = (XColor *) calloc(sizeof(XColor), cats.num);

  for (i = 0; i < cats.num; i++)
    colors[i].pixel = Gobj->Obj.GeoFrame.lookup_tbl[i];
  
  XQueryColors(XtDisplay(widget), Global.cmap, colors, cats.num);
  for (i = 0; i < cats.num; i++){
    G_set_color(cats.list[i].num, (int) (colors[i].red >> 8),
		(int) (colors[i].green >> 8), (int) (colors[i].blue >> 8),
		&Gobj->Obj.GeoFrame.colors);

  }

  G_write_colors(Gobj->Obj.GeoFrame.rname, Gobj->Obj.GeoFrame.rmapset,
		 &(Gobj->Obj.GeoFrame.colors));
}





