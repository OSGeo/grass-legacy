#include "xgsupport.h"

static int done = 0;
static EditCellhdData *retval = NULL;

void
XgEditCellhdEventLoop(appContext, widget)
XtAppContext    appContext;
Widget widget;
{
    Display *display;

    done = 0;
    retval = NULL;
    while (done == 0 || XtPending()) {
        XEvent          event;
        XtAppNextEvent(appContext, &event);
        XtDispatchEvent(&event);
    }
    XtUnmanageChild(widget);
    XSync(XtDisplay(widget), False);
    while (XtPending()) {
        XEvent          event;
        XtAppNextEvent(appContext, &event);
        XtDispatchEvent(&event);
    }

}

void
EditCellhdOkCallBack(w, cld)
Widget w;
XtPointer cld;
{
    done = 1;
    retval = (EditCellhdData *)cld;
    return;
}

void
EditCellhdCancelCallBack(w)
Widget w;
{
  done = 1;
  retval = NULL;
  return;
}

Widget
DoEditCellhdDialog(shell, cellhd, map, mapset) 
Widget shell;
struct Cell_head *cellhd;
char *map, *mapset;
{
    Widget xgi;
    Widget child;
    Widget form;
    Widget label;
    Widget northCaption;
    Widget northText;
    Widget southCaption;
    Widget southText;
    Widget eastCaption;
    Widget eastText;
    Widget westCaption;
    Widget westText;
    Arg al[10];
    int ac = 0;
    char north[30];
    char south[30];
    char east[30];
    char west[30];
    char ew_res[30];
    char ns_res[30];
    char def_north[30];
    char def_south[30];
    char def_east[30];
    char def_west[30];
    char def_ew_res[30];
    char def_ns_res[30];
    char buffer[256];
    XmString xms;
    struct Cell_head def_wind;
    char *prj;
    char projection[80];
    char *err;
    EditCellhdData *data = (EditCellhdData *)XtMalloc(sizeof(EditCellhdData));

    if ( G_get_default_window(&def_wind) != 1) {
        XgWarningDialog(shell, "Can't get default window.");
	return NULL;
    }
    if ( cellhd->proj < 0) {
	cellhd->proj = def_wind.proj ;
	cellhd->zone = def_wind.zone ;
    } else if(cellhd->zone < 0)
	cellhd->zone = def_wind.zone ;
    prj = (char *)G__projection_name (cellhd->proj);

    if (!prj) prj = "** unknown **";

    if (cellhd->west >= cellhd->east || cellhd->south >= cellhd->north) {
	cellhd->north = def_wind.north;
	cellhd->south = def_wind.south;
	cellhd->east = def_wind.east;
	cellhd->west = def_wind.west;
    }
    if (cellhd->proj != def_wind.proj) {
	sprintf(buffer, 
"Projection %d differs from default projection %d\nDo you want them to match?",
            cellhd->proj, def_wind.proj);
	if (!XgYesNo(shell, buffer)) return NULL;
	cellhd->proj = def_wind.proj;
	cellhd->zone = def_wind.zone;
    }
    if (cellhd->zone != def_wind.zone) {
	sprintf(buffer, 
"Zone %d differs from default Zone %d\nDo you want them to match?",
            cellhd->zone, def_wind.zone);
	if (!XgYesNo(shell, buffer)) return NULL;
	cellhd->proj = def_wind.proj;
	cellhd->zone = def_wind.zone;
    }
    sprintf (projection, "Projection: %d (%s) Zone: %d", 
	cellhd->proj, prj, cellhd->zone) ;
    *north = 0;
    *south = 0;
    *east = 0;
    *west = 0;
    *ew_res = 0;
    *ns_res = 0;
    *def_north = 0;
    *def_south = 0;
    *def_east = 0;
    *def_west = 0;
    *def_ew_res = 0;
    *def_ns_res = 0;
    G_format_northing(def_wind.north, def_north, def_wind.proj);
    G_format_northing(def_wind.south, def_south, def_wind.proj);
    G_format_easting(def_wind.east, def_east, def_wind.proj);
    G_format_easting(def_wind.west, def_west, def_wind.proj);
    G_format_resolution(def_wind.ns_res, def_ns_res, def_wind.proj);
    G_format_resolution(def_wind.ew_res, def_ew_res, def_wind.proj);
    G_format_northing(cellhd->north, north, cellhd->proj);
    G_format_northing(cellhd->south, south, cellhd->proj);
    G_format_easting(cellhd->east, east, cellhd->proj);
    G_format_easting(cellhd->west, west, cellhd->proj);
    G_format_resolution(cellhd->ew_res, ew_res, cellhd->proj);
    G_format_resolution(cellhd->ns_res, ns_res, cellhd->proj);

    XtSetArg(al[ac], XmNautoUnmanage, False); ac++;
    XtSetArg(al[ac], XmNenableWorkAreaStretch, True); ac++;
    xgi = XgCreateInteractorDialog(shell,"Identify Raster Header Dialog",al,ac);
    child = XgInteractorGetChild(xgi, XmINTERACT_HELP_BUTTON);
    XtUnmanageChild(child);

    form = XtVaCreateManagedWidget("form", xmFormWidgetClass, xgi, NULL);

    sprintf(buffer,"North Edge [Default:%s]",def_north);
    xms = XmStringCreateSimple(buffer);
    northCaption = XtVaCreateManagedWidget("north_caption",
        xbaeCaptionWidgetClass, form,
        XmNleftAttachment, XmATTACH_FORM,
        XmNrightAttachment, XmATTACH_FORM,
        XmNtopAttachment, XmATTACH_FORM,
        XmNlabelPosition, XbaePositionLeft,
        XmNlabelString, xms,
        XmNlabelAlignment, XbaeAlignmentCenter,
        XmNtraversalOn, False,
        NULL);
    XmStringFree(xms);

    northText = XtVaCreateManagedWidget("row_text", xmTextFieldWidgetClass,
        northCaption,
        XmNcolumns, 15,
        XmNvalue, north,
        NULL);

    sprintf(buffer,"South Edge [Default:%s]",def_south);
    xms = XmStringCreateSimple(buffer);
    southCaption = XtVaCreateManagedWidget("south_caption",
        xbaeCaptionWidgetClass, form,
        XmNleftAttachment, XmATTACH_FORM,
        XmNrightAttachment, XmATTACH_FORM,
        XmNtopAttachment, XmATTACH_WIDGET,
	XmNtopWidget, northCaption,
        XmNlabelPosition, XbaePositionLeft,
        XmNlabelString, xms,
        XmNlabelAlignment, XbaeAlignmentCenter,
        XmNtraversalOn, False,
        NULL);
    XmStringFree(xms);

    southText = XtVaCreateManagedWidget("row_text", xmTextFieldWidgetClass,
        southCaption,
        XmNcolumns, 15,
        XmNvalue, south,
        NULL);

    sprintf(buffer,"East Edge [Default:%s]",def_east);
    xms = XmStringCreateSimple(buffer);
    eastCaption = XtVaCreateManagedWidget("east_caption",
        xbaeCaptionWidgetClass, form,
        XmNleftAttachment, XmATTACH_FORM,
        XmNrightAttachment, XmATTACH_FORM,
        XmNtopAttachment, XmATTACH_WIDGET,
	XmNtopWidget, southCaption,
        XmNlabelPosition, XbaePositionLeft,
        XmNlabelString, xms,
        XmNlabelAlignment, XbaeAlignmentCenter,
        XmNtraversalOn, False,
        NULL);
    XmStringFree(xms);

    eastText = XtVaCreateManagedWidget("row_text", xmTextFieldWidgetClass,
        eastCaption,
        XmNcolumns, 15,
        XmNvalue, east,
        NULL);

    sprintf(buffer,"West Edge [Default:%s]",def_west);
    xms = XmStringCreateSimple(buffer);
    westCaption = XtVaCreateManagedWidget("west_caption",
        xbaeCaptionWidgetClass, form,
        XmNleftAttachment, XmATTACH_FORM,
        XmNrightAttachment, XmATTACH_FORM,
        XmNtopAttachment, XmATTACH_WIDGET,
	XmNtopWidget, eastCaption,
        XmNlabelPosition, XbaePositionLeft,
        XmNlabelString, xms,
        XmNlabelAlignment, XbaeAlignmentCenter,
        XmNtraversalOn, False,
        NULL);
    XmStringFree(xms);

    westText = XtVaCreateManagedWidget("row_text", xmTextFieldWidgetClass,
        westCaption,
        XmNcolumns, 15,
        XmNvalue, west,
        NULL);
    
    xms = XmStringCreateSimple(projection);
    label = XtVaCreateManagedWidget("proj_label",
	xmLabelWidgetClass, form,
	XmNleftAttachment, XmATTACH_FORM,
	XmNrightAttachment, XmATTACH_FORM,
	XmNtopAttachment, XmATTACH_WIDGET,
	XmNtopWidget, westCaption,
	XmNbottomAttachment, XmATTACH_FORM,
	XmNlabelString, xms,
	NULL);
    XmStringFree(xms);

    data->cellhd = cellhd;
    data->north = northText;
    data->south = southText;
    data->east = eastText;
    data->west = westText;
    XtAddCallback(xgi, XmNokCallback, EditCellhdOkCallBack, data);
    XtAddCallback(xgi, XmNcancelCallback, EditCellhdCancelCallBack, NULL);

    XtManageChild(xgi);
    return xgi;
}

EditCellhdData *
XgEditCellhd(shell, cellhd, map, mapset)
Widget shell;
struct Cell_head *cellhd;
char *map, *mapset;
{
    XtAppContext    appContext;
    Widget widget;

    widget = DoEditCellhdDialog(shell, cellhd, map, mapset);
    if ( widget == NULL ) return 0;
    appContext = XtWidgetToApplicationContext(shell);
    XgEditCellhdEventLoop(appContext, widget);
    return retval;
}
