#ifndef __XGDPROTO_H
#define __XGDPROTO_H

/* exit.c */

void ConfirmExit(
#ifndef _NO_PROTO
     int *exit_code
#endif
);

void ExitOK(
#ifndef _NO_PROTO
     Widget    w,
     XtPointer cli,
     XtPointer call
#endif
);

/* client.c */
void _XgWMClientMessage(
#ifndef _NO_PROTO
    Widget w,
    XtPointer cld,
    XClientMessageEvent *msg
#endif
);

/* legend.c */
void SetLegendObject(
#ifndef _NO_PROTO
     XgdObject *obj
#endif
);

void LegendDisplayToggleCallBack(
#ifndef _NO_PROTO
     Widget    w,
     int       cli,
     XtPointer call
#endif
);

void ColumnSpacingDecrementCallBack(
#ifndef _NO_PROTO
    Widget w,
    Widget cld,
    XtPointer cad
#endif
);


void ColumnSpacingIncrementCallBack(
#ifndef _NO_PROTO
Widget w,
Widget cld,
XtPointer cad
#endif
);

void LegendToggleCallBack(
#ifndef _NO_PROTO
     Widget    w,
     XtPointer cli,
     XtPointer call
#endif
);

void LegendListCallBack(
#ifndef _NO_PROTO
     Widget    w,
     XtPointer cli,
     XmListCallbackStruct *call
#endif
);

Widget CreateLegendBox(
#ifndef _NO_PROTO
     XgdObject *obj
#endif
);

void LegendBoxCB(
#ifndef _NO_PROTO
     Widget    w,
     XtPointer cli,
     XtPointer call
#endif
);

/* queryraster.c */
Pixel queryraster(
#ifndef _NO_PROTO
     int x,
     int y,
     int ret
#endif
);		  

/* args.c */
void ParseString(
#ifndef _NO_PROTO
     char *r,
     int  *n,
     char ***st
#endif     
);

/* chlist.c */
void chlist(
#ifndef _NO_PROTO
Widget w, XtPointer cld 
#endif
);

/*highlight.c */
XtTimerCallbackProc HighlightFlash(
#ifndef _NO_PROTO
     caddr_t cli,
     XtIntervalId *id
#endif
);

/* labelbox.c */
Boolean IsLabelAttUp(
#ifndef _NO_PROTO
    void
#endif
);

void LabelAttDismissCB(
#ifndef _NO_PROTO
     Widget    widget,
     Widget    cli,
     XtPointer call
#endif
);

void ChangeLabelAttObject(
#ifndef _NO_PROTO
    XgdObject *obj
#endif
);

void LabelAttCB(
#ifndef _NO_PROTO
    Widget widget,
    XgdObject *obj,
    caddr_t call
#endif
);

Widget CreateLabelDialog(
#ifndef _NO_PROTO
XgdObject *obj
#endif
);

/* band.c */
void BandLine(
#ifndef _NO_PROTO
int x, int y, int mode
#endif
);

void BandRect(
#ifndef _NO_PROTO
int *x, int *y, int mode, Boolean equal
#endif
);

void CreateXorGc(
#ifndef _NO_PROTO
void
#endif
);

/* ctrecolor.c */
Widget XgCreateColormapEditor(
#ifndef _NO_PROTO
XgdObject *obj, Widget parent, char *name, 
Arg *args, int numargs, struct Categories * cat
#endif
);

/* coloropt.c */
Widget CreateColorOptionMenu(
#ifndef _NO_PROTO
Widget parent, Widget *omenu, char *string, void (*function)()
#endif
);

void SetOptMenuToItem(
#ifndef _NO_PROTO
Widget optMenu, char *item
#endif
);

void SetOptMenuToPixel(
#ifndef _NO_PROTO
Widget optMenu, Pixel item
#endif
);

Pixel SelectBestLabelColor(
#ifndef _NO_PROTO
Pixel pixel
#endif
);
void UpdateColorOptionMenu(
#ifndef _NO_PROTO
Widget w, Widget cld, XtPointer cad
#endif
);

/* font.c */
void SetFontCallBack(
#ifndef _NO_PROTO
Widget w, int cld, XtPointer cad
#endif
);


void SetFontOkCallBack (
#ifndef _NO_PROTO
Widget	w,
int cld,
XtPointer cad
#endif
);

/* grid.c */
void InitGridAttributes(
#ifndef _NO_PROTO
Display *dpy, int scrn, GC gc
#endif
);

Widget CreateGridGadget(
#ifndef _NO_PROTO
Widget parent, char *string, int type
#endif
);

void GridGapTextVerifyCallBack(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void GridGapTextCallBack(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void GridGapDecrementCallBack(
#ifndef _NO_PROTO
Widget w, Widget cld, XtPointer cad
#endif
);

/* gridbox.c */
void GridToggleCallBack( 
#ifndef _NO_PROTO
Widget w, XtPointer cld, XmToggleButtonCallbackStruct *cbs
#endif
);
void GridLabelToggleCallBack(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XmToggleButtonCallbackStruct *cbs
#endif
);
Widget CreateGridToolBox(
#ifndef _NO_PROTO
void
#endif
);
void GridForegroundColorCallBack(
#ifndef _NO_PROTO
Widget w, Pixel cld, XtPointer cad
#endif
);
void GridTextColorCallBack(
#ifndef _NO_PROTO
Widget w, Pixel cld, XtPointer cad
#endif
);
void GridLinePatternCallBack(
#ifndef _NO_PROTO
Widget w, int cld, XtPointer cad
#endif
);

/* gridlabel.c */
void DoGridLabel(
#ifndef _NO_PROTO
XgdObject *obj, int drawmode
#endif
);


/* barscale.c */
void InitBarscaleAttributes(
#ifndef _NO_PROTO
Display *dpy, int scrn, GC gc
#endif
);

/* todrawbarscale.c */
void todrawbarscale(
#ifndef _NO_PROTO
    XgdObject *obj,
    int x,
    int y
#endif
);

/* drawbarscale.c */
void DrawBarscale(
#ifndef _NO_PROTO
XgdObject *obj, int updtemode
#endif
);



/* barbox.c */
Widget CreateBarToolBox(
#ifndef _NO_PROTO
void
#endif
);
void BarForegroundColorCallBack(
#ifndef _NO_PROTO
Widget w, Pixel cld, XtPointer cad
#endif
);
void BarTextColorCallBack(
#ifndef _NO_PROTO
Widget w, Pixel cld, XtPointer cad
#endif
);


/* barattr.c */
Widget CreateBarStyle(
#ifndef _NO_PROTO
Widget parent, char *string
#endif
);
Widget CreateBarUnit(
#ifndef _NO_PROTO
Widget parent, char *string
#endif
);
void BarStyleToggleCallBack(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XmToggleButtonCallbackStruct *cbs
#endif
);
void BarUnitToggleCallBack(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XmToggleButtonCallbackStruct *cbs
#endif
);

void BarLengthTextVerifyCallBack(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);




/* bartextgadg.c */
Widget CreateBarGadget(
#ifndef _NO_PROTO
Widget parent, char *string, int type
#endif
);

void BarLengthTextCallBack(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void BarLengthDecrementCallBack(
#ifndef _NO_PROTO
Widget w, Widget cld, XtPointer cad
#endif
);
void BarLengthIncrementCallBack(
#ifndef _NO_PROTO
Widget w, Widget cld, XtPointer cad
#endif
);
void BarIntervalTextCallBack(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void BarIntervalDecrementCallBack(
#ifndef _NO_PROTO
Widget w, Widget cld, XtPointer cad
#endif
);
void BarIntervalIncrementCallBack(
#ifndef _NO_PROTO
Widget w, Widget cld, XtPointer cad
#endif
);
void BarThickTextCallBack(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void BarThickDecrementCallBack(
#ifndef _NO_PROTO
Widget w, Widget cld, XtPointer cad
#endif
);
void BarThickIncrementCallBack(
#ifndef _NO_PROTO
Widget w, Widget cld, XtPointer cad
#endif
);



/* handler.c */

void DrawPolyPointHandles(
#ifndef _NO_PROTO
    XgdObject *obj,
    GC gc
#endif
);

XgdPointList *InPolyPointMoveHandle(
#ifndef _NO_PROTO
    XgdObject *obj,
    int x,
    int y
#endif
);

void MovePolyPoint(
#ifndef _NO_PROTO
    XgdObject *obj,
    XgdPointList *point,
    int x,
    int y
#endif
);

void UpdatePositionReadout(
#ifndef _NO_PROTO
Widget w, int x, int y
#endif
);
void HandleExposeEvents(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XEvent *event, Boolean *dispatch
#endif
);
void HandleResizeEvents(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XEvent *event, Boolean *dispatch
#endif
);
void HandleMouseEvents(
#ifndef _NO_PROTO
Widget w, int cld, XEvent *event, Boolean *dispatch
#endif
);
void HandleDrawModeMotion(
#ifndef _NO_PROTO
int x, int y
#endif
);
void HandleDrawModeButtonPress(
#ifndef _NO_PROTO
int *x, int *y
#endif
);
void HandleDrawModeButtonRelease(
#ifndef _NO_PROTO
int x, int y
#endif
);
void HandleResizeButtonPress(
#ifndef _NO_PROTO
int x, int y, XgdBox *bbox, int which, Boolean equal
#endif
);
void HandleResizeButtonRelease(
#ifndef _NO_PROTO
int x, int y, XgdBox *bbox, int which, Boolean equal
#endif
);
void HandleResizeMotion(
#ifndef _NO_PROTO
int x, int y, XgdBox *bbox, int which, Boolean equal
#endif
);
void UpdateBBox(
#ifndef _NO_PROTO
int x, int y, XgdBox *bbox, int which, Boolean equal
#endif
);
void HandleMoveButtonPress(
#ifndef _NO_PROTO
XgdBox *bbox
#endif
);
void HandleMoveButtonRelease(
#ifndef _NO_PROTO
int lxoff, int lyoff, XgdBox *bbox
#endif
);
void HandleMoveMotion(
#ifndef _NO_PROTO
int xoff, int yoff, int lxoff, int lyoff, XgdBox *bbox
#endif
);

/* layout.c */
void CreateLayout(
#ifndef _NO_PROTO
Widget shell
#endif
);


/* linewgadg.c */
Widget CreateLineWidthGadget(
#ifndef _NO_PROTO
Widget parent, char *string
#endif
);
void LineWidthTextVerifyCallback(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void LineWidthTextCallback(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void LineWidthDecrementCallBack(
#ifndef _NO_PROTO
Widget w, Widget cld, XtPointer cad
#endif
);
void LineWidthIncrementCallBack(
#ifndef _NO_PROTO
Widget w, Widget cld, XtPointer cad
#endif
);


/* gridgapgadg.c */
Widget CreateGridGapGadget(
#ifndef _NO_PROTO
Widget parent, char *string
#endif
);
void GridGapTextVerifyCallback(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void GridGapTextCallback(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void GridGapIncrementCallBack(
#ifndef _NO_PROTO
Widget w, Widget cld, XtPointer cad
#endif
);

void GridSpacingTextCallback(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void GridSpacingDecrementCallBack(
#ifndef _NO_PROTO
Widget w, Widget cld, XtPointer cad
#endif
);
void GridSpacingIncrementCallBack(
#ifndef _NO_PROTO
Widget w, Widget cld, XtPointer cad
#endif
);

void GridLineWidthTextCallback(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void GridLineWidthDecrementCallBack(
#ifndef _NO_PROTO
Widget w, Widget cld, XtPointer cad
#endif
);
void GridLineWidthIncrementCallBack(
#ifndef _NO_PROTO
Widget w, Widget cld, XtPointer cad
#endif
);

/* menubar.c */
Widget CreateMenuBar(
#ifndef _NO_PROTO
Widget shell
#endif
);

/* menucb.c */
void SiteStandardCallBack(
#ifndef _NO_PROTO
     Widget    w,
     XtPointer cld,
     XtPointer call
#endif
);

void SiteFreeHandCallBack(
#ifndef _NO_PROTO
     Widget    w,
     XtPointer cld,
     XtPointer call
#endif
);

void OpenFileCallBack(
#ifndef _NO_PROTO
     Widget     w,
     XtPointer  cli,
     XtPointer  call
#endif
);

void OpenFileOKCallBack(
#ifndef _NO_PROTO
     Widget     w,
     XtPointer  cli,
     XtPointer  call
#endif
);

void SaveObjectsCallBack(
#ifndef _NO_PROTO
     Widget     w,
     XtPointer  cli,
     XtPointer  call
#endif
);

void SaveObjectsOKCallBack(
#ifndef _NO_PROTO
     Widget     w,
     XtPointer  cli,
     XtPointer  call
#endif
);
void SaveObjects(
#ifndef _NO_PROTO
    FILE *fp,
    ObjectList list
#endif
);

void DeleteMapOKCallBack(
#ifndef _NO_PROTO
     Widget     w,
     XtPointer  cli,
     XtPointer  call
#endif
);

void DeleteMapCallBack(
#ifndef _NO_PROTO
     Widget     w,
     XtPointer  cli,
     XtPointer  call
#endif
);

void TAct(
#ifndef _NO_PROTO
Widget w, unsigned int cld, XtPointer cad
#endif
);
void PBAct(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void SetHighlightColor(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void DoToolBox(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void DoGridBox(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void DoBarBox(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void MapDisplayCallBack(
#ifndef _NO_PROTO
Widget w, int cld, XtPointer cad
#endif
);

void BrowserOkCallBack(
#ifndef _NO_PROTO
Widget w, int cld, XtPointer cad
#endif
);

void RegionCallBack(
#ifndef _NO_PROTO
Widget w, int cld, XtPointer cad
#endif
);
void RegionOkCallBack(
#ifndef _NO_PROTO
Widget w, int cld, XtPointer cad
#endif
);
void BrowserCancelCallBack(
#ifndef _NO_PROTO
Widget w, int cld, XtPointer cad
#endif
);

/* mode.c */
char *GetStringFromMode(
#ifndef _NO_PROTO
unsigned int mode
#endif
);
void SetMode(
#ifndef _NO_PROTO
unsigned int mode
#endif
);
void SetDrawMode(
#ifndef _NO_PROTO
Widget w, int cld, XtPointer cad
#endif
);

/* objbutton.c */
Widget CreateObjectButtonPad(
#ifndef _NO_PROTO
Widget parent, char *string
#endif
);

/* object.c */

XgdBox *GetBBoxOfObjects(
#ifndef _NO_PROTO
     ObjectList list
#endif
);

Boolean IsObjectInList(
#ifndef _NO_PROTO
     ObjectList list,
     XgdObject *obj
#endif
);

void DeleteObjects(
#ifndef _NO_PROTO
     Widget widget,
     caddr_t call,
     caddr_t cli
#endif
);

void DrawObjectsInList(
#ifndef _NO_PROTO
ObjectList list
#endif
);
void DeleteObjectFromList(
#ifndef _NO_PROTO
     ObjectList *list,
     XgdObject  *obj
#endif
);

void PrintObjectList(
#ifndef _NO_PROTO
ObjectList *list, char *string
#endif
);

void AddObjectToList(
#ifndef _NO_PROTO
ObjectList *list, XgdObject *obj
#endif
);

void ObjectToFront(
#ifndef _NO_PROTO
ObjectList *list, XgdObject *obj
#endif
);

void ObjectToBack(
#ifndef _NO_PROTO
ObjectList *list, XgdObject *obj
#endif
);

void FreeObjectList(
#ifndef _NO_PROTO
ObjectList list
#endif
);

void GetPointSelectedObjects(
#ifndef _NO_PROTO
ObjectList list, int x, int y, ObjectList newList
#endif
);

void GetBoxSelectedObjects(
#ifndef _NO_PROTO
XgdBox *box, ObjectList newList
#endif
);

int SelectedObjectCount(
#ifndef _NO_PROTO
void
#endif
);

XgdBox * GetBBoxOfSelectedObjects(
#ifndef _NO_PROTO
void
#endif
);

void GetAffectedObjects(
#ifndef _NO_PROTO
XgdObject *obj, ObjectList addlist, ObjectList exclude
#endif
);

void GetBoxAffectedObjects(
#ifndef _NO_PROTO
XgdBox *obb, ObjectList addlist, ObjectList exclude
#endif
);

void ReorderObjects(
#ifndef _NO_PROTO
ObjectList list
#endif
);

int CountObjects(
#ifndef _NO_PROTO
ObjectList list
#endif
);

/* popup.c */
void CreatePopup(
#ifndef _NO_PROTO
void
#endif
);

/* qrastpl.c */
Widget CreateQueryRasterPanel(
#ifndef _NO_PROTO
void
#endif
);

/* qrasttext.c */
Widget CreateRWhereGadget(
#ifndef _NO_PROTO
Widget parent, char *string
#endif
);
Widget CreateRWhatGadget(
#ifndef _NO_PROTO
Widget parent, char *string
#endif
);


/* redrwarea.c */
void RedrawArea(
#ifndef _NO_PROTO
XgdObject *obj
#endif
);

/* ruler.c */
void CreateRulers(
#ifndef _NO_PROTO
void
#endif
);
void ClearRulerPixmaps(
#ifndef _NO_PROTO
void
#endif
);
void DrawRulerPixmaps(
#ifndef _NO_PROTO
void
#endif
);

/* scroll.c */
void HorizontalSliderMoved(
#ifndef _NO_PROTO
Widget w, int cld, XmScrollBarCallbackStruct * cad
#endif
);
void VerticalSliderMoved(
#ifndef _NO_PROTO
Widget w, int cld, XmScrollBarCallbackStruct * cad
#endif
);
void SetScrollBars(
#ifndef _NO_PROTO
int w, int h
#endif
);


/* setsite.c */
void SetSite(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);

/* sitegadg.c */
Widget CreateStdSiteGadget(
#ifndef _NO_PROTO
Widget parent, char *string, int type
#endif
);
void SiteSizeTextCallBack(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void SiteSizeDecrementCallBack(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void SiteSizeIncrementCallBack(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void SiteLWTextCallBack(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void SiteLWDecrementCallBack(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);
void SiteLWIncrementCallBack(
#ifndef _NO_PROTO
Widget w, XtPointer cld, XtPointer cad
#endif
);

/* siteicon.c */
Widget CreateSiteIcons(
#ifndef _NO_PROTO
Widget parent, char* string
#endif
);

/* stdsitespl.c */
Widget CreateStdSitesPanel(
#ifndef _NO_PROTO
void
#endif
);
void SiteForegroundColorCallBack(
#ifndef _NO_PROTO
Widget w, Pixel cld, XtPointer cad
#endif
);



/* toolbox.c */
Widget CreateToolBox(
#ifndef _NO_PROTO
void
#endif
);
void FillPatternCallBack(
#ifndef _NO_PROTO
Widget w, int cld, XtPointer cad
#endif
);
void LinePatternCallBack(
#ifndef _NO_PROTO
Widget w, int cld, XtPointer cad
#endif
);
void gLinePatternCallBack(
#ifndef _NO_PROTO
Widget w, int cld, XtPointer cad
#endif
);
void ForegroundColorCallBack(
#ifndef _NO_PROTO
Widget w, Pixel cld, XtPointer cad
#endif
);
void BackgroundColorCallBack(
#ifndef _NO_PROTO
Widget w, Pixel cld, XtPointer cad
#endif
);

/* units.c */
void SetUnits(
#ifndef _NO_PROTO
Widget w, int cld, XtPointer cad
#endif
);

/* menucb.c */
void DeleteRasterCallBack(
#ifndef _NO_PROTO
     Widget     w,
     XtPointer  cli,
     XtPointer  call
#endif
);

void WindowDumpCallBack(
#ifndef _NO_PROTO
     Widget w,
     XtPointer cld,
     XtPointer cad;
#endif
);

void WindowDumpOKCallBack(
#ifndef _NO_PROTO
     Widget w,
     XtPointer cld,
     XtPointer cad			
#endif
);

#endif /*  __XGDPROTO_H */
