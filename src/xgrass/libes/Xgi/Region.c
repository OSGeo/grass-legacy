static char rcsid[] = "@(#)XGRASS $Id: Region.c,v 0.0 1992/05/05 14:56:07 sink Exp sink $";
/*
 * File: Region.c
 *
 * Desc: Implementation of Region Widget
 *
 * Auth: Eric W. Sink
 *
 * Date: 24 Feb 1992
 *
 * Modification History:
 *
 *
 */

#include <RegionP.h>
#include <Interact.h>
#include <color.h>

#include <X11/StringDefs.h>
#include <Xm/LabelG.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/DrawingA.h>
#include <Xm/Scale.h>
#include <Xm/ScaleP.h>
#include <Xm/Form.h>
#include <X11/Xlib.h>
#include <Xm/DialogS.h>

#include <stdio.h>
#include <string.h>
#include "xgrass_lib.h"
#include "Region.h"
#include "Pixel.h"
#include "Browser.h"

extern char    *G_adjust_Cell_head();

/*---------------------------------------------------*/
/* forward declarations                              */
/* */
/* this is a list of all private procedures in this  */
/* module                                            */
/*---------------------------------------------------*/

#ifdef _NO_PROTO

static void     PutFields();
static void     SetGraphicSize();
static void     RegionRedrawGraphic();
static void     rubberBand();
static void     ClassInitialize();
static void     ClassPartInitialize();
static void     Initialize();
static void     RegionResize();
static void     Destroy();
static void     DeleteChild();
static Boolean  SetValues();
static void     RegionCreatePixmap();
static void     RegionCreateWidgets();
static void     RegionAdjustAndAttach();
static void  ValidateFields();
static int      MapCoord2ScreenEasting();
static int      MapCoord2ScreenNorthing();
static double   Screen2MapCoordNorthing();
static double   Screen2MapCoordEasting();

#else                           /* _NO_PROTO */

static void
GridGetSavedRegion(
                   Widget w,
                   XtPointer data
);
static void RegionResize(Widget);
static void rubberBand(
           Widget widget,
           XButtonEvent * event,
           String * args,
           int *num_args
);
static void     RegionCreatePixmap(Widget);
static void     SetGraphicSize(Widget xgr);
static void     RegionRedrawGraphic(Widget, Boolean);
static void     ClassInitialize();
static void     ClassPartInitialize(RegionWidgetClass xgrc);
static void
Initialize(RegionWidget request,
           RegionWidget new);
static void     Destroy(RegionWidget xgr);
static void     DeleteChild(Widget w);
static          Boolean
SetValues(RegionWidget current,
          RegionWidget request,
          RegionWidget new);
static void     RegionCreateWidgets(RegionWidget xgr);
static void     RegionAdjustAndAttach(RegionWidget xgr, Boolean attach);
static void ToggleCurrent(Widget,XtPointer);
static void ToggleDefault(Widget,XtPointer);
static void ToggleGrid(Widget,XtPointer);
static void
PutFields(
          Widget xgr,
          struct Cell_head * region
);

static void ValidateFields( Widget xgr);
static int MapCoord2ScreenEasting( Widget xgr, double v);
static int MapCoord2ScreenNorthing( Widget xgr, double v);
static double Screen2MapCoordNorthing( Widget xgr, int v);
static double Screen2MapCoordEasting( Widget xgr, int v);

#endif                          /* _NO_PROTO */

#ifndef MCCABE
static char     defaultTranslations[] =
"<Key>osfSelect:         ManagerGadgetSelect()\n\
        <Key>osfHelp:           XgInteractorHelp()\n\
        <Key>osfCancel:         BulletinBoardCancel()\n\
        ~Shift ~Meta ~Alt <Key>space:   ManagerGadgetSelect()\n\
        <Key>:                  ManagerGadgetKeyInput()\n\
        <BtnMotion>:    ManagerGadgetButtonMotion()\n\
        <Btn1Down>:     ManagerGadgetArm()\n\
        <Btn1Down>,<Btn1Up>:    ManagerGadgetActivate()\n\
        <Btn1Up>:       ManagerGadgetActivate()\n\
        <Btn1Down>(2+): ManagerGadgetMultiArm()\n\
        <Btn1Up>(2+):   ManagerGadgetMultiActivate()";

static char     defaultAccelerators[] =
"\043override\n\
        <Key>osfCancel:         BulletinBoardCancel()";
#else
static char     defaultTranslations[];
static char     defaultAccelerators[];
#endif

static XtActionsRec actionsList[] =
{
    {"Enter", (XtActionProc) _XmManagerEnter},  /* Motif 1.0 */
    {"FocusIn", (XtActionProc) _XmManagerFocusIn},      /* Motif 1.0 */
    {"Arm", (XtActionProc) _XmGadgetArm},       /* Motif 1.0 */
    {"Activate", (XtActionProc) _XmGadgetActivate},     /* Motif 1.0 */
    {"XgInteractorHelp", (XtActionProc) _XgInteractorHelpAction},       /* Motif 1.0 */
    {"BulletinBoardCancel", (XtActionProc) _XmBulletinBoardCancel},
    {"draw", (XtActionProc) rubberBand},
};


/*---------------------------------------------------*/
/* widget resources                                  */
/*---------------------------------------------------*/
static XtResource resources[] =
{
    /* Region specific resources */

    {XmNgridColor,
        XmCGridColor,
        XmRString,
        sizeof(String),
        XtOffset(RegionWidget, region.color_name),
        XmRImmediate,
        (XtPointer) "black"
    },
    {XmNgrid,
        XmCGrid,
        XmRInt,
        sizeof(int),
        XtOffset(RegionWidget, region.grid_mode),
        XmRImmediate,
        (XtPointer) 0
    },
    {XmNcurrentRegion,
        XmCCurrentRegion,
        XmRCellHeadPtr,
        sizeof(struct Cell_head *),
        XtOffset(RegionWidget, region.input_region),
        XmRImmediate,
        (XtPointer) 0
    },
    {XmNeditDefaultRegion,
        XmCEditDefaultRegion,
        XmRBoolean,
        sizeof(Boolean),
        XtOffset(RegionWidget, region.edit_default_region),
        XmRImmediate,
        (XtPointer) FALSE
    },
    /* superclass resource default overrides */

    {XmNautoUnmanage,
        XmCAutoUnmanage,
        XmRBoolean,
        sizeof(Boolean),
        XtOffset(RegionWidget, bulletin_board.auto_unmanage),
        XmRImmediate,
        (XtPointer) FALSE
    },
    {XmNdialogType,
        XmCDialogType,
        XmRDialogType,
        sizeof(unsigned char),
        XtOffset(RegionWidget, interactor.dialog_type),
        XmRImmediate,
        (XtPointer) XmINTERACT_WORK_AREA_TYPE
    },

    {XmNaccelerators,
        XmCAccelerators, XmRAcceleratorTable, sizeof(XtAccelerators),
        XtOffset(XmBulletinBoardWidget, core.accelerators),
        XmRString, (XtPointer) defaultAccelerators
    },

};

static XmSyntheticResource syn_resources[] =
{
    {NULL, 0, 0, NULL, NULL}
};

externaldef(xgregionclassrec)
    RegionClassRec  regionClassRec =
    {
        {                       /* core class record        */
             /* superclass          */ (WidgetClass) & interactorClassRec,
             /* class_name          */ "Region",
             /* widget_size         */ sizeof(RegionRec),
             /* class_initialize    */ ClassInitialize,
             /* class part init     */ ClassPartInitialize,
             /* class_inited        */ FALSE,
             /* initialize          */ Initialize,
             /* initialize hook     */ NULL,
             /* realize             */ _XtInherit,
             /* actions             */ actionsList,
             /* num_actions         */ XtNumber(actionsList),
             /* resources           */ resources,
             /* num_resources       */ XtNumber(resources),
             /* xrm_class           */ NULLQUARK,
             /* compress_motion     */ TRUE,
             /* compress_exposure   */ XtExposeCompressMaximal,
             /* compress crossing   */ FALSE,
             /* visible_interest    */ FALSE,
             /* destroy             */ Destroy,
             /* resize              */ RegionResize,
             /* expose              */ _XtInherit,
             /* set_values          */ SetValues,
             /* set_values_hook     */ NULL,
             /* set_values_almost   */ _XtInherit,
             /* get_values_hook     */ NULL,
             /* accept_focus        */ NULL,
             /* version             */ XtVersion,
             /* callback_offsets    */ NULL,
             /* tm_table            */ defaultTranslations,
             /* query_geometry      */ (XtGeometryHandler) _XtInherit,
             /* display_accelerator */ NULL,
             /* extension           */ NULL,
        },
        {                       /* composite class record   */
             /* geometry manager */ (XtGeometryHandler) _XtInherit,
             /* set changed proc */ _XtInherit,
             /* insert_child     */ _XtInherit,
             /* delete_child     */ DeleteChild,
             /* extension        */ NULL,
        },
        {                       /* constraint class record  */
             /* no additional resources  */ NULL,
             /* num additional resources */ 0,
             /* size of constraint rec   */ 0,
             /* constraint_initialize    */ NULL,
             /* constraint_destroy       */ NULL,
             /* constraint_setvalue      */ NULL,
             /* extension                */ NULL,
        },
        {                       /* manager class record     */
             /* translations                 */ XtInheritTranslations,
             /* get_resources                */ syn_resources,
             /* num_syn_resources            */ XtNumber(syn_resources),
             /* constraint_syn_resources     */ NULL,
             /* num_constraint_syn_resources */ 0,
             /* parent_process               */ XmInheritParentProcess,
             /* extension                    */ NULL,
        },
        {                       /* bulletinBoard class record */
             /* always_install_accelerators */ TRUE,
             /* geo_matrix_create           */ (XmGeoCreateProc) _XtInherit,
             /* focus_moved_proc            */ NULL,
             /* extension                   */ NULL,
        },
        {                       /* interactor class record */
             /* extension */ NULL,
        },
        {                       /* region class record */
             /* extension */ NULL,
        }
    };

externaldef(xgregionwidgetclass) WidgetClass
regionWidgetClass = (WidgetClass) & regionClassRec;

static void
ClassInitialize()
{

}

static void
#ifdef _NO_PROTO
ClassPartInitialize(xgr)
    RegionWidgetClass xgr;
#else
ClassPartInitialize(
                    RegionWidgetClass xgr)
#endif
/****************
 * Class Initialization.  Sets up accelerators and fast subclassing.
 ****************/
{
    _XmFastSubclassInit(xgr, XmREGION_BIT);

    return;
}

/* PRIVATE */

static void
#ifdef _NO_PROTO
NewBox(xgr, isold, X1, Y1, X2, Y2, window)
    Widget          xgr;
    Boolean         isold;
    int             X1;
    int             Y1;
    int             X2;
    int             Y2;
    Boolean         window;
#else
NewBox(
       Widget xgr,
       Boolean isold,
       int X1,
       int Y1,
       int X2,
       int Y2,
       Boolean window
)
#endif
{
  /* This function simply erases any old box and draws a new one.  It is
     the primary routine used when rubberbanding.
  */
    if (isold) {
        if (window) {
            XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), XtWindow(R_GraphicDrawingArea(xgr)), R_RubberGC(xgr), R_OldX1(xgr), R_OldY1(xgr), R_OldX2(xgr), R_OldY1(xgr));
            XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), XtWindow(R_GraphicDrawingArea(xgr)), R_RubberGC(xgr), R_OldX1(xgr), R_OldY1(xgr), R_OldX1(xgr), R_OldY2(xgr));
            XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), XtWindow(R_GraphicDrawingArea(xgr)), R_RubberGC(xgr), R_OldX2(xgr), R_OldY1(xgr), R_OldX2(xgr), R_OldY2(xgr));
            XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), XtWindow(R_GraphicDrawingArea(xgr)), R_RubberGC(xgr), R_OldX2(xgr), R_OldY2(xgr), R_OldX1(xgr), R_OldY2(xgr));
        }
        XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), R_GraphicPixmap(xgr), R_RubberGC(xgr), R_OldX1(xgr), R_OldY1(xgr), R_OldX2(xgr), R_OldY1(xgr));
        XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), R_GraphicPixmap(xgr), R_RubberGC(xgr), R_OldX1(xgr), R_OldY1(xgr), R_OldX1(xgr), R_OldY2(xgr));
        XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), R_GraphicPixmap(xgr), R_RubberGC(xgr), R_OldX2(xgr), R_OldY1(xgr), R_OldX2(xgr), R_OldY2(xgr));
        XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), R_GraphicPixmap(xgr), R_RubberGC(xgr), R_OldX2(xgr), R_OldY2(xgr), R_OldX1(xgr), R_OldY2(xgr));
    }
    R_OldX1(xgr) = X1;
    R_OldX2(xgr) = X2;
    R_OldY1(xgr) = Y1;
    R_OldY2(xgr) = Y2;

    if (window) {
        XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), XtWindow(R_GraphicDrawingArea(xgr)), R_RubberGC(xgr), X1, Y1, X2, Y1);
        XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), XtWindow(R_GraphicDrawingArea(xgr)), R_RubberGC(xgr), X1, Y1, X1, Y2);
        XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), XtWindow(R_GraphicDrawingArea(xgr)), R_RubberGC(xgr), X2, Y1, X2, Y2);
        XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), XtWindow(R_GraphicDrawingArea(xgr)), R_RubberGC(xgr), X2, Y2, X1, Y2);
    }
    XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), R_GraphicPixmap(xgr), R_RubberGC(xgr), X1, Y1, X2, Y1);
    XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), R_GraphicPixmap(xgr), R_RubberGC(xgr), X1, Y1, X1, Y2);
    XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), R_GraphicPixmap(xgr), R_RubberGC(xgr), X2, Y1, X2, Y2);
    XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), R_GraphicPixmap(xgr), R_RubberGC(xgr), X2, Y2, X1, Y2);
}

static void
#ifdef _NO_PROTO
NewBoxRegion(xgr, isold, r, window)
    Widget          xgr;
    Boolean         isold;
    struct Cell_head *r;
    Boolean         window;
#else
NewBoxRegion(
             Widget xgr,
             Boolean isold,
             struct Cell_head * r,
             Boolean window
)
#endif
{
  /* This is another way of calling NewBox(), by passing it a region
     structure instead.
  */
    NewBox(xgr, isold,
           MapCoord2ScreenEasting(xgr, r->east),
           MapCoord2ScreenNorthing(xgr, r->north),
           MapCoord2ScreenEasting(xgr, r->west),
           MapCoord2ScreenNorthing(xgr, r->south), window);
}

static void
#ifdef _NO_PROTO
exposeGraphic(drawing_a, cld, cad)
    Widget          drawing_a;
    XtPointer       cld, cad;
#else
exposeGraphic(
              Widget drawing_a,
              XtPointer cld,
              XtPointer cad
)
#endif
{
    /* This is called when the graphic receives an expose event */

    XmDrawingAreaCallbackStruct *cbs = (XmDrawingAreaCallbackStruct *) cad;
    XEvent         *ev = cbs->event;

    Widget          xgr = (Widget) cld;

    XCopyArea(XtDisplay(drawing_a), R_GraphicPixmap(xgr), XtWindow(drawing_a), R_DefaultGC(xgr),
              ev->xexpose.x, ev->xexpose.y, ev->xexpose.width,
              ev->xexpose.height, ev->xexpose.x, ev->xexpose.y);
}

/* Clear the main drawing area by clearing the pixmap and calling XCopyArea() */
static void
#ifdef _NO_PROTO
RegionClearGraphic(xgr, window)
    Widget          xgr;
    Boolean         window;
#else
RegionClearGraphic(
                   Widget xgr,
                   Boolean window
)
#endif
{
    Dimension       w, h;

    /* Fill the pixmap with black */
    XSetForeground(XtDisplay(xgr), R_FillGC(xgr), BlackPixelOfScreen(XtScreen(xgr)));

    XFillRectangle(XtDisplay(R_GraphicDrawingArea(xgr)), R_GraphicPixmap(xgr), R_FillGC(xgr), 0, 0, R_TotalWidth(xgr), R_TotalHeight(xgr));

    /* clear default region with white */
    XSetForeground(XtDisplay(xgr), R_FillGC(xgr),
                   WhitePixelOfScreen(XtScreen(xgr)));
    /* this clears the pixmap */
    XFillRectangle(XtDisplay(xgr), R_GraphicPixmap(xgr), R_FillGC(xgr), R_GraphicX(xgr), R_GraphicY(xgr), R_GraphicWidth(xgr), R_GraphicHeight(xgr));
    /* drawing is now done using black; change the gc */
    XSetForeground(XtDisplay(xgr), R_FillGC(xgr),
                   BlackPixelOfScreen(XtScreen(xgr)));
    /* render the newly cleared pixmap onto the window */
    if (window) {
        XCopyArea(XtDisplay(xgr), R_GraphicPixmap(xgr), XtWindow(R_GraphicDrawingArea(xgr)), R_FillGC(xgr),
                  0, 0, R_TotalWidth(xgr), R_TotalHeight(xgr), 0, 0);
    }
}

#define max(a,b) (((a) > (b)) ? (a) : (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))

/*
 * This is the callback function which is called when the mouse is used
 * within the drawing area
 */

static void
#ifdef _NO_PROTO
rubberBand(widget, event, args, num_args)
    Widget          widget;
    XButtonEvent   *event;
    String         *args;
    int            *num_args;
#else
rubberBand(
           Widget widget,
           XButtonEvent * event,
           String * args,
           int *num_args
)
#endif
{
    Widget          xgr;
    xgr = widget;
    while (!XgIsRegion(xgr))
        xgr = XtParent(xgr);

    if (!strcmp(args[0], "down")) {
            NewBox(xgr, True, R_RubberX1(xgr) = R_RubberX2(xgr) = event->x, R_RubberY1(xgr) = R_RubberY2(xgr) = event->y, event->x, event->y, True);
    } else if (!strcmp(args[0], "up")) {
        NewBox(xgr, True, R_RubberX2(xgr) = event->x, R_RubberY2(xgr) = event->y, R_RubberX1(xgr), R_RubberY1(xgr), True);

        R_ResultRegion(xgr)->north = max(Screen2MapCoordNorthing(xgr, R_RubberY1(xgr)), Screen2MapCoordNorthing(xgr, R_RubberY2(xgr)));
        R_ResultRegion(xgr)->south = min(Screen2MapCoordNorthing(xgr, R_RubberY1(xgr)), Screen2MapCoordNorthing(xgr, R_RubberY2(xgr)));
        R_ResultRegion(xgr)->east = max(Screen2MapCoordEasting(xgr, R_RubberX1(xgr)), Screen2MapCoordEasting(xgr, R_RubberX2(xgr)));
        R_ResultRegion(xgr)->west = min(Screen2MapCoordEasting(xgr, R_RubberX1(xgr)), Screen2MapCoordEasting(xgr, R_RubberX2(xgr)));
        PutFields(xgr, R_ResultRegion(xgr));
        ValidateFields(xgr);
        if (R_EditDefaultRegion(xgr)) {
            RegionRedrawGraphic(xgr, True);
        }
    } else {                    /* motion */
        NewBox(xgr, True, R_RubberX2(xgr) = event->x, R_RubberY2(xgr) = event->y, R_RubberX1(xgr), R_RubberY1(xgr), True);
    }
}

/*
 * Generic routine to draw a line in the main drawing area.  Coordinates are
 * those of the map itself, not the drawing area.
 */

static void
#ifdef _NO_PROTO
drawLine(xgr, x1, y1, x2, y2, window)
    Widget          xgr;
    double          x1, y1, x2, y2;
    Boolean         window;
#else
drawLine(
         Widget xgr,
         double x1,
         double y1,
         double x2,
         double y2,
         Boolean window
)
#endif
{
    int             a, b, c, d;
    a = MapCoord2ScreenEasting(xgr, x1);
    b = MapCoord2ScreenNorthing(xgr, y1);
    c = MapCoord2ScreenEasting(xgr, x2);
    d = MapCoord2ScreenNorthing(xgr, y2);
    if (window) {
        XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), XtWindow(R_GraphicDrawingArea(xgr)), R_GridGC(xgr), a, b, a, d);
        XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), XtWindow(R_GraphicDrawingArea(xgr)), R_GridGC(xgr), a, b, c, b);
        XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), XtWindow(R_GraphicDrawingArea(xgr)), R_GridGC(xgr), c, b, c, d);
        XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), XtWindow(R_GraphicDrawingArea(xgr)), R_GridGC(xgr), c, d, a, d);
    }
    XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), R_GraphicPixmap(xgr), R_GridGC(xgr), a, b, a, d);
    XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), R_GraphicPixmap(xgr), R_GridGC(xgr), a, b, c, b);
    XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), R_GraphicPixmap(xgr), R_GridGC(xgr), c, b, c, d);
    XDrawLine(XtDisplay(R_GraphicDrawingArea(xgr)), R_GraphicPixmap(xgr), R_GridGC(xgr), c, d, a, d);
}

/* Routine which draws a grid on the main drawing area */

static void
#ifdef _NO_PROTO
DrawGrid(xgr, window)
    Widget          xgr;
    Boolean         window;
#else
DrawGrid(
         Widget xgr,
         Boolean window
)
#endif
{
    double          x, y;
    double          startWest;
    double          startSouth;
    unsigned long   cnt;

    startWest = R_GridRegion(xgr).west;
    if (startWest < R_DefaultRegion(xgr).west) {
        cnt = (R_DefaultRegion(xgr).west - startWest) / R_GridRegion(xgr).ew_res;
        if (!cnt)
            cnt++;
        startWest += cnt * R_GridRegion(xgr).ew_res;
    } else if (startWest > R_DefaultRegion(xgr).west + R_GridRegion(xgr).ew_res) {
        cnt = (startWest - R_DefaultRegion(xgr).west) / R_GridRegion(xgr).ew_res;
        if (!cnt)
            cnt++;
        startWest -= cnt * R_GridRegion(xgr).ew_res;
    }
    startSouth = R_GridRegion(xgr).south;
    if (startSouth < R_DefaultRegion(xgr).south) {
        cnt = (R_DefaultRegion(xgr).south - startSouth) / R_GridRegion(xgr).ns_res;
        if (!cnt)
            cnt++;
        startSouth += cnt * R_GridRegion(xgr).ns_res;
    } else if (startSouth > R_DefaultRegion(xgr).south + R_GridRegion(xgr).ns_res) {
        cnt = (startSouth - R_DefaultRegion(xgr).south) / R_GridRegion(xgr).ns_res;
        if (!cnt)
            cnt++;
        startSouth -= cnt * R_GridRegion(xgr).ns_res;
    }
    x = startWest;
    while (x <= R_DefaultRegion(xgr).east) {
        drawLine(xgr, x, R_DefaultRegion(xgr).north, x, R_DefaultRegion(xgr).south, window);
        x += R_GridInfoGap(xgr);
    }

    y = startSouth;
    while (y <= R_DefaultRegion(xgr).north) {
        drawLine(xgr, R_DefaultRegion(xgr).west, y, R_DefaultRegion(xgr).east, y, window);
        y += R_GridInfoGap(xgr);
    }
}

static double
#ifdef _NO_PROTO
Screen2MapCoordEasting(xgr, v)
    Widget          xgr;
    int             v;
#else
Screen2MapCoordEasting(
                       Widget xgr,
                       int v
)
#endif
{
    double          horizUnitsPerPixel;
    double          myv = ((double) v) - R_GraphicX(xgr);
    horizUnitsPerPixel = R_GraphicWidth(xgr) / (R_DefaultRegion(xgr).east - R_DefaultRegion(xgr).west);
    return myv / horizUnitsPerPixel + R_DefaultRegion(xgr).west;
}

static double
#ifdef _NO_PROTO
Screen2MapCoordNorthing(xgr, v)
    Widget          xgr;
    int             v;
#else
Screen2MapCoordNorthing(
                        Widget xgr,
                        int v
)
#endif
{
    double          vertUnitsPerPixel;
    double          myv = ((double) v) - R_GraphicY(xgr);

    vertUnitsPerPixel = R_GraphicHeight(xgr) / (R_DefaultRegion(xgr).north - R_DefaultRegion(xgr).south);
    return (R_GraphicHeight(xgr) - myv) / vertUnitsPerPixel + R_DefaultRegion(xgr).south;
}

static int
#ifdef _NO_PROTO
MapCoord2ScreenNorthing(xgr, v)
    Widget          xgr;
    double          v;
#else
MapCoord2ScreenNorthing(
                        Widget xgr,
                        double v
)
#endif
{
    double          vertUnitsPerPixel;

    vertUnitsPerPixel = R_GraphicHeight(xgr) / (R_DefaultRegion(xgr).north - R_DefaultRegion(xgr).south);
    return (R_GraphicHeight(xgr) - ((v - R_DefaultRegion(xgr).south) * vertUnitsPerPixel)) + R_GraphicY(xgr);
}

static int
#ifdef _NO_PROTO
MapCoord2ScreenEasting(xgr, v)
    Widget          xgr;
    double          v;
#else
MapCoord2ScreenEasting(
                       Widget xgr,
                       double v
)
#endif
{
    double          horizUnitsPerPixel;
    horizUnitsPerPixel = R_GraphicWidth(xgr) / (R_DefaultRegion(xgr).east - R_DefaultRegion(xgr).west);
    return R_GraphicX(xgr) + (v - R_DefaultRegion(xgr).west) * horizUnitsPerPixel;
}

static void
#ifdef _NO_PROTO
PutFields(xgr, region)
    Widget          xgr;
    struct Cell_head *region;
#else
PutFields(
          Widget xgr,
          struct Cell_head * region
)
#endif
{
    char            buf[100];
    G_format_northing(region->north, buf, region->proj);
    XmTextSetString(R_NorthText(xgr), buf);
    G_format_northing(region->south, buf, region->proj);
    XmTextSetString(R_SouthText(xgr), buf);
    G_format_easting(region->west, buf, region->proj);
    XmTextSetString(R_WestText(xgr), buf);
    G_format_easting(region->east, buf, region->proj);
    XmTextSetString(R_EastText(xgr), buf);
    G_format_resolution(region->ns_res, buf, region->proj);
    XmTextSetString(R_NSResText(xgr), buf);
    G_format_resolution(region->ew_res, buf, region->proj);
    XmTextSetString(R_EWResText(xgr), buf);
    if (R_GridInfoActive(xgr)) {
        G_format_resolution(R_GridRegion(xgr).ns_res, buf, R_GridRegion(xgr).proj);
        XmTextSetString(R_GridInfoNSResText(xgr), buf);
        G_format_resolution(R_GridRegion(xgr).ew_res, buf, R_GridRegion(xgr).proj);
        XmTextSetString(R_GridInfoEWResText(xgr), buf);
        G_format_resolution(R_GridInfoGap(xgr), buf, R_GridRegion(xgr).proj);
        XmTextSetString(R_GridInfoGapText(xgr), buf);
    }
}

static          void
#ifdef _NO_PROTO
ValidateFields(xgr)
    Widget          xgr;
#else
ValidateFields(
               Widget xgr
)
#endif
{
    char           *tmp;
    char           *garb;
    double          northResult, southResult, westResult, eastResult, nsresResult, ewresResult;
    tmp = XmTextGetString(R_NorthText(xgr));
    if (!G_scan_northing(tmp, &northResult, R_DefaultRegion(xgr).proj)) {
        XgWarningDialog(xgr, "North edge field invalid");
        return;
    }
    tmp = XmTextGetString(R_SouthText(xgr));
    if (!G_scan_northing(tmp, &southResult, R_DefaultRegion(xgr).proj)) {
        XgWarningDialog(xgr, "South edge field invalid");
        return;
    }
    tmp = XmTextGetString(R_WestText(xgr));
    if (!G_scan_easting(tmp, &westResult, R_DefaultRegion(xgr).proj)) {
        XgWarningDialog(xgr, "West edge field invalid");
        return;
    }
    tmp = XmTextGetString(R_EastText(xgr));
    if (!G_scan_easting(tmp, &eastResult, R_DefaultRegion(xgr).proj)) {
        XgWarningDialog(xgr, "East edge field invalid");
        return;
    }
    tmp = XmTextGetString(R_NSResText(xgr));
    if (!G_scan_resolution(tmp, &nsresResult, R_DefaultRegion(xgr).proj)) {
        XgWarningDialog(xgr, "North-South resolution invalid");
        return;
    }
    tmp = XmTextGetString(R_EWResText(xgr));
    if (!G_scan_resolution(tmp, &ewresResult, R_DefaultRegion(xgr).proj)) {
        XgWarningDialog(xgr, "East-west resolution invalid");
        return;
    }
    R_ResultRegion(xgr)->north = northResult;
    R_ResultRegion(xgr)->south = southResult;
    R_ResultRegion(xgr)->west = westResult;
    R_ResultRegion(xgr)->east = eastResult;
    R_ResultRegion(xgr)->ns_res = nsresResult;
    R_ResultRegion(xgr)->ew_res = ewresResult;
    if (tmp = G_adjust_Cell_head(R_ResultRegion(xgr), 0, 0)) {
        XgWarningDialog(xgr, tmp);
        return;
    }
    PutFields(xgr, R_ResultRegion(xgr));
    if (R_EditDefaultRegion(xgr)) {
        R_DefaultRegion(xgr) = *R_ResultRegion(xgr);
    }
    return;
}

/* snapToGrid() aligns the region with a grid imposed by some coordinate
   system.  This coord system may be chosen from several sources.
*/

static void
#ifdef _NO_PROTO
snapToGrid(w, data)
    Widget          w;
    XtPointer       data;
#else
snapToGrid(
           Widget w,
           XtPointer data
)
#endif
{
    char           *tmp;
    double          x;
    Widget          xgr = (Widget) data;
    extern char    *G_align_window();

    if (tmp = G_align_window(R_ResultRegion(xgr), &R_GridRegion(xgr))) {
        XgWarningDialog(XtParent(w), tmp);
    }
    PutFields(xgr, (R_ResultRegion(xgr)));
    NewBoxRegion(xgr, True, (R_ResultRegion(xgr)), True);
}

static void
#ifdef _NO_PROTO
acceptSetGridColor(w, cl, ca)             /* CALLBACK */
    Widget          w;
    XtPointer       cl, ca;
#else
acceptSetGridColor(
                   Widget w,
                   XtPointer cl,
                   XtPointer ca
)
#endif
{
    Widget          xgr = (Widget) cl;
    XColor         *xcolor;
    Widget          xgp;

    xgp = w;
    while (!XgIsPixel(xgp))
        xgp = XtParent(xgp);


    XtVaGetValues(xgp, XmNxColor, &xcolor, NULL);

    R_GridColor(xgr).red = xcolor->red;
    R_GridColor(xgr).green = xcolor->green;
    R_GridColor(xgr).blue = xcolor->blue;
    R_GridColor(xgr).pixel = xcolor->pixel;
    R_GridColor(xgr).flags = xcolor->flags;
}

static void
#ifdef _NO_PROTO
acceptCancelGridColor(w, cl, ca)             /* CALLBACK */
    Widget          w;
    XtPointer       cl, ca;
#else
acceptCancelGridColor(
                      Widget w,
                      XtPointer cl,
                      XtPointer ca
)
#endif
{
    Widget          xgr = (Widget) cl;
    XColor         *xcolor;
    Widget          xgp;

    xgp = w;
    while (!XgIsPixel(xgp))
        xgp = XtParent(xgp);

    XtVaGetValues(xgp, XmNxColorOriginal, &xcolor, NULL);

    R_GridColor(xgr).red = xcolor->red;
    R_GridColor(xgr).green = xcolor->green;
    R_GridColor(xgr).blue = xcolor->blue;
    R_GridColor(xgr).pixel = xcolor->pixel;
    R_GridColor(xgr).flags = xcolor->flags;
}

/* SetGridColor() brings up a Pixel widget which allows the user to set
   the color of the grid.
*/

static void
#ifdef _NO_PROTO
SetGridColor(w, data)
    Widget          w;
    XtPointer       data;
#else
SetGridColor(
             Widget w,
             XtPointer data
)
#endif
{
    Widget          xgp, child;
    Arg             al[16];
    int             ac = 0;
    int             screen;
    Widget          xgr = (Widget) data;

    screen = DefaultScreen(XtDisplay(w));
    XtSetArg(al[ac], XmNcolormap, DefaultColormapOfScreen(XtScreen(R_GraphicDrawingArea(xgr))));
    ac++;
    XtSetArg(al[ac], XmNxColor, &R_GridColor(xgr));
    ac++;
    XtSetArg(al[ac], XmNscaleTypeMask, XgRGB);
    ac++;
    XtSetArg(al[ac], XmNenableWorkAreaStretch, True);
    ac++;
    XtSetArg(al[ac], XmNokLabelString, XmStringCreateSimple("Done"));
    ac++;
    XtSetArg(al[ac], XmNapplyLabelString, XmStringCreateSimple("Accept"));
    ac++;
    XtSetArg(al[ac], XmNautoUnmanage, True);
    ac++;
    XtSetArg(al[ac], XmNdialogStyle, XmDIALOG_APPLICATION_MODAL);
    ac++;
    XtSetArg(al[ac], XmNnavigationType, XmEXCLUSIVE_TAB_GROUP);
    ac++;
    xgp = XgCreatePixelDialog(w, "Set Grid Color", al, ac);
    XtManageChild(xgp);

    XtAddCallback(xgp, XmNokCallback, acceptSetGridColor, (XtPointer) xgr);
    XtAddCallback(xgp, XmNcancelCallback, acceptCancelGridColor, (XtPointer) xgr);

}

static void
#ifdef _NO_PROTO
browserAcceptRaster(w, cl, ca)             /* CALLBACK */
    Widget          w;
    XtPointer       cl, ca;
#else
browserAcceptRaster(
                    Widget w,
                    XtPointer cl,
                    XtPointer ca
)
#endif
{
    Widget          xgb = (Widget) cl;
    char           *reg;
    char           *tmp;
    XmString       result;
    struct Cell_head region;
    int             i;
    char            name[512];
    char            mapset[512];
    char           *s;
    extern char *G_get_cellhd();
    Widget          xgr = w;
    while (!XgIsRegion(xgr))
        xgr = XtParent(xgr);


    XtVaGetValues(xgb, XmNresultString, &result, NULL);
    XmStringGetLtoR(result, XmSTRING_DEFAULT_CHARSET, &reg);

    if (reg) {
        s = strtok(reg, "@");
        strcpy(name, s);
        s = strtok(NULL, "@");
        strcpy(mapset, s);
        if (tmp = G_get_cellhd(name, mapset, &region)) {
            XgWarningDialog(xgr, tmp);
        }
        if (tmp = G_adjust_Cell_head(&region, 1, 1)) {
            XgWarningDialog(xgr, tmp);
        }
        R_GridRegion(xgr) = region;
        R_GridMode(xgr) = XG_GRID_RASTER;
        PutFields(xgr, R_ResultRegion(xgr));
        RegionRedrawGraphic(xgr, True);
        _XgFree(reg);
    }
    XtDestroyWidget(xgb);
}

static void
#ifdef _NO_PROTO
browserAcceptRegion(w, cl, ca)             /* CALLBACK */
    Widget          w;
    XtPointer       cl, ca;
#else
browserAcceptRegion(
                    Widget w,
                    XtPointer cl,
                    XtPointer ca
)
#endif
{
    Widget          xgb = (Widget) cl;
    char           *reg;
    XmString       result;
    struct Cell_head region;
    int             i;
    char            name[512];
    char            mapset[512];
    char           *s;
    Widget          xgr = w;
    while (!XgIsRegion(xgr))
        xgr = XtParent(xgr);


    XtVaGetValues(xgb, XmNresultString, &result, NULL);
    XmStringGetLtoR(result, XmSTRING_DEFAULT_CHARSET, &reg);

    if (reg) {
        s = strtok(reg, "@");
        strcpy(name, s);
        s = strtok(NULL, "@");
        strcpy(mapset, s);
        G__get_window(&region, "windows", name, mapset);
        *R_ResultRegion(xgr) = region;
	XmTextFieldSetString(R_SaveName(xgr),name);
        PutFields(xgr, R_ResultRegion(xgr));
        ValidateFields(xgr);
        NewBoxRegion(xgr, True, R_ResultRegion(xgr), True);
        _XgFree(reg);
    }
    XtDestroyWidget(xgb);
}

static void
#ifdef _NO_PROTO
GridOptionsCancel(w, cl, ca)             /* CALLBACK */
    Widget          w;
    XtPointer       cl, ca;
#else
GridOptionsCancel(
                  Widget w,
                  XtPointer cl,
                  XtPointer ca
)
#endif
{
    Widget          xgi = (Widget) cl;
    Widget          xgr = w;
    while (!XgIsRegion(xgr))
        xgr = XtParent(xgr);
    R_GridInfoActive(xgr) = False;

    XtDestroyWidget(xgi);
}

static void
#ifdef _NO_PROTO
GridOptionsOK(w, cl, ca)             /* CALLBACK */
    Widget          w;
    XtPointer       cl, ca;
#else
GridOptionsOK(
              Widget w,
              XtPointer cl,
              XtPointer ca
)
#endif
{
    Widget          xgi = (Widget) cl;
    Widget          xgr = w;
    while (!XgIsRegion(xgr))
        xgr = XtParent(xgr);
    R_GridInfoActive(xgr) = False;

    XtDestroyWidget(xgi);
}

static void
#ifdef _NO_PROTO
browserCancelRaster(w, cl, ca)             /* CALLBACK */
    Widget          w;
    XtPointer       cl, ca;
#else
browserCancelRaster(
                    Widget w,
                    XtPointer cl,
                    XtPointer ca
)
#endif
{
    Widget          xgb = (Widget) cl;
    char           *reg;
    Widget          xgr = w;
    while (!XgIsRegion(xgr))
        xgr = XtParent(xgr);


    XtDestroyWidget(xgb);
}

static void
#ifdef _NO_PROTO
browserCancelRegion(w, cl, ca)             /* CALLBACK */
    Widget          w;
    XtPointer       cl, ca;
#else
browserCancelRegion(
                    Widget w,
                    XtPointer cl,
                    XtPointer ca
)
#endif
{
    Widget          xgb = (Widget) cl;
    char           *reg;

    XtDestroyWidget(xgb);
}

/* GetDefaultRegion() sets the current region to the default region */

static void
#ifdef _NO_PROTO
GetDefaultRegion(w, data)
    Widget          w;
    XtPointer       data;
#else
GetDefaultRegion(
                 Widget w,
                 XtPointer data
)
#endif
{
    Widget          xgr = (Widget) data;

    *R_ResultRegion(xgr) = R_DefaultRegion(xgr);
    PutFields(xgr, R_ResultRegion(xgr));
    ValidateFields(xgr);
    NewBoxRegion(xgr, True, R_ResultRegion(xgr), True);
}

/* SaveRegion() actually saves the current region into the database, using
   the contents of the SaveName text field as the file name
*/

static void
#ifdef _NO_PROTO
SaveRegion(w, data)
    Widget          w;
    XtPointer       data;
#else
SaveRegion(
           Widget w,
           XtPointer data
)
#endif
{
  Widget          xgr = (Widget) data;

  G__put_window (R_ResultRegion(xgr), "windows", XmTextFieldGetString(R_SaveName(xgr)));
}

/* Bring up a Browser allowing the user to set the current region from
   some other region file.
*/

static void
#ifdef _NO_PROTO
GetSavedRegion(w, data)
    Widget          w;
    XtPointer       data;
#else
GetSavedRegion(
               Widget w,
               XtPointer data
)
#endif
{
    char           *reg;
    struct Cell_head region;
    int             i;
    char            name[512];
    char            mapset[512];
    char           *s;
    Widget          xgr = (Widget) data;
    Widget          xgb;
    Arg             al[16];
    int             ac = 0;
    Widget          child;

    XtSetArg(al[ac], XmNinitialMapset1, XmStringCreateSimple(G_mapset()));ac++;
    XtSetArg(al[ac], XmNinitialMapset2, XmStringCreateSimple("PERMANENT"));ac++;
    XtSetArg(al[ac], XmNnumLists, 1); ac++;
    XtSetArg(al[ac], XmNbrowseMode, XG_REGION); ac++;
    XtSetArg(al[ac], XmNselMode, XG_SINGLE_SELECT);
    ac++;
    XtSetArg(al[ac], XmNpromptString, XmStringCreateSimple("Please select a region file"));
    ac++;
    xgb = XgCreateBrowserDialog(xgr, "Region selector", al, ac);
    XtManageChild(xgb);
    XtAddCallback(xgb, XmNokCallback, browserAcceptRegion, (XtPointer) xgb);
    XtAddCallback(xgb, XmNcancelCallback, browserCancelRegion, (XtPointer) xgb);

}

static void
#ifdef _NO_PROTO
GridInfoChanged(w, data)
    Widget          w;
    XtPointer       data;
#else
GridInfoChanged(
                Widget w,
                XtPointer data
)
#endif
{
    Widget    xgr = (Widget) data;
    char           *tmp;
    char           *garb;
    double          nsresResult;
    double          ewresResult;
    long            gap;

    tmp = XmTextGetString(R_GridInfoNSResText(xgr));
    if (!G_scan_resolution(tmp, &nsresResult, R_DefaultRegion(xgr).proj)) {
        XgWarningDialog(xgr, "North-South resolution invalid");
    }
    tmp = XmTextGetString(R_GridInfoEWResText(xgr));
    if (!G_scan_resolution(tmp, &ewresResult, R_DefaultRegion(xgr).proj)) {
        XgWarningDialog(xgr, "East-West resolution invalid");
    }
    tmp = XmTextGetString(R_GridInfoGapText(xgr));
    if (!G_scan_resolution(tmp, &R_GridInfoGap(xgr), R_DefaultRegion(xgr).proj)) {
        XgWarningDialog(xgr, "Grid Spacing Invalid");
    }

    R_GridRegion(xgr).ns_res = nsresResult;
    R_GridRegion(xgr).ew_res = ewresResult;
    if (tmp = G_adjust_Cell_head(&R_GridRegion(xgr), 0, 0)) {
        XgWarningDialog((Widget) xgr, tmp);
    }
    PutFields(xgr, R_ResultRegion(xgr));

    RegionRedrawGraphic(xgr, True);
    XmProcessTraversal(xgr, XmTRAVERSE_NEXT_TAB_GROUP);
}

static void
#ifdef _NO_PROTO
FieldChanged(w, data)
    Widget          w;
    XtPointer       data;
#else
FieldChanged(
             Widget w,
             XtPointer data
)
#endif
{
    Widget    xgr = (Widget) data;

    ValidateFields(xgr);
    if (R_EditDefaultRegion(xgr)) {
        RegionRedrawGraphic(xgr, True);
    } else {
        NewBoxRegion(xgr, True, R_ResultRegion(xgr), True);
    }
    XmProcessTraversal(xgr, XmTRAVERSE_NEXT_TAB_GROUP);
}

static void
#ifdef _NO_PROTO
RegionRedrawGraphic(xgr, window)
    Widget          xgr;
    Boolean         window;
#else
RegionRedrawGraphic(
                    Widget xgr,
                    Boolean window
)
#endif
{
    if (R_EditDefaultRegion(xgr)) {
        SetGraphicSize(xgr);
        XFreePixmap(XtDisplay(xgr), R_GraphicPixmap(xgr));
        RegionCreatePixmap(xgr);
    }
    RegionClearGraphic(xgr, window);
    if (R_GridMode(xgr)) {
        DrawGrid(xgr, window);
    }
        NewBoxRegion(xgr, False, R_ResultRegion(xgr), window);
}

static void
#ifdef _NO_PROTO
ToggleGrid(w, data)
    Widget          w;
    XtPointer       data;
#else
ToggleGrid(
           Widget w,
           XtPointer data
)
#endif
{                               /* Help button */
    Widget          xgr = (Widget) data;

    R_GridMode(xgr) = !R_GridMode(xgr);
    RegionRedrawGraphic(xgr, True);
}

/****************************************************************/
static void
#ifdef _NO_PROTO
Initialize(request, new)
    RegionWidget    request;
    RegionWidget    new;
#else
Initialize(
           RegionWidget request,
           RegionWidget new)
#endif
/****************
 * This routine initializes an instance of the region widget.
 ****************/
{
    Arg             args[16];
    int             numArgs;
    XColor         *xcolor;
    Widget          child;
    /****************/

    if (R_EditDefaultRegion(new)) {
        R_DefaultRegion(new) = *R_InputRegion(new);
    } else {
        G_get_default_window(&(R_DefaultRegion(new)));
    }
    R_GridRegion(new) = R_DefaultRegion(new);
    R_GridInfoGap(new) = 10*R_GridRegion(new).ns_res;
    R_GridInfoActive(new) = False;

    /*
     * Create all of the new widgets.
     */
    if (request->manager.navigation_type != XmEXCLUSIVE_TAB_GROUP)
        _XmChangeNavigationType((Widget) new, ((XtIsShell(XtParent(request))
                                       ? XmSTICKY_TAB_GROUP : XmTAB_GROUP)));

    RegionCreateWidgets(new);
}

static void
CalcGraphicSize(xgr)
{
    Dimension       frameHeight;
    Dimension       frameWidth;
    Dimension       h1, h2, w1, w2;

    frameHeight = 0.9 * R_TotalHeight(xgr);
    frameWidth = 0.9 * R_TotalWidth(xgr);

    /* First, try setting height */
    h1 = frameHeight;
    w1 = h1 * ((R_DefaultRegion(xgr).east - R_DefaultRegion(xgr).west) / (R_DefaultRegion(xgr).north - R_DefaultRegion(xgr).south));
    w2 = frameWidth;
    h2 = w2 * ((R_DefaultRegion(xgr).north - R_DefaultRegion(xgr).south) / (R_DefaultRegion(xgr).east - R_DefaultRegion(xgr).west));
    /*
     * Now, we have two possible sets of dimensions.  We need to choose which
     * one we want.
     */

    if (w1 > frameWidth) {
        R_GraphicHeight(xgr) = h2;
        R_GraphicWidth(xgr) = w2;
        return;
    }
    if (h2 > frameHeight) {
        R_GraphicHeight(xgr) = h1;
        R_GraphicWidth(xgr) = w1;
        return;
    }
    if (w1 > w2) {
        R_GraphicHeight(xgr) = h1;
        R_GraphicWidth(xgr) = w1;
    } else {
        R_GraphicHeight(xgr) = h2;
        R_GraphicWidth(xgr) = w2;
    }
}

static void
#ifdef _NO_PROTO
SetGraphicSize(xgr)
    Widget          xgr;
#else
SetGraphicSize(
               Widget xgr
)
#endif
{
    Dimension       frameHeight;
    Dimension       frameWidth;
    Dimension       h1, h2, w1, w2;

    XtVaGetValues(R_GraphicDrawingArea(xgr), XmNheight, &frameHeight, XmNwidth, &frameWidth, NULL);

    R_TotalHeight(xgr) = frameHeight;
    R_TotalWidth(xgr) = frameWidth;

    CalcGraphicSize(xgr);
}

static void
#ifdef _NO_PROTO
RegionCreatePixmap(xgr)
    Widget    xgr;
#else
RegionCreatePixmap(Widget xgr)
#endif
{
    R_GraphicPixmap(xgr) = XCreatePixmap(XtDisplay(R_GraphicDrawingArea(xgr)),
                    RootWindowOfScreen(XtScreen(R_GraphicDrawingArea(xgr))),
                                      R_TotalWidth(xgr), R_TotalHeight(xgr),
                 DefaultDepthOfScreen(XtScreen(R_GraphicDrawingArea(xgr))));

    R_GraphicX(xgr) = (R_TotalWidth(xgr) - R_GraphicWidth(xgr)) / 2;
    R_GraphicY(xgr) = (R_TotalHeight(xgr) - R_GraphicHeight(xgr)) / 2;

    /* Fill the pixmap with black */
    XSetForeground(XtDisplay(xgr), R_FillGC(xgr), BlackPixelOfScreen(XtScreen(xgr)));

    XFillRectangle(XtDisplay(R_GraphicDrawingArea(xgr)), R_GraphicPixmap(xgr), R_FillGC(xgr), 0, 0,
                   R_TotalWidth(xgr), R_TotalHeight(xgr));

    /* Fill the pixmap with white */
    XSetForeground(XtDisplay(xgr), R_FillGC(xgr), WhitePixelOfScreen(XtScreen(xgr)));

    XFillRectangle(XtDisplay(R_GraphicDrawingArea(xgr)), R_GraphicPixmap(xgr), R_FillGC(xgr), R_GraphicX(xgr), R_GraphicY(xgr),
                   R_GraphicWidth(xgr), R_GraphicHeight(xgr));
}

static void
#ifdef _NO_PROTO
RegionResize(w)
    Widget          w;
#else
RegionResize(
             Widget w
)
#endif
{
    Widget          xgr = w;
    Boolean         hasWindow;

    while (!XgIsRegion(xgr))
        xgr = XtParent(xgr);

    hasWindow = (Boolean) xgr->core.window;
    (*((InteractorWidgetClass) interactorWidgetClass)->core_class.resize) (xgr);

    SetGraphicSize(xgr);

    XFreePixmap(XtDisplay(xgr), R_GraphicPixmap(xgr));
    RegionCreatePixmap(xgr);
    RegionClearGraphic(xgr, hasWindow);
    RegionRedrawGraphic(xgr, hasWindow);

    if (hasWindow)
        XClearArea(XtDisplay(xgr), XtWindow(xgr), 0, 0, 0, 0, True);
}


static void
#ifdef _NO_PROTO
ToggleCoord(w, data)
    Widget          w;
    XtPointer       data;
#else
ToggleCoord(
            Widget w,
            XtPointer data
)
#endif
{
    Widget          xgr = (Widget) data;
    Boolean         isSet = XmToggleButtonGetState(w);
    if (isSet) {
        XmToggleButtonSetState(R_GridInfoToggleDefault(xgr), False, False);
        XmToggleButtonSetState(R_GridInfoToggleRaster(xgr), False, False);
        XmToggleButtonSetState(R_GridInfoToggleCurrent(xgr), False, False);
    }
    if (isSet) {
        R_GridMode(xgr) = XG_GRID_COORD;
        R_GridRegion(xgr).south = 0;
        R_GridRegion(xgr).west = 0;
    } else
        R_GridMode(xgr) = XG_GRID_NONE;
    PutFields(xgr, R_ResultRegion(xgr));
    RegionRedrawGraphic(xgr, True);
}

static void
#ifdef _NO_PROTO
GridGetSavedRegion(w, data)
    Widget          w;
    XtPointer       data;
#else
GridGetSavedRegion(
                   Widget w,
                   XtPointer data
)
#endif
{
    char           *reg;
    struct Cell_head region;
    int             i;
    char            name[64];
    char            mapset[64];
    char           *s;
    Widget          xgr = (Widget) data;
    Widget          xgb;
    Arg             al[16];
    int             ac = 0;
    Widget          child;

    XtSetArg(al[ac], XmNinitialMapset1, XmStringCreateSimple(G_mapset()));ac++;
    XtSetArg(al[ac], XmNinitialMapset2, XmStringCreateSimple("PERMANENT"));ac++;
    XtSetArg(al[ac], XmNbrowseMode, XG_RASTER); ac++;
    XtSetArg(al[ac], XmNselMode, XG_SINGLE_SELECT); ac++;
    XtSetArg(al[ac], XmNpromptString, XmStringCreateSimple("Please select a raster file")); ac++;
    xgb = XgCreateBrowserDialog(xgr, "Raster selector", al, ac);
    XtManageChild(xgb);
    XtAddCallback(xgb, XmNokCallback, browserAcceptRaster, (XtPointer) xgb);
    XtAddCallback(xgb, XmNcancelCallback, browserCancelRaster, (XtPointer) xgb);

}

static void
#ifdef _NO_PROTO
ToggleRaster(w, data)
    Widget          w;
    XtPointer       data;
#else
ToggleRaster(
             Widget w,
             XtPointer data
)
#endif
{
    Widget          xgr = (Widget) data;
    Boolean         isSet = XmToggleButtonGetState(w);
    if (isSet) {
        XmToggleButtonSetState(R_GridInfoToggleCoord(xgr), False, False);
        XmToggleButtonSetState(R_GridInfoToggleDefault(xgr), False, False);
        XmToggleButtonSetState(R_GridInfoToggleCurrent(xgr), False, False);
    }
    if (isSet) {
        GridGetSavedRegion(w, (XtPointer) xgr);
    } else
        R_GridMode(xgr) = XG_GRID_NONE;
}

static void
#ifdef _NO_PROTO
ToggleCurrent(w, data)
    Widget          w;
    XtPointer       data;
#else
ToggleCurrent(
              Widget w,
              XtPointer data
)
#endif
{
    Widget          xgr = (Widget) data;
    Boolean         isSet = XmToggleButtonGetState(w);
    if (isSet) {
        XmToggleButtonSetState(R_GridInfoToggleCoord(xgr), False, False);
        XmToggleButtonSetState(R_GridInfoToggleRaster(xgr), False, False);
        XmToggleButtonSetState(R_GridInfoToggleDefault(xgr), False, False);
    }
    if (isSet) {
        R_GridMode(xgr) = XG_GRID_CURRENT;
        R_GridRegion(xgr) = *R_ResultRegion(xgr);
    } else
        R_GridMode(xgr) = XG_GRID_NONE;
    PutFields(xgr, R_ResultRegion(xgr));
    RegionRedrawGraphic(xgr, True);
}

static void
#ifdef _NO_PROTO
ToggleDefault(w, data)
    Widget          w;
    XtPointer       data;
#else
ToggleDefault(
              Widget w,
              XtPointer data
)
#endif
{
    Widget          xgr = (Widget) data;
    Boolean         isSet = XmToggleButtonGetState(w);
    if (isSet) {
        XmToggleButtonSetState(R_GridInfoToggleCoord(xgr), False, False);
        XmToggleButtonSetState(R_GridInfoToggleRaster(xgr), False, False);
        XmToggleButtonSetState(R_GridInfoToggleCurrent(xgr), False, False);
    }
    if (isSet) {
        R_GridMode(xgr) = XG_GRID_DEFAULT;
        R_GridRegion(xgr) = R_DefaultRegion(xgr);
    } else
        R_GridMode(xgr) = XG_GRID_NONE;
    PutFields(xgr, R_ResultRegion(xgr));
    RegionRedrawGraphic(xgr, True);
}

static void
#ifdef _NO_PROTO
DoGridOptions(w, data)
    Widget          w;
    XtPointer       data;
#else
DoGridOptions(
              Widget w,
              XtPointer data
)
#endif
{
    Widget          xgr = (Widget) data;
    Widget          dialog;

    dialog = XgCreateInteractorDialog(xgr, "Region Grid Options", NULL, 0);

    R_GridInfoFrame(xgr) = XtVaCreateManagedWidget("region_gridinfo_frame", xmFrameWidgetClass, dialog,
                                                   NULL);
    R_GridInfoContainer(xgr) = XtVaCreateManagedWidget("region_gridinfo_container", xmRowColumnWidgetClass, R_GridInfoFrame(xgr),
                                                 XmNorientation, XmVERTICAL,
                                                   XmNpacking, XmPACK_TIGHT,
                                                       NULL);

    R_GridInfoTopPart(xgr) = XtVaCreateManagedWidget("region_gridinfo_top_part", xmRowColumnWidgetClass, R_GridInfoContainer(xgr),
                                               XmNorientation, XmHORIZONTAL,
                                                   XmNpacking, XmPACK_TIGHT,
                                                     NULL);

    R_GridInfoToggles(xgr) = XtVaCreateManagedWidget("region_gridinfo_toggles", xmRowColumnWidgetClass, R_GridInfoTopPart(xgr),
                                                 XmNorientation, XmVERTICAL,
                                                   XmNpacking, XmPACK_TIGHT,
                                                     NULL);

    R_GridInfoOriginLabel(xgr) = XtVaCreateManagedWidget("region_gridinfo_origin_label", xmLabelWidgetClass, R_GridInfoToggles(xgr),
                       XmNlabelString, XmStringCreateSimple("Grid Origin:"),
                                                      XmNtraversalOn, False,
                                                         NULL);

    R_GridInfoToggleCoord(xgr) = XtVaCreateManagedWidget("region_gridinfo_toggle_coord", xmToggleButtonWidgetClass, R_GridInfoToggles(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                                            XmNindicatorType, XmONE_OF_MANY,
                      XmNset, ((Boolean) (R_GridMode(xgr) == XG_GRID_COORD)),
                  XmNlabelString, XmStringCreateSimple("Coordinate system"),
                                                         NULL);

    R_GridInfoToggleRaster(xgr) = XtVaCreateManagedWidget("region_gridinfo_toggle_raster", xmToggleButtonWidgetClass, R_GridInfoToggles(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                                            XmNindicatorType, XmONE_OF_MANY,
                     XmNset, ((Boolean) (R_GridMode(xgr) == XG_GRID_RASTER)),
                     XmNlabelString, XmStringCreateSimple("Raster file..."),
                                                          NULL);

    R_GridInfoToggleCurrent(xgr) = XtVaCreateManagedWidget("region_gridinfo_toggle_current", xmToggleButtonWidgetClass, R_GridInfoToggles(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                                            XmNindicatorType, XmONE_OF_MANY,
                    XmNset, ((Boolean) (R_GridMode(xgr) == XG_GRID_CURRENT)),
                     XmNlabelString, XmStringCreateSimple("Current Region"),
                                                           NULL);

    R_GridInfoToggleDefault(xgr) = XtVaCreateManagedWidget("region_gridinfo_toggle_default", xmToggleButtonWidgetClass, R_GridInfoToggles(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                                            XmNindicatorType, XmONE_OF_MANY,
                    XmNset, ((Boolean) (R_GridMode(xgr) == XG_GRID_DEFAULT)),
                     XmNlabelString, XmStringCreateSimple("Default Region"),
                                                           NULL);

    XtAddCallback(R_GridInfoToggleCoord(xgr), XmNvalueChangedCallback, ToggleCoord, xgr);
    XtAddCallback(R_GridInfoToggleRaster(xgr), XmNvalueChangedCallback, ToggleRaster, xgr);
    XtAddCallback(R_GridInfoToggleCurrent(xgr), XmNvalueChangedCallback, ToggleCurrent, xgr);
    XtAddCallback(R_GridInfoToggleDefault(xgr), XmNvalueChangedCallback, ToggleDefault, xgr);

    R_GridInfoVertSep(xgr) = XtVaCreateManagedWidget("region_gridinfo_vert_sep", xmSeparatorWidgetClass, R_GridInfoTopPart(xgr),
                                                 XmNorientation, XmVERTICAL,
                                                     NULL);

    R_GridInfoFields(xgr) = XtVaCreateManagedWidget("region_gridinfo_fields", xmRowColumnWidgetClass, R_GridInfoTopPart(xgr),
                                                    XmNisAligned, True,
                                      XmNentryAlignment, XmALIGNMENT_CENTER,
                                                 XmNorientation, XmVERTICAL,
                                                    XmNpacking, XmPACK_TIGHT,
                                                    NULL);

    R_GridInfoFieldsLabel(xgr) = XtVaCreateManagedWidget("region_gridinfo_fields_label", xmLabelWidgetClass, R_GridInfoFields(xgr),
                       XmNlabelString, XmStringCreateSimple("Resolutions:"),
                                                      XmNtraversalOn, False,
                                                         NULL);

    R_GridInfoFieldsTable(xgr) = XtVaCreateManagedWidget("region_gridinfo_fields_table", xmRowColumnWidgetClass, R_GridInfoFields(xgr),
                                                         XmNisAligned, False,
                                                 XmNorientation, XmVERTICAL,
                                                  XmNpacking, XmPACK_COLUMN,
                                                         XmNnumColumns, 2,
                                                         NULL);

    R_GridInfoNSResLabel(xgr) = XtVaCreateManagedWidget("region_gridinfo_nsres_label", xmLabelWidgetClass, R_GridInfoFieldsTable(xgr),
                                                      XmNtraversalOn, False,
                                                     XmNlabelType, XmSTRING,
                             XmNlabelString, XmStringCreateSimple("NS res"),
                                        XmNalignment, XmALIGNMENT_BEGINNING,
                                                        XmNmarginRight, 20,
                                                        NULL);

    R_GridInfoEWResLabel(xgr) = XtVaCreateManagedWidget("region_gridinfo_ewres_label", xmLabelWidgetClass, R_GridInfoFieldsTable(xgr),
                                                      XmNtraversalOn, False,
                                                     XmNlabelType, XmSTRING,
                             XmNlabelString, XmStringCreateSimple("EW res"),
                                        XmNalignment, XmALIGNMENT_BEGINNING,
                                                        XmNmarginRight, 20,
                                                        NULL);

    R_GridInfoGapLabel(xgr) = XtVaCreateManagedWidget("region_gridinfo_gap_label", xmLabelWidgetClass, R_GridInfoFieldsTable(xgr),
                                                      XmNtraversalOn, False,
                                                      XmNlabelType, XmSTRING,
                       XmNlabelString, XmStringCreateSimple("Display interval"),
                                        XmNalignment, XmALIGNMENT_BEGINNING,
                                                      XmNmarginRight, 20,
                                                      NULL);

    R_GridInfoNSResText(xgr) = XtVaCreateManagedWidget("region_gridinfo_nsres_text", xmTextWidgetClass, R_GridInfoFieldsTable(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                                                       XmNwordWrap, False,
                                                       XmNmaxLength, 10,
                                                       XmNcolumns, 10,
                                                       NULL);
    XtAddCallback(R_GridInfoNSResText(xgr), XmNactivateCallback, GridInfoChanged, xgr);

    R_GridInfoEWResText(xgr) = XtVaCreateManagedWidget("region_gridinfo_ewres_text", xmTextWidgetClass, R_GridInfoFieldsTable(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                                                       XmNwordWrap, False,
                                                       XmNmaxLength, 10,
                                                       XmNcolumns, 10,
                                                       NULL);
    XtAddCallback(R_GridInfoEWResText(xgr), XmNactivateCallback, GridInfoChanged, xgr);

    R_GridInfoGapText(xgr) = XtVaCreateManagedWidget("region_gridinfo_gap_text", xmTextWidgetClass, R_GridInfoFieldsTable(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                                                     XmNwordWrap, False,
                                                     XmNmaxLength, 10,
                                                     XmNcolumns, 10,
                                                     NULL);
    XtAddCallback(R_GridInfoGapText(xgr), XmNactivateCallback, GridInfoChanged, xgr);

    R_GridInfoHorizSep(xgr) = XtVaCreateManagedWidget("region_gridinfo_horiz_sep", xmSeparatorWidgetClass, R_GridInfoContainer(xgr),
                                                      NULL);

    R_GridInfoButtons(xgr) = XtVaCreateManagedWidget("region_gridinfo_buttons", xmRowColumnWidgetClass, R_GridInfoContainer(xgr),
                                                     XmNisAligned, True,
                                      XmNentryAlignment, XmALIGNMENT_CENTER,
                                               XmNorientation, XmHORIZONTAL,
                                                  XmNpacking, XmPACK_COLUMN,
                                                     NULL);
    R_SnapButton(xgr) = XtVaCreateManagedWidget("reg_snp_button", xmPushButtonWidgetClass, R_GridInfoButtons(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                               XmNlabelString, XmStringCreateSimple("Align"),
                                                NULL);
    XtAddCallback(R_SnapButton(xgr), XmNactivateCallback, snapToGrid, xgr);
    XgAddHelpCallBackFromFile(R_SnapButton(xgr),"reg_snp_button");

    R_GridColorButton(xgr) = XtVaCreateManagedWidget("region_col_button", xmPushButtonWidgetClass, R_GridInfoButtons(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                      XmNlabelString, XmStringCreateSimple("Grid color..."),
                                                     NULL);
    XtAddCallback(R_GridColorButton(xgr), XmNactivateCallback, SetGridColor, xgr);
    XgAddHelpCallBackFromFile(R_GridColorButton(xgr), "reg_col_button");
    XtAddCallback(dialog, XmNokCallback, GridOptionsOK, dialog);
    XtAddCallback(dialog, XmNcancelCallback, GridOptionsCancel, dialog);
    XtManageChild(dialog);
    R_GridInfoActive(xgr) = True;

    PutFields(xgr, R_ResultRegion(xgr));
}

/****************************************************************/
static void
#ifdef _NO_PROTO
RegionCreateWidgets(xgr)
    RegionWidget    xgr;
#else
RegionCreateWidgets(
                    RegionWidget xgr
)
#endif
{
    Display        *dpy;
    Widget          theform;
    Arg             args[5];
    int             nargs = 0;
    char            buf2[256];
    char            buf[256];

    XGCValues       gcv;
    XGCValues       gcv2;
    XGCValues       gcv3;
    XGCValues       gcvGrid;
    Widget          child;

    String          translations =      /* for the DrawingArea widget */
    "<Btn1Down>:   draw(down)\n\
         <Btn1Up>:     draw(up)  \n\
         <Btn1Motion>: draw(motion)";
    XtActionsRec    actions;

    gcv.foreground = WhitePixelOfScreen(XtScreen(xgr));
    R_FillGC(xgr) = XCreateGC(XtDisplay(xgr), RootWindowOfScreen(XtScreen(xgr)), GCForeground, &gcv);

    gcvGrid.foreground = WhitePixelOfScreen(XtScreen(xgr));
    R_GridGC(xgr) = XCreateGC(XtDisplay(xgr), RootWindowOfScreen(XtScreen(xgr)), GCForeground, &gcvGrid);

    gcv2.function = GXxor;
    gcv2.plane_mask = BlackPixel(XtDisplay(xgr), 0) ^ WhitePixel(XtDisplay(xgr), 0);
    gcv2.foreground = 0xffffffff;
    R_RubberGC(xgr) = XCreateGC(XtDisplay(xgr), RootWindowOfScreen(XtScreen(xgr)), GCFunction | GCForeground | GCPlaneMask, &gcv2);

    dpy = XtDisplay(xgr);
    R_DefaultGC(xgr) = DefaultGC(dpy, DefaultScreen(dpy));

    theform = XmCreateForm((Widget) xgr, "xgregion_form", args, nargs);
    XgAddHelpCallBackFromFile(theform, "xgregion_form");

    XtManageChild(theform);

    R_ButtonsSeparator(xgr) = XtVaCreateManagedWidget("region_buttons_separator", xmSeparatorWidgetClass, theform,
                                                      NULL);

    R_ButtonsContainer(xgr) = XtVaCreateManagedWidget("region_buttons_container", xmRowColumnWidgetClass, theform,
                                                      XmNisAligned, True,
                                      XmNentryAlignment, XmALIGNMENT_CENTER,
                                               XmNorientation, XmHORIZONTAL,
                                                  XmNpacking, XmPACK_COLUMN,
                                                      NULL);

    R_SaveFrame(xgr) = XtVaCreateManagedWidget("region_save_frame",xmFrameWidgetClass, theform,
	NULL);

    R_SaveContainer(xgr) = XtVaCreateManagedWidget("region_save_container", xmRowColumnWidgetClass, R_SaveFrame(xgr),
                                                      XmNisAligned, True,
                                      XmNentryAlignment, XmALIGNMENT_CENTER,
                                               XmNorientation, XmHORIZONTAL,
                                                         XmNnumColumns, 2,
                                                  XmNpacking, XmPACK_TIGHT,
                                                      NULL);

    R_DataFrame(xgr) = XtVaCreateManagedWidget("region_data_frame", xmFrameWidgetClass, theform,
                                               NULL);

    R_DataTable(xgr) = XtVaCreateManagedWidget("region_table", xmRowColumnWidgetClass, R_DataFrame(xgr),
                                               XmNisAligned, False,
                                               XmNorientation, XmVERTICAL,
                                               XmNpacking, XmPACK_COLUMN,
                                               XmNnumColumns, 2,
                                               NULL);
    XgAddHelpCallBackFromFile(R_DataTable(xgr), "region_table");

    R_GridButton(xgr) = XtVaCreateManagedWidget("region_gr_button", xmPushButtonWidgetClass, R_ButtonsContainer(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                            XmNlabelString, XmStringCreateSimple("Align/Grid..."),
                                                NULL);
    XtAddCallback(R_GridButton(xgr), XmNactivateCallback, DoGridOptions, xgr);
    XgAddHelpCallBackFromFile(R_GridButton(xgr), "reg_gr_button");

    R_SavedButton(xgr) = XtVaCreateManagedWidget("reg_svedbutton", xmPushButtonWidgetClass, R_ButtonsContainer(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                           XmNlabelString, XmStringCreateSimple("Saved..."),
                                                 NULL);
    XtAddCallback(R_SavedButton(xgr), XmNactivateCallback, GetSavedRegion, xgr);
    XgAddHelpCallBackFromFile(R_SavedButton(xgr), "reg_svedbutton");

    R_DefaultButton(xgr) = XtVaCreateManagedWidget("region_def_button", xmPushButtonWidgetClass, R_ButtonsContainer(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                     XmNlabelString, XmStringCreateSimple("Default Region"),
                                                   NULL);
    XtAddCallback(R_DefaultButton(xgr), XmNactivateCallback, GetDefaultRegion, xgr);
    XgAddHelpCallBackFromFile(R_DefaultButton(xgr), "reg_def_button");

    if (R_EditDefaultRegion(xgr)) {
        XtSetSensitive(R_DefaultButton(xgr), False);
    }
    R_SaveButton(xgr) = XtVaCreateManagedWidget("reg_sve_button", xmPushButtonWidgetClass, R_SaveContainer(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                     XmNlabelString, XmStringCreateSimple("Save region"),
                                                NULL);
    XtAddCallback(R_SaveButton(xgr), XmNactivateCallback, SaveRegion, xgr);
    XgAddHelpCallBackFromFile(R_SaveButton(xgr), "reg_sve_button");

    R_SaveName(xgr) = XtVaCreateManagedWidget("reg_save_name", xmTextFieldWidgetClass, R_SaveContainer(xgr),
				      XmNcolumns, 30,
				      XmNvalue, "enter_save_filename_here",
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                                                NULL);
    XgAddHelpCallBackFromFile(R_SaveName(xgr), "reg_save_name");

    if (R_EditDefaultRegion(xgr)) {
        strcpy(buf, "-");
    } else {
        G_format_northing(R_DefaultRegion(xgr).north, buf, R_DefaultRegion(xgr).proj);
    }
    sprintf(buf2, "North edge [%s]", buf);
    R_NorthLabel(xgr) = XtVaCreateManagedWidget("region_north_label", xmLabelWidgetClass, R_DataTable(xgr),
                                                XmNtraversalOn, False,
                                                XmNlabelType, XmSTRING,
                                 XmNlabelString, XmStringCreateSimple(buf2),
                                        XmNalignment, XmALIGNMENT_BEGINNING,
                                                XmNmarginRight, 20,
                                                NULL);

    if (R_EditDefaultRegion(xgr)) {
        strcpy(buf, "-");
    } else {
        G_format_northing(R_DefaultRegion(xgr).south, buf, R_DefaultRegion(xgr).proj);
    }
    sprintf(buf2, "South edge [%s]", buf);
    R_SouthLabel(xgr) = XtVaCreateManagedWidget("region_south_label", xmLabelWidgetClass, R_DataTable(xgr),
                                                XmNtraversalOn, False,
                                                XmNlabelType, XmSTRING,
                                 XmNlabelString, XmStringCreateSimple(buf2),
                                        XmNalignment, XmALIGNMENT_BEGINNING,
                                                NULL);

    if (R_EditDefaultRegion(xgr)) {
        strcpy(buf, "-");
    } else {
        G_format_easting(R_DefaultRegion(xgr).west, buf, R_DefaultRegion(xgr).proj);
    }
    sprintf(buf2, "West edge [%s]", buf);
    R_WestLabel(xgr) = XtVaCreateManagedWidget("region_west_label", xmLabelWidgetClass, R_DataTable(xgr),
                                               XmNtraversalOn, False,
                                               XmNlabelType, XmSTRING,
                                 XmNlabelString, XmStringCreateSimple(buf2),
                                        XmNalignment, XmALIGNMENT_BEGINNING,
                                               NULL);

    if (R_EditDefaultRegion(xgr)) {
        strcpy(buf, "-");
    } else {
        G_format_easting(R_DefaultRegion(xgr).east, buf, R_DefaultRegion(xgr).proj);
    }
    sprintf(buf2, "East edge [%s]", buf);
    R_EastLabel(xgr) = XtVaCreateManagedWidget("region_east_label", xmLabelWidgetClass, R_DataTable(xgr),
                                               XmNtraversalOn, False,
                                               XmNlabelType, XmSTRING,
                                 XmNlabelString, XmStringCreateSimple(buf2),
                                        XmNalignment, XmALIGNMENT_BEGINNING,
                                               NULL);

    if (R_EditDefaultRegion(xgr)) {
        strcpy(buf, "-");
    } else {
        G_format_resolution(R_DefaultRegion(xgr).ns_res, buf, R_DefaultRegion(xgr).proj);
    }
    sprintf(buf2, "North/South Res [%s]", buf);
    R_NSResLabel(xgr) = XtVaCreateManagedWidget("region_nsres_label", xmLabelWidgetClass, R_DataTable(xgr),
                                                XmNtraversalOn, False,
                                                XmNlabelType, XmSTRING,
                                 XmNlabelString, XmStringCreateSimple(buf2),
                                        XmNalignment, XmALIGNMENT_BEGINNING,
                                                NULL);

    if (R_EditDefaultRegion(xgr)) {
        strcpy(buf, "-");
    } else {
        G_format_resolution(R_DefaultRegion(xgr).ew_res, buf, R_DefaultRegion(xgr).proj);
    }
    sprintf(buf2, "East/West Res [%s]", buf);
    R_EWResLabel(xgr) = XtVaCreateManagedWidget("region_ewres_label", xmLabelWidgetClass, R_DataTable(xgr),
                                                XmNtraversalOn, False,
                                                XmNlabelType, XmSTRING,
                                 XmNlabelString, XmStringCreateSimple(buf2),
                                        XmNalignment, XmALIGNMENT_BEGINNING,
                                                NULL);

    R_NorthText(xgr) = XtVaCreateManagedWidget("reg_north_text", xmTextWidgetClass, R_DataTable(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                                               XmNwordWrap, False,
                                               XmNmaxLength, 10,
                                               XmNcolumns, 10,
                                               NULL);
    XtAddCallback(R_NorthText(xgr), XmNactivateCallback, FieldChanged, xgr);
    XgAddHelpCallBackFromFile(R_NorthText(xgr), "reg_north_text");

    R_SouthText(xgr) = XtVaCreateManagedWidget("reg_south_text", xmTextWidgetClass, R_DataTable(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                                               XmNwordWrap, False,
                                               XmNmaxLength, 10,
                                               XmNcolumns, 10,
                                               NULL);
    XtAddCallback(R_SouthText(xgr), XmNactivateCallback, FieldChanged, xgr);
    XgAddHelpCallBackFromFile(R_SouthText(xgr), "reg_south_text");

    R_WestText(xgr) = XtVaCreateManagedWidget("reg_west_text", xmTextWidgetClass, R_DataTable(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                                              XmNwordWrap, False,
                                              XmNmaxLength, 10,
                                              XmNcolumns, 10,
                                              NULL);
    XtAddCallback(R_WestText(xgr), XmNactivateCallback, FieldChanged, xgr);
    XgAddHelpCallBackFromFile(R_WestText(xgr), "reg_west_text");

    R_EastText(xgr) = XtVaCreateManagedWidget("reg_east_text", xmTextWidgetClass, R_DataTable(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                                              XmNwordWrap, False,
                                              XmNmaxLength, 10,
                                              XmNcolumns, 10,
                                              NULL);
    XtAddCallback(R_EastText(xgr), XmNactivateCallback, FieldChanged, xgr);
    XgAddHelpCallBackFromFile(R_EastText(xgr), "reg_east_text");

    R_NSResText(xgr) = XtVaCreateManagedWidget("reg_nsres_text", xmTextWidgetClass, R_DataTable(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                                               XmNwordWrap, False,
                                               XmNmaxLength, 10,
                                               XmNcolumns, 10,
                                               NULL);
    XtAddCallback(R_NSResText(xgr), XmNactivateCallback, FieldChanged, xgr);
    XgAddHelpCallBackFromFile(R_NSResText(xgr), "reg_nsres_text");

    R_EWResText(xgr) = XtVaCreateManagedWidget("reg_ewres_text", xmTextWidgetClass, R_DataTable(xgr),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                                               XmNwordWrap, False,
                                               XmNmaxLength, 10,
                                               XmNcolumns, 10,
                                               NULL);
    XtAddCallback(R_EWResText(xgr), XmNactivateCallback, FieldChanged, xgr);
    XgAddHelpCallBackFromFile(R_EWResText(xgr), "reg_ewres_text");

    R_ResultRegion(xgr) = R_InputRegion(xgr);
    PutFields((Widget) xgr, R_ResultRegion(xgr));

    R_GraphicFrame(xgr) = XtVaCreateManagedWidget("region_graphic_frame", xmFrameWidgetClass, theform, NULL);

    R_TotalHeight(xgr) = 250;
    R_TotalWidth(xgr) = R_TotalHeight(xgr) * ((R_DefaultRegion(xgr).east - R_DefaultRegion(xgr).west) / (R_DefaultRegion(xgr).north - R_DefaultRegion(xgr).south));
    CalcGraphicSize(xgr);

    R_GraphicDrawingArea(xgr) = XtVaCreateManagedWidget("region_draw_area", xmDrawingAreaWidgetClass, R_GraphicFrame(xgr), XmNtraversalOn, False,
                     XmNtranslations, XtParseTranslationTable(translations),
                                              XmNheight, R_TotalHeight(xgr),
                                                XmNwidth, R_TotalWidth(xgr),
                                                        NULL);
    XtAddCallback(R_GraphicDrawingArea(xgr), XmNresizeCallback, RegionResize, xgr);
    XtAddCallback(R_GraphicDrawingArea(xgr), XmNexposeCallback, exposeGraphic, xgr);
    XgAddHelpCallBackFromFile(R_GraphicDrawingArea(xgr), "reg_draw_area");

    RegionCreatePixmap((Widget) xgr);

    if ( DefaultDepthOfScreen(XtScreen((Widget)xgr)) == 0 ) {
        XSetForeground(dpy, R_GridGC(xgr), 
            BlackPixel(dpy,DefaultScreen(XtDisplay((Widget)xgr))));
    } else if ( R_ColorName(xgr) != NULL ) {
        R_GridColor(xgr).pixel = XgdGetVectColorPixelByName(R_ColorName(xgr));
        XQueryColor(dpy, DefaultColormapOfScreen(
             XtScreen(R_GraphicDrawingArea(xgr))), &R_GridColor(xgr));
	XSetForeground(dpy, R_GridGC(xgr), R_GridColor(xgr).pixel);
    }

    RegionAdjustAndAttach(xgr, True);
    RegionRedrawGraphic((Widget) xgr, False);
}

/****************************************************************/
static void
#ifdef _NO_PROTO
RegionAdjustAndAttach(xgr, attach)
    RegionWidget    xgr;
    Boolean         attach;
#else
RegionAdjustAndAttach(
                      RegionWidget xgr,
                      Boolean attach
)
#endif
{
    Dimension       height = 0;
    Dimension       maxHeight = 0;
    Dimension       width = 0;
    Dimension       maxLabelWidth = 0;
    Dimension       maxTextWidth = 0;
    Dimension       bbWidth;
    Dimension       maxWidth;

    if (attach) {
        XtVaSetValues(R_SaveFrame(xgr),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      XmNbottomAttachment, XmATTACH_FORM,
                      NULL);
        XtVaSetValues(R_ButtonsContainer(xgr),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      XmNbottomAttachment, XmATTACH_WIDGET,
		      XmNbottomWidget,R_SaveFrame(xgr),
                      NULL);
        XtVaSetValues(R_DataTable(xgr),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNbottomAttachment, XmATTACH_WIDGET,
                      XmNbottomWidget, R_ButtonsSeparator(xgr),
                      XmNtopAttachment, XmATTACH_FORM,
                      NULL);
        XtVaSetValues(R_ButtonsSeparator(xgr),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      XmNbottomAttachment, XmATTACH_WIDGET,
                      XmNbottomWidget, R_ButtonsContainer(xgr),
                      NULL);
        XtVaSetValues(R_GraphicFrame(xgr),
                      XmNleftAttachment, XmATTACH_WIDGET,
                      XmNleftWidget, R_DataFrame(xgr),
                      XmNtopAttachment, XmATTACH_FORM,
                      XmNbottomAttachment, XmATTACH_WIDGET,
                      XmNbottomWidget, R_ButtonsSeparator(xgr),
                      XmNrightAttachment, XmATTACH_FORM,
                      NULL);
    }
}

/****************************************************************/
static void
#ifdef _NO_PROTO
Destroy(xgr)
    RegionWidget    xgr;
#else
Destroy(
        RegionWidget xgr)
#endif
{
  /* There are no doubt memory leaks in this routine. */

  XFreeGC(XtDisplay(xgr),R_RubberGC(xgr));
  XFreeGC(XtDisplay(xgr),R_GridGC(xgr));
  XFreeGC(XtDisplay(xgr),R_FillGC(xgr));

    return;
}

/****************************************************************/
static void
#ifdef _NO_PROTO
DeleteChild(w)
    Widget          w;
#else
DeleteChild(
            Widget w)
#endif
/****************
 * This procedure is called to remove the child from
 *   the child list, and to allow the parent to do any
 *   neccessary clean up.
 ****************/
{
    RegionWidget    xgr;
    /****************/

    (*((InteractorWidgetClass) interactorWidgetClass)
     ->composite_class.delete_child) (w);
    return;
}

/****************************************************************/
static          Boolean
#ifdef _NO_PROTO
SetValues(current, request, new)
    RegionWidget    current;
    RegionWidget    request;
    RegionWidget    new;
#else
SetValues(
          RegionWidget current,
          RegionWidget request,
          RegionWidget new)
#endif
/****************
 * This routine detects differences in two versions
 *   of a widget, when a difference is found the
 *   appropriate action is taken.
 ****************/
{
    Arg             args[10];
    int             n;
    String          newString;
    /****************/

    BB_InSetValues(new) = TRUE;

    if (R_ColorName(new) != R_ColorName(current)) {
        R_ColorName(new) = R_ColorName(current);
    }
    if (R_GridMode(new) != R_GridMode(current)) {
        ToggleGrid((Widget) new,(XtPointer) new);
    }
    if (R_InputRegion(new) != R_InputRegion(current)) {
        R_InputRegion(new) = R_InputRegion(current);
    }
    BB_InSetValues(new) = FALSE;

    if (XtClass(new) == regionWidgetClass) {
        _XmBulletinBoardSizeUpdate((XmBulletinBoardWidget) new);
    }
    return (FALSE);
}

Widget
#ifdef _NO_PROTO
XgRegionGetChild(xgr, which)
    Widget          xgr;        /* Region widget  */
    unsigned char   which;      /* Which child          */
#else
XgRegionGetChild(
                 Widget xgr,    /* Region widget  */
                 unsigned char which)   /* Which child          */
#endif
/****************
 * This function returns the widget id of the
 *   specified Region child widget.
 ****************/
{
    Widget          child = NULL;

    switch (which) {
    case XgREGION_DISPLAY_AREA:
        {
            child = R_DisplayArea(xgr);
            break;
        }
    default:
        {
            child = XgInteractorGetChild(xgr, which);
            break;
        }
    }
    return (child);
}

/* Convenience routines */

Widget
#ifdef _NO_PROTO
XgCreateRegion(p, name, args, n)
    Widget          p;          /* parent widget   */
    String          name;       /* widget name     */
    ArgList         args;       /* arg list        */
    Cardinal        n;          /* arg count       */
#else
XgCreateRegion(
               Widget p,        /* parent widget   */
               String name,     /* widget name     */
               ArgList args,    /* arg list        */
               Cardinal n)      /* arg count       */
#endif
{
    return (XtCreateWidget(name, regionWidgetClass, p, args, n));
}

Widget
#ifdef _NO_PROTO
XgCreateRegionDialog(ds_p, name, xgr_args, xgr_n)
    Widget          ds_p;       /* parent for shell    */
    String          name;       /* widget name         */
    ArgList         xgr_args;   /* arglist for xgr      */
    Cardinal        xgr_n;      /* argcount for xgr     */
#else
XgCreateRegionDialog(
                     Widget ds_p,       /* parent for shell    */
                     String name,       /* widget name         */
                     ArgList xgr_args,  /* arglist for xgr      */
                     Cardinal xgr_n)    /* argcount for xgr     */
#endif
/****************
 * This convenience function creates a DialogShell
 *   and a Region child of the shell;
 *   returns the Region widget.
 ****************/
{
    Widget          xgr;        /* new xgr widget      */
    Widget          ds;         /* DialogShell         */
    Arg             ds_args[10];/* arglist for shell  */
    char           *ds_name;
    /****************/

    /*
     * Create DialogShell parent.
     */
    ds_name = XtCalloc(strlen(name) + 1, sizeof(char));
    strcpy(ds_name, name);

    XtSetArg(ds_args[0], XmNallowShellResize, True);
    ds = XmCreateDialogShell(ds_p, ds_name, ds_args, 1);

    XtFree(ds_name);

    /*
     * Create Region widget.
     */
    xgr = XtCreateWidget(name, regionWidgetClass, ds,
                         xgr_args, xgr_n);
    XtAddCallback(xgr, XmNdestroyCallback, _XmDestroyParentCallback, NULL);

    return (xgr);
}
