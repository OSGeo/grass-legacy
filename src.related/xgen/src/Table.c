/**********************************************************************
   Table.c - table widget source
 *********************************************************************/
/*******************************************************************************
Xgen was developed by Kurt Buehler, while at the Center for Advanced Decision
Support for Water and Environmental Systems (CADSWES), University of Colorado
at Boulder and at the Indiana Water Resources Research Center (IWRRC),
Purdue University for the U.S. Army Construction Engineering Research
Laboratory in support of the Geographical Resources Analysis Support
System (GRASS) software. The example scripts were developed by Ms. Christine
Poulsen of USA-CERL, much thanks goes to her for her work.

Permission to use, copy, modify and distribute without charge this software,
documentation, etc. is granted, provided that this comment is retained,
and that the names of Kurt Buehler, Christine Poulsen, CADSWES, IWRRC,
the University of Colorado at Boulder, Purdue University, or USA-CERL are not
used in advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

The author disclaims all warranties with regard to this software, including
all implied warranties of merchantability and fitness, in no event shall
the author be liable for any special, indirect or consequential damages or
any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this
software.
*******************************************************************************/
/***************
 * Header files 
 ***************/
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/Xutil.h>
#include <X11/CoreP.h>
#include <X11/RectObj.h>
#include <Xm/DrawingA.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrollBar.h>
#include <Xm/Text.h>
#include "TableP.h"

extern _XmSelectColorDefault();


/****************
 * Resource List 
 ****************/
static XtResource resources[] = {
    {
        XmNhorizontalScrollBar, XmCHorizontalScrollBar,
        XmRWindow, sizeof(Widget),
        XtOffset (XmTableWidget, table.hScrollBar),
        XmRImmediate, NULL
    },{
        XmNverticalScrollBar, XmCVerticalScrollBar,
        XmRWindow, sizeof(Widget),
        XtOffset (XmTableWidget, table.vScrollBar),
        XmRImmediate, NULL
    },{
        XmNspacing, XmCSpacing,
        XmRInt, sizeof (int),
        XtOffset (XmTableWidget, table.spacing),
        XmRImmediate,  (caddr_t) (-1)
    },{
        XmNheadingSpacing, XmCHeadingSpacing,
        XmRInt, sizeof (int),
        XtOffset (XmTableWidget, table.headingSpacing),
        XmRImmediate,  (caddr_t) (-1)
    },{
        XmNtitleFontColor, XmCTitleFontColor,
        XmRPixel, sizeof (Pixel),
        XtOffset (XmTableWidget, table.titleFontColor),
        XmRCallProc,  (caddr_t) _XmSelectColorDefault
    },{
        XmNrowHeadingFontColor, XmCRowHeadingFontColor,
        XmRPixel, sizeof (Pixel),
        XtOffset (XmTableWidget, table.rowHeadingFontColor),
        XmRCallProc,  (caddr_t) _XmSelectColorDefault
    },{
        XmNcolumnHeadingFontColor, XmCColumnHeadingFontColor,
        XmRPixel, sizeof (Pixel),
        XtOffset (XmTableWidget, table.columnHeadingFontColor),
        XmRCallProc,  (caddr_t) _XmSelectColorDefault
    },{
        XmNentryFontList, XmCEntryFontList,
        XmRFontList, sizeof (XmFontList),
        XtOffset (XmTableWidget, table.entryFontList),
        XmRString,  "fixed"
    },{
        XmNheadingFontList, XmCHeadingFontList,
        XmRFontList, sizeof (XmFontList),
        XtOffset (XmTableWidget, table.headingFontList),
        XmRString,  "*adobe*helv*o*norm*12*"
    },{
        XmNrows, XmCRows,
        XmRInt, sizeof (int),
        XtOffset (XmTableWidget, table.rows),
        XmRImmediate,  (caddr_t) 0
    },{
        XmNcolumns, XmCColumns,
        XmRInt, sizeof (int),
        XtOffset (XmTableWidget, table.columns),
        XmRImmediate,  (caddr_t) 0
    },{
        XmNrowsDisplayed, XmCRowsDisplayed,
        XmRInt, sizeof (int),
        XtOffset (XmTableWidget, table.rowsDisplayed),
        XmRImmediate,  (caddr_t) (-1)
    },{
        XmNcolumnsDisplayed, XmCColumnsDisplayed,
        XmRInt, sizeof (int),
        XtOffset (XmTableWidget, table.colsDisplayed),
        XmRImmediate,  (caddr_t) (-1)
    },{
        XmNtitleString, XmCTitleString,
        XmRXmString, sizeof (XmString),
        XtOffset (XmTableWidget, table.title),
        XmRImmediate,  NULL
    },{
        XmNcolumnHeadings, XmCColumnHeadings,
        XmRXmStringTable, sizeof (caddr_t),
        XtOffset (XmTableWidget, table.colHeadings),
        XmRStringTable,  NULL
    },{
        XmNrowHeadings, XmCRowHeadings,
        XmRXmStringTable, sizeof (caddr_t),
        XtOffset (XmTableWidget, table.rowHeadings),
        XmRStringTable,  NULL
    },{
        XmNcolumnWidth, XmCColumnWidth,
        XmRInt, sizeof (int),
        XtOffset (XmTableWidget, table.colWidth),
        XmRImmediate,  (caddr_t) 0
    },{
        XmNrowHeight, XmCRowHeight,
        XmRInt, sizeof (int),
        XtOffset (XmTableWidget, table.rowHeight),
        XmRImmediate,  (caddr_t) 0
    },{
        XmNmarginWidth, XmCMarginWidth,
        XmRInt, sizeof (int),
        XtOffset (XmTableWidget, table.marginWidth),
        XmRImmediate,  (caddr_t) 0
    },{
        XmNmarginHeight, XmCMarginHeight,
        XmRInt, sizeof (int),
        XtOffset (XmTableWidget, table.marginHeight),
        XmRImmediate,  (caddr_t) 0
    },{
        XmNseparator, XmCSeparator,
        XmRChar, sizeof (char),
        XtOffset (XmTableWidget, table.separator),
        XmRImmediate,  (caddr_t) NULL
    }
 };

/***********************************
 * Resolution independent resources
 ***********************************/
#ifdef PRE_MOTIF_1_1
static XmGetValueResource get_resources[] =
#else
static XmSyntheticResource syn_resources[] =
#endif
{
    { 
        XmNmarginWidth, 
        sizeof (int),
        XtOffset (XmTableWidget, table.marginWidth),
        _XmFromHorizontalPixels
    },{ 
        XmNmarginHeight, 
        sizeof (int),
        XtOffset (XmTableWidget, table.marginHeight),
        _XmFromVerticalPixels
    },{ 
        XmNspacing, 
        sizeof (int),
        XtOffset (XmTableWidget, table.spacing),
        _XmFromHorizontalPixels
    },{ 
        XmNheadingSpacing, 
        sizeof (int),
        XtOffset (XmTableWidget, table.headingSpacing),
        _XmFromVerticalPixels
    }
};

/*****************
 * Error messages
 *****************/
#define Message1  "Negative margin width ignored (using 0)"
#define Message2  "Negative margin height ignored (using 0)"
#define Message3  "Negative spacing ignored (using 4)"
#define Message4  "Negative heading spacing ignored (using 4)"
#define Message5  "Negative or zero rows ignored (using 1)"
#define Message6  "Negative or zero columns ignored (using 1)"
#define Message7  "Negative or zero rows displayed ignored"
#define Message8  "Negative or zero columns displayed ignored"
#define Message9  "Columns displayed greater than number of columns ignored"
#define Message10 "Rows displayed greater than number of rows ignored"
#define Message11 "Negative or zero column width ignored (using default)"

#define RC_PAD 3

/*******************************
 * Method function declarations
 *******************************/
/*******************************
 * Core Class functions
 *******************************/
static void              Initialize();
static void              ClassInitialize();
static void              ClassPartInitialize();
static void              Redisplay();
static void              Realize();
static void              Resize();
static void              Destroy();
static Boolean           SetValues();
static XtGeometryResult  QueryGeometry();
/*******************************
 * Composite Class functions
 *******************************/
static XtGeometryResult  GeometryManager();
static void              InsertChild();

/*******************************
 * Other function declarations
 *******************************/
static void CopyHeadings();

/******************************
 * Class record initialization
 ******************************/
XmTableClassRec xmTableClassRec = {
    { /***** Core Part *****/
        /* superclass            */    (WidgetClass)&xmManagerClassRec,
        /* class_name            */    "XmTable",
        /* widget_size           */    sizeof(XmTableRec),
        /* class_initialize      */    ClassInitialize,
        /* class_part_initialize */    ClassPartInitialize,
        /* class_inited          */    FALSE,
        /* initialize            */    Initialize,
        /* initialize_hook       */    NULL,
        /* realize               */    Realize,
        /* actions               */    NULL,
        /* num_actions           */    0,
        /* resources             */    resources,
        /* num_resources         */    XtNumber(resources),
        /* xrm_class             */    NULLQUARK,
        /* compress_motion       */    TRUE,
        /* compress_exposure     */    TRUE,
        /* compress_enterleave   */    TRUE,
        /* visible_interest      */    FALSE,
        /* destroy               */    Destroy,
        /* resize                */    Resize,
        /* expose                */    (XtExposeProc) Redisplay,
        /* set_values            */    (XtSetValuesFunc)SetValues,
        /* set_values_hook       */    NULL,
        /* set_values_almost     */    XtInheritSetValuesAlmost,
        /* get_values_hook       */    NULL,
        /* accept_focus          */    NULL,
        /* version               */    XtVersion,
        /* callback_offsets      */    NULL,
        /* tm_table              */    NULL,
        /* query_geometry        */    QueryGeometry,
        /* display_accelerator   */    NULL,
        /* extension             */    NULL
    },{ /***** Composite Part *****/
        /* childrens geo mgr proc*/    GeometryManager,
        /* set changed proc      */    NULL,
        /* add a child           */    InsertChild,
        /* remove a child        */    _XtInherit,
        /* extension             */    NULL,
    },{ /***** Constraint Part *****/
        /* resource list         */    NULL,
        /* num resources         */    0,
        /* constraint size       */    0,
        /* init proc             */    NULL,
        /* destroy proc          */    NULL,
        /* set values proc       */    NULL,
        /* extension             */    NULL
    },{ /***** Manager Part *****/
#ifdef PRE_MOTIF_1_1
        /* translations          */    (XtTranslations) _XtInherit,
        /* get resources         */    get_resources,
        /* num get_resources     */    XtNumber(get_resources),
        /* get_cont_resources    */    NULL,
        /* num_get_cont_resources*/    0,
#else
        /* translations          */    (String) _XtInherit,
        /* synthetic resources   */    syn_resources,
        /* num syn_resources     */    XtNumber(syn_resources),
        /* syn_cont_resources    */    NULL,
        /* num_syn_cont_resources*/    0,
#endif
        /* extension             */    NULL
    },{ /***** Table Part *****/
        /* nothing               */    0
    }
};

WidgetClass xmTableWidgetClass = (WidgetClass) &xmTableClassRec;

/***************************************************************
 * Callback Functions
 * These are the callback routines for the scrollbar actions.
 ***************************************************************/
static XtCallbackProc VertSliderMove();
static XtCallbackProc HorizSliderMove();

static XtCallbackRec VSCallBack[] =
{
   {(XtCallbackProc )VertSliderMove, (caddr_t) NULL},
   {NULL,           (caddr_t) NULL},
};

static XtCallbackRec HSCallBack[] =
{
   {(XtCallbackProc )HorizSliderMove, (caddr_t) NULL},
   {NULL,           (caddr_t) NULL},
};

/*SUPRESSS 547*/
/********************************************************************
 * VertSliderMove
 * Callback for the sliderMoved resource of the vertical scrollbar.
 ********************************************************************/
static  XtCallbackProc  VertSliderMove(w,closure,call_data)
	/*ARGSUSED*/
    Widget w;
    caddr_t  closure;
    XmScrollBarCallbackStruct *call_data;
{
    XmTableWidget tb;

    tb = (XmTableWidget )w->core.parent;
    if (!tb->table.tableRC->core.being_destroyed) {
        _XmMoveObject(tb->table.tableRC,tb->table.tableRC->core.x,
                      -((int ) call_data->value));
        if ( tb->table.hasRowHeadings )
        _XmMoveObject(tb->table.rowHeadingRC, tb->table.rowHeadingRC->core.x,
                      -((int ) call_data->value));
        tb->table.vOrigin = (int ) call_data->value;
    }
	return NULL;
}

/********************************************************************
 * HorizSliderMove
 * Callback for the sliderMoved resource of the horizontal scrollbar.
 ********************************************************************/
static  XtCallbackProc  HorizSliderMove(w,closure,call_data)
	/*ARGSUSED*/
    Widget w;
    caddr_t  closure;
    XmScrollBarCallbackStruct *call_data;
{
    XmTableWidget tb;

    tb = (XmTableWidget )w->core.parent;
    if (!tb->table.tableRC->core.being_destroyed &&
        !tb->table.headingRC->core.being_destroyed) {
        _XmMoveObject(tb->table.tableRC, -((int ) call_data->value),
            tb->table.tableRC->core.y);
        if (tb->table.hasColHeadings)
             _XmMoveObject(tb->table.headingRC, -((int ) call_data->value),
                 tb->table.headingRC->core.y);
        tb->table.vOrigin = (int ) call_data->value;
    }
	return NULL;
}

/********************************************************************
 * ClassInitialize
 ********************************************************************/
static void 
ClassInitialize(w)
	/*ARGSUSED*/
    Widget w;
{
#ifdef PRE_MOTIF_1_1
    _XmInitializeGetValuesResources(get_resources,XtNumber(get_resources));
#else

#endif
}

/********************************************************************
 * ClassPartInitialize
 ********************************************************************/
static void 
ClassPartInitialize(wc)
    register XmTableWidgetClass wc;
{
    _XmFastSubclassInit(wc,XmTABLE_BIT);
}

/********************************************************************
 * Initialize
 ********************************************************************/
static void 
Initialize(request, new)
    /* ARGSUSED */
    XmTableWidget request, new;
{
    Arg args[30];
    int n;
    int row, column;
    int shadow;
    int entry = 0;
    XtWidgetGeometry query, preferred;

/*******************************************
 * if the unit type is not pixels, convert.
 *******************************************/
    if (new->manager.unit_type != XmPIXELS) {
        if (new->table.marginWidth > 0)
            new->table.marginWidth =
                (Dimension) _XmToHorizontalPixels(new, new->manager.unit_type,
                                            new->table.marginWidth);
        if (new->table.marginHeight > 0)
            new->table.marginHeight =
                (Dimension) _XmToVerticalPixels (new, new->manager.unit_type,
                                           new->table.marginHeight);
        if (new->table.spacing > 0)
            new->table.spacing =
                (Dimension) _XmToHorizontalPixels(new, new->manager.unit_type,
                                            new->table.spacing);
        if (new->table.headingSpacing > 0)
            new->table.headingSpacing =
                (Dimension) _XmToVerticalPixels(new, new->manager.unit_type,
                                            new->table.headingSpacing);
    }

    if (new->table.marginWidth < 0) {
        new->table.marginWidth = 0;
        _XmWarning(new, Message1);
    }

    if (new->table.marginHeight < 0) {
        new->table.marginHeight = 0;
        _XmWarning(new, Message2);
    }

    if (new->table.spacing < 0) {
        new->table.spacing = 4;
    }

    if (new->table.headingSpacing < 0) {
        new->table.headingSpacing = 4;
    }

    if (new->table.rows < 1) {
        new->table.rows = 1;
        _XmWarning(new, Message5);
    }

    if (new->table.columns < 1) {
        new->table.columns = 1;
        _XmWarning(new, Message6);
    }

    if (new->table.rowsDisplayed < 0) {
        new->table.rowsDisplayed = ((new->table.rows <= 2)?(new->table.rows):2);
    }
    if (new->table.rowsDisplayed > new->table.rows) {
        new->table.rowsDisplayed = ((new->table.rows <= 2)?(new->table.rows):2);
    }
    if (new->table.colsDisplayed < 0) {
        new->table.colsDisplayed = ((new->table.columns <= 2)?(new->table.columns):2);
    }
    if (new->table.colsDisplayed > new->table.columns) {
        new->table.colsDisplayed = ((new->table.columns <= 2)?(new->table.columns):2);
    }

    if (new->table.colWidth < 0) {
        /* set to zero for now, then use whatever we get. */
        new->table.colWidth = 0;
    }

    new->manager.shadow_thickness = 2;

/*********************************************************
 * make copies of the font list and the row/column headings.
 * THE USER IS RESPONSIBLE FOR FREEING THE MEMORY.
 *********************************************************/

    new->table.headingFontList = XmFontListCopy(new->table.headingFontList);
    new->table.entryFontList = XmFontListCopy(new->table.entryFontList);
    CopyHeadings(new,1);
    CopyHeadings(new,0);
    if ( new->table.colHeadings ) new->table.hasColHeadings = True;
    else new->table.hasColHeadings = False;
    if ( new->table.rowHeadings ) new->table.hasRowHeadings = True;
    else new->table.hasRowHeadings = False;

/**********************************
 * Create title.
 **********************************/
    if ( new->table.title ) {
        n = 0;
        XtSetArg(args[n],XmNshadowThickness,new->manager.shadow_thickness); n++;
        XtSetArg(args[n],XmNbackground,new->core.background_pixel); n++;
        XtSetArg(args[n],XmNforeground,new->table.titleFontColor); n++;
        XtSetArg(args[n],XmNbackgroundPixmap,new->core.background_pixmap); n++;
        XtSetArg(args[n],XmNtopShadowColor,new->manager.top_shadow_color); n++;
        XtSetArg(args[n],XmNtopShadowPixmap,new->manager.top_shadow_pixmap); n++;
        XtSetArg(args[n],XmNbottomShadowColor,new->manager.bottom_shadow_color); n++;
        XtSetArg(args[n],XmNbottomShadowPixmap,new->manager.bottom_shadow_pixmap); n++;
        XtSetArg(args[n],XmNlabelString,new->table.title); n++;
        XtSetArg(args[n],XmNfontList,new->table.headingFontList); n++;
        new->table.title = XmCreateLabel(new,"title",args,n);
        XtManageChild(new->table.title);
    }

/**********************************
 * Create frames and clip windows.
 **********************************/
    if ( new->table.hasColHeadings ) {
        n = 0;
        XtSetArg(args[n],XmNshadowThickness,new->manager.shadow_thickness); n++;
        XtSetArg(args[n],XmNbackground,new->core.background_pixel); n++;
        XtSetArg(args[n],XmNforeground,new->manager.foreground); n++;
        new->table.headingFrame = XmCreateFrame(new,"headingClipFrame",args,n);
        XtManageChild(new->table.headingFrame);
    }
    if ( new->table.hasRowHeadings ) {
        n = 0;
        XtSetArg(args[n],XmNshadowThickness,new->manager.shadow_thickness); n++;
        XtSetArg(args[n],XmNbackground,new->core.background_pixel); n++;
        XtSetArg(args[n],XmNforeground,new->manager.foreground); n++;
        new->table.rowHeadingFrame = XmCreateFrame(new,"rowHeadingClipFrame",args,n);
        XtManageChild(new->table.rowHeadingFrame);
    }
    n = 0;
    XtSetArg(args[n],XmNshadowThickness,new->manager.shadow_thickness); n++;
    XtSetArg(args[n],XmNbackground,new->core.background_pixel); n++;
    XtSetArg(args[n],XmNforeground,new->manager.foreground); n++;
    new->table.tableFrame = XmCreateFrame(new,"tableClipFrame",args,n);
    XtManageChild(new->table.tableFrame);

    if ( new->table.hasColHeadings ) {
        n = 0;
        XtSetArg(args[n],XmNborderWidth,0); n++;
        XtSetArg(args[n],XmNmarginWidth,0); n++;
        XtSetArg(args[n],XmNmarginHeight,0); n++;
        new->table.headingWindow = XmCreateDrawingArea(new->table.headingFrame,
            "headingClipWindow",args,n);
        XtManageChild(new->table.headingWindow);
    }
    if ( new->table.hasRowHeadings ) {
        n = 0;
        XtSetArg(args[n],XmNborderWidth,0); n++;
        XtSetArg(args[n],XmNmarginWidth,0); n++;
        XtSetArg(args[n],XmNmarginHeight,0); n++;
        new->table.rowHeadingWindow = XmCreateDrawingArea(new->table.rowHeadingFrame,
            "rowHeadingClipWindow",args,n);
        XtManageChild(new->table.rowHeadingWindow);
    }
    n = 0;
    XtSetArg(args[n],XmNborderWidth,0); n++;
    XtSetArg(args[n],XmNmarginWidth,0); n++;
    XtSetArg(args[n],XmNmarginHeight,0); n++;
    new->table.tableWindow = XmCreateDrawingArea(new->table.tableFrame,
        "tableClipWindow",args,n);
    XtManageChild(new->table.tableWindow);
    /****************************************
     * Create the heading row column widget.
     ****************************************/
    if ( new->table.hasColHeadings ) {
        n = 0;
            XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
        XtSetArg(args[n],XmNnumColumns,new->table.columns); n++;
        XtSetArg(args[n],XmNpacking,XmPACK_COLUMN); n++;
        XtSetArg(args[n],XmNmarginWidth,0); n++;
        XtSetArg(args[n],XmNmarginHeight,0); n++;
        XtSetArg(args[n],XmNshadowThickness,new->manager.shadow_thickness); n++;
        XtSetArg(args[n],XmNbackground,new->core.background_pixel); n++;
        XtSetArg(args[n],XmNforeground,new->manager.foreground); n++;
        XtSetArg(args[n],XmNbackgroundPixmap,new->core.background_pixmap); n++;
        XtSetArg(args[n],XmNtopShadowColor,new->manager.top_shadow_color); n++;
        XtSetArg(args[n],XmNtopShadowPixmap,new->manager.top_shadow_pixmap); n++;
        XtSetArg(args[n],XmNbottomShadowColor,new->manager.bottom_shadow_color); n++;
        XtSetArg(args[n],XmNbottomShadowPixmap,new->manager.bottom_shadow_pixmap); n++;
        new->table.headingRC = 
            XmCreateRowColumn(new->table.headingWindow,"headingRC",args,n);
        XtManageChild(new->table.headingRC);
    
    /*****************************************
     * Add new->table.columns labels into it.
     *****************************************/
        new->table.columnHeads = (Widget *)XtCalloc(new->table.columns,sizeof(XmLabelWidget));
    
        n = 0;
        XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;
        XtSetArg(args[n],XmNfontList,new->table.headingFontList); n++;
        XtSetArg(args[n],XmNshadowThickness,new->manager.shadow_thickness); n++;
        XtSetArg(args[n],XmNbackground,new->core.background_pixel); n++;
        XtSetArg(args[n],XmNforeground,new->table.columnHeadingFontColor); n++;
        XtSetArg(args[n],XmNbackgroundPixmap,new->core.background_pixmap); n++;
        XtSetArg(args[n],XmNtopShadowColor,new->manager.top_shadow_color); n++;
        XtSetArg(args[n],XmNtopShadowPixmap,new->manager.top_shadow_pixmap); n++;
        XtSetArg(args[n],XmNbottomShadowColor,new->manager.bottom_shadow_color); n++;
        XtSetArg(args[n],XmNbottomShadowPixmap,new->manager.bottom_shadow_pixmap); n++;
        /******************************************************************
         * If the user has specified a column width or row height
         * use it no matter what.
         ******************************************************************/
        if ( new->table.colWidth )
            XtSetArg(args[n],XmNwidth,new->table.colWidth); n++;
        if ( new->table.rowHeight )
            XtSetArg(args[n],XmNwidth,new->table.colWidth); n++;
        if ( new->table.rowHeight || new->table.colWidth )
            XtSetArg(args[n],XmNrecomputeSize,False); n++;
        for ( column = 0; column < new->table.columns; column++) {
            char labelName[12];
    
            sprintf(labelName,"column%03d",column);
            if ( new->table.colHeadings )
                XtSetArg(args[n],XmNlabelString,new->table.colHeadings[column]);
            else
                XtSetArg(args[n],XmNlabelString,
                    XmStringCreateLtoR(labelName,XmSTRING_DEFAULT_CHARSET));
            new->table.columnHeads[column] =
                XmCreateLabel(new->table.headingRC,labelName,args,n + 1);
            XtManageChild(new->table.columnHeads[column]);
        }
    }

/**************************************
 * Create the row heading row column widget.
 **************************************/
    if ( new->table.hasRowHeadings ) {
        n = 0;
        XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
        XtSetArg(args[n],XmNnumColumns,new->table.rows); n++;
        XtSetArg(args[n],XmNpacking,XmPACK_COLUMN); n++;
        XtSetArg(args[n],XmNmarginWidth,0); n++;
        XtSetArg(args[n],XmNmarginHeight,0); n++;
        XtSetArg(args[n],XmNshadowThickness,new->manager.shadow_thickness); n++;
        XtSetArg(args[n],XmNbackground,new->core.background_pixel); n++;
        XtSetArg(args[n],XmNforeground,new->manager.foreground); n++;
        XtSetArg(args[n],XmNbackgroundPixmap,new->core.background_pixmap); n++;
        XtSetArg(args[n],XmNtopShadowColor,new->manager.top_shadow_color); n++;
        XtSetArg(args[n],XmNtopShadowPixmap,new->manager.top_shadow_pixmap); n++;
        XtSetArg(args[n],XmNbottomShadowColor,new->manager.bottom_shadow_color); n++;
        XtSetArg(args[n],XmNbottomShadowPixmap,new->manager.bottom_shadow_pixmap); n++;
        new->table.rowHeadingRC = 
            XmCreateRowColumn(new->table.rowHeadingWindow,"rowHeadingRC",args,n);
        XtManageChild(new->table.rowHeadingRC);
    
    /*****************************************
     * Add new->table.rows labels into it.
     *****************************************/
        new->table.rowHeads = (Widget *)XtCalloc(new->table.rows,sizeof(XmLabelWidget));
    
        n = 0;
        XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;
        XtSetArg(args[n],XmNfontList,new->table.headingFontList); n++;
        XtSetArg(args[n],XmNshadowThickness,new->manager.shadow_thickness); n++;
        XtSetArg(args[n],XmNbackground,new->core.background_pixel); n++;
        XtSetArg(args[n],XmNforeground,new->table.rowHeadingFontColor); n++;
        XtSetArg(args[n],XmNbackgroundPixmap,new->core.background_pixmap); n++;
        XtSetArg(args[n],XmNtopShadowColor,new->manager.top_shadow_color); n++;
        XtSetArg(args[n],XmNtopShadowPixmap,new->manager.top_shadow_pixmap); n++;
        XtSetArg(args[n],XmNbottomShadowColor,new->manager.bottom_shadow_color); n++;
        XtSetArg(args[n],XmNbottomShadowPixmap,new->manager.bottom_shadow_pixmap); n++;
        /******************************************************************
         * If the user has specified a column width or row height
         * use it no matter what.
         ******************************************************************/
        if ( new->table.rowHeight )
            XtSetArg(args[n],XmNwidth,new->table.colWidth); n++;
        if ( new->table.rowHeight || new->table.colWidth )
            XtSetArg(args[n],XmNrecomputeSize,False); n++;
        for ( row = 0; row < new->table.rows; row++) {
            char labelName[12];
    
            sprintf(labelName,"row%03d",row);
            XtSetArg(args[n],XmNlabelString,new->table.rowHeadings[row]);
            new->table.rowHeads[row] =
                XmCreateLabel(new->table.rowHeadingRC,labelName,args,n + 1);
            XtManageChild(new->table.rowHeads[row]);
        }
    }

/**************************************
 * Create the table row column widget.
 **************************************/
    n = 0;
    XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
    XtSetArg(args[n],XmNnumColumns,new->table.columns); n++;
    XtSetArg(args[n],XmNpacking,XmPACK_COLUMN); n++;
    XtSetArg(args[n],XmNmarginWidth,0); n++;
    XtSetArg(args[n],XmNmarginHeight,0); n++;
    XtSetArg(args[n],XmNshadowThickness,new->manager.shadow_thickness); n++;
    XtSetArg(args[n],XmNbackground,new->core.background_pixel); n++;
    XtSetArg(args[n],XmNforeground,new->manager.foreground); n++;
    XtSetArg(args[n],XmNbackgroundPixmap,new->core.background_pixmap); n++;
    XtSetArg(args[n],XmNtopShadowColor,new->manager.top_shadow_color); n++;
    XtSetArg(args[n],XmNtopShadowPixmap,new->manager.top_shadow_pixmap); n++;
    XtSetArg(args[n],XmNbottomShadowColor,new->manager.bottom_shadow_color); 
        n++;
    XtSetArg(args[n],XmNbottomShadowPixmap,new->manager.bottom_shadow_pixmap); 
        n++;
    new->table.tableRC = XmCreateRowColumn(new->table.tableWindow,"tableRC",args,n);
    XtManageChild(new->table.tableRC);

/****************************************************************
 * Add new->table.columns*new->table.rows texts into the second.
 ****************************************************************/
    new->table.tableElements = (Widget *)XtCalloc(new->table.rows*new->table.columns,
             sizeof(Widget));

    n = 0;
    XtSetArg(args[n],XmNfontList,new->table.entryFontList); n++;
    XtSetArg(args[n],XmNshadowThickness,2); n++;
    XtSetArg(args[n],XmNbackground,new->core.background_pixel); n++;
    XtSetArg(args[n],XmNforeground,new->manager.foreground); n++;
    XtSetArg(args[n],XmNbackgroundPixmap,new->core.background_pixmap); n++;
    XtSetArg(args[n],XmNtopShadowColor,new->manager.top_shadow_color); n++;
    XtSetArg(args[n],XmNtopShadowPixmap,new->manager.top_shadow_pixmap); n++;
    XtSetArg(args[n],XmNbottomShadowColor,new->manager.bottom_shadow_color); 
        n++;
    XtSetArg(args[n],XmNbottomShadowPixmap,new->manager.bottom_shadow_pixmap); 
        n++;
    /******************************************************************
     * If the user has specified a column width or row height 
     * use it no matter what.
     ******************************************************************/
    shadow = new->manager.shadow_thickness;
    if ( new->table.colWidth ) {
        XtSetArg(args[n],XmNwidth,new->table.colWidth); n++;
    } else if ( new->table.hasColHeadings ) {
    /***************************************************
     * Determine the proper colWidth.
     ***************************************************/
        bzero((char *)&query,sizeof(XtWidgetGeometry));
        query.request_mode = CWWidth | CWHeight;
        XtQueryGeometry(new->table.columnHeads[0], &query, &preferred);
        new->table.colWidth = preferred.width;
        XtSetArg(args[n],XmNwidth,new->table.colWidth); n++;
    }
    if ( new->table.rowHeight ) {
        XtSetArg(args[n],XmNheight,new->table.rowHeight); n++;
    } else if ( new->table.hasRowHeadings ) {
    /***************************************************
     * Determine the proper rowHeight.
     ***************************************************/
        bzero((char *)&query,sizeof(XtWidgetGeometry));
        query.request_mode = CWWidth | CWHeight;
        XtQueryGeometry(new->table.rowHeads[0], &query, &preferred);
        new->table.rowHeight = preferred.height;
        XtSetArg(args[n],XmNheight,new->table.rowHeight); n++;
    }
    for ( entry = 0; entry < (new->table.columns*new->table.rows); entry++) {
        char entryName[12];

        sprintf(entryName,"entry%03d",entry);
        new->table.tableElements[entry] = 
            XmCreateText(new->table.tableRC,entryName,args,n);
        XtManageChild(new->table.tableElements[entry]);
        XmAddTabGroup(new->table.tableElements[entry]);
    }

/***************************************************
 * Determine the heading and table dimensions.
 ***************************************************/
    if ( new->table.hasRowHeadings ) {
        bzero((char *)&query,sizeof(XtWidgetGeometry));
        query.request_mode = CWWidth | CWHeight;
        XtQueryGeometry(new->table.rowHeadingRC, &query, &preferred);
        new->table.rowHeadingX = new->table.marginWidth;
        new->table.rowHeadingWidth = preferred.width;
        new->table.titleX = new->table.marginWidth + new->table.rowHeadingWidth + 
                new->table.headingSpacing + 2*shadow;
    } else
        new->table.titleX = new->table.marginWidth;
    new->table.titleY = new->table.marginHeight;
    if ( new->table.title ) {
        bzero((char *)&query,sizeof(XtWidgetGeometry));
        query.request_mode = CWWidth | CWHeight;
        XtQueryGeometry(new->table.title, &query, &preferred);
        new->table.titleHeight = preferred.height;
    } else
        new->table.titleHeight = 0;
    if ( new->table.hasRowHeadings )
        new->table.headingX = new->table.marginWidth + new->table.rowHeadingWidth + 
            new->table.headingSpacing + 2*shadow;
    else 
        new->table.headingX = new->table.marginWidth;
    new->table.headingY = new->table.marginHeight + new->table.titleHeight;
    new->table.headingHeight = preferred.height;
/*****************************************************
 * Determine the proper column height for tableHeight.
 *****************************************************/
    bzero((char *)&query,sizeof(XtWidgetGeometry));
    query.request_mode = CWWidth | CWHeight;
    XtQueryGeometry(new->table.tableElements[0], &query, &preferred);

    new->table.headingWidth = new->table.colsDisplayed*preferred.width + 
        (new->table.colsDisplayed - 1)*RC_PAD;
    new->table.titleWidth = new->table.headingWidth;
    if ( new->table.hasRowHeadings )
        new->table.tableX = new->table.marginWidth + new->table.rowHeadingWidth + 
            new->table.headingSpacing + 2*shadow;
    else 
        new->table.tableX = new->table.marginWidth;
    if ( new->table.hasColHeadings )
        new->table.tableY = new->table.marginHeight + new->table.headingHeight + 
            new->table.titleHeight + new->table.headingSpacing + 2*shadow;
    else
        new->table.tableY = new->table.marginHeight + new->table.titleHeight + new->table.headingSpacing;
    new->table.tableWidth = new->table.headingWidth;
    new->table.tableHeight = new->table.rowsDisplayed*(preferred.height) +
        (new->table.rowsDisplayed - 1)*RC_PAD;
    if ( new->table.hasRowHeadings ) {
         new->table.rowHeadingY = new->table.tableY;
         new->table.rowHeadingHeight = new->table.tableHeight;
    }

/****************************************
 * set values for the title, the heading 
 * and table frames and clip windows.
 ****************************************/
    if ( new->table.hasRowHeadings ) {  
        n = 0;
        XtSetArg(args[n],XmNx,new->table.rowHeadingX); n++;
        XtSetArg(args[n],XmNy,new->table.rowHeadingY); n++;
        XtSetArg(args[n],XmNwidth,new->table.rowHeadingWidth); n++;
        XtSetArg(args[n],XmNheight,new->table.rowHeadingHeight); n++;
        XtSetValues(new->table.rowHeadingFrame,args,n);
        n = 0;
        XtSetArg(args[n],XmNx,new->table.rowHeadingX); n++;
        XtSetArg(args[n],XmNy,new->table.rowHeadingY); n++;
        XtSetArg(args[n],XmNwidth,new->table.rowHeadingWidth); n++;
        XtSetArg(args[n],XmNheight,new->table.rowHeadingHeight); n++;
        XtSetArg(args[n],XmNresizePolicy,XmRESIZE_NONE); n++;
        XtSetValues(new->table.rowHeadingWindow,args,n);
    }
    if ( new->table.title ) {  
        n = 0;
        XtSetArg(args[n],XmNx,new->table.titleX); n++;
        XtSetArg(args[n],XmNy,new->table.titleY); n++;
        XtSetArg(args[n],XmNwidth,new->table.titleWidth); n++;
        XtSetArg(args[n],XmNheight,new->table.titleHeight); n++;
        XtSetValues(new->table.title,args,n);
    }
    if ( new->table.hasColHeadings ) {  
        n = 0;
        XtSetArg(args[n],XmNx,new->table.headingX); n++;
        XtSetArg(args[n],XmNy,new->table.headingY); n++;
        XtSetArg(args[n],XmNwidth,new->table.headingWidth); n++;
        XtSetArg(args[n],XmNheight,new->table.headingHeight); n++;
        XtSetValues(new->table.headingFrame,args,n);
        n = 0;
        XtSetArg(args[n],XmNx,0); n++;
        XtSetArg(args[n],XmNy,0); n++;
        XtSetArg(args[n],XmNwidth,new->table.headingWidth); n++;
        XtSetArg(args[n],XmNheight,new->table.headingHeight); n++;
        XtSetArg(args[n],XmNresizePolicy,XmRESIZE_NONE); n++;
        XtSetValues(new->table.headingWindow,args,n);
    }

    n = 0;
    XtSetArg(args[n],XmNx,new->table.tableX); n++;
    XtSetArg(args[n],XmNy,new->table.tableY); n++;
    XtSetArg(args[n],XmNwidth,new->table.tableWidth); n++;
    XtSetArg(args[n],XmNheight,new->table.tableHeight); n++;
    XtSetValues(new->table.tableFrame,args,n);
    n = 0;
    XtSetArg(args[n],XmNx,0); n++;
    XtSetArg(args[n],XmNy,0); n++;
    XtSetArg(args[n],XmNwidth,new->table.tableWidth); n++;
    XtSetArg(args[n],XmNheight,new->table.tableHeight); n++;
    XtSetArg(args[n],XmNresizePolicy,XmRESIZE_NONE); n++;
    XtSetValues(new->table.tableWindow,args,n);

/****************************************************
 * Create the scrollbars and update internal fields.
 ****************************************************/
    if ( new->table.hasRowHeadings ) 
        new->table.vsbX = new->table.marginWidth + new->table.rowHeadingWidth + new->table.headingSpacing +
            new->table.tableWidth + new->table.spacing + 4*shadow;
    else
        new->table.vsbX = new->table.marginWidth + new->table.tableWidth + new->table.spacing + 2*shadow;
    if ( new->table.hasColHeadings )
        new->table.vsbY = new->table.marginHeight + new->table.titleHeight + new->table.headingHeight + 
               new->table.headingSpacing + 2*shadow;
    else
        new->table.vsbY = new->table.marginHeight + new->table.titleHeight + new->table.headingSpacing;
    if ( new->table.hasRowHeadings ) 
        new->table.hsbX = new->table.marginWidth + new->table.rowHeadingWidth + 
            new->table.headingSpacing + 2*shadow;
    else
        new->table.hsbX = new->table.marginWidth;
    if ( new->table.hasColHeadings )
        new->table.hsbY = new->table.marginHeight + new->table.titleHeight + 
               new->table.headingHeight + new->table.headingSpacing + 
               new->table.tableHeight + new->table.spacing + 4*shadow;
    else
        new->table.hsbY = new->table.marginHeight + new->table.titleHeight + 
               new->table.headingSpacing + new->table.tableHeight + new->table.spacing + 2*shadow;

    new->table.hsbWidth = new->table.tableWidth + 2*shadow;
    new->table.vsbHeight = new->table.tableHeight + 2*shadow;

    new->table.vMin = 0; 
    new->table.vMax = new->table.rows*(preferred.height) + (new->table.rows- 1)*RC_PAD;
    new->table.vOrigin = 0; 
    new->table.vExtent = new->table.tableHeight;

    new->table.hMin = 0; 
    new->table.hMax = new->table.columns*preferred.width + (new->table.columns - 1)*RC_PAD;
    new->table.hOrigin = 0; 
    new->table.hExtent = new->table.tableWidth;
    
    new->table.hasVSB = FALSE;
    if ( new->table.rowsDisplayed < new->table.rows ) {
        new->table.hasVSB = TRUE;
        n = 0;
        XtSetArg(args[n],XmNx,new->table.vsbX); n++;
        XtSetArg(args[n],XmNy,new->table.vsbY); n++;
        XtSetArg(args[n],XmNheight,new->table.vsbHeight); n++;
        XtSetArg(args[n],XmNshadowThickness,new->manager.shadow_thickness); n++;
        XtSetArg(args[n],XmNbackground,new->core.background_pixel); n++;
        XtSetArg(args[n],XmNforeground,new->manager.foreground); n++;
        XtSetArg(args[n],XmNbackgroundPixmap,new->core.background_pixmap); n++;
        XtSetArg(args[n],XmNtopShadowColor,new->manager.top_shadow_color); n++;
        XtSetArg(args[n],XmNtopShadowPixmap,
            new->manager.top_shadow_pixmap); n++;
        XtSetArg(args[n],XmNbottomShadowColor,
            new->manager.bottom_shadow_color); n++;
        XtSetArg(args[n],XmNbottomShadowPixmap,
            new->manager.bottom_shadow_pixmap); n++;
        XtSetArg(args[n],XmNminimum,new->table.vMin); n++; 
        XtSetArg(args[n],XmNmaximum,new->table.vMax); n++; 
        XtSetArg(args[n],XmNvalue,new->table.vOrigin); n++; 
        XtSetArg(args[n],XmNsliderSize,new->table.vExtent); n++; 
        XtSetArg(args[n],XmNincrement,preferred.height + RC_PAD); n++;
        XtSetArg(args[n],XmNincrementCallback, (XtArgVal) VSCallBack); n++;
        XtSetArg(args[n],XmNdecrementCallback, (XtArgVal) VSCallBack); n++;
        XtSetArg(args[n],XmNpageIncrementCallback, (XtArgVal) VSCallBack); n++;
        XtSetArg(args[n],XmNpageDecrementCallback, (XtArgVal) VSCallBack); n++;
        XtSetArg(args[n],XmNdragCallback, (XtArgVal) VSCallBack); n++;
        XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
        new->table.vScrollBar = XtCreateManagedWidget("vScrollBar", 
                          xmScrollBarWidgetClass,(Widget)new, args, n);
        XtManageChild(new->table.vScrollBar);
        XmAddTabGroup(new->table.vScrollBar);
    }
    new->table.hasHSB = FALSE;
    if ( new->table.colsDisplayed < new->table.columns ) {
        new->table.hasHSB = TRUE;
        n = 0;
        XtSetArg(args[n],XmNx,new->table.hsbX); n++;
        XtSetArg(args[n],XmNy,new->table.hsbY); n++;
        XtSetArg(args[n],XmNwidth,new->table.hsbWidth); n++;
        XtSetArg(args[n],XmNshadowThickness,new->manager.shadow_thickness); n++;
        XtSetArg(args[n],XmNbackground,new->core.background_pixel); n++;
        XtSetArg(args[n],XmNforeground,new->manager.foreground); n++;
        XtSetArg(args[n],XmNbackgroundPixmap,new->core.background_pixmap); n++;
        XtSetArg(args[n],XmNtopShadowColor,new->manager.top_shadow_color); n++;
        XtSetArg(args[n],XmNtopShadowPixmap,
            new->manager.top_shadow_pixmap); n++;
        XtSetArg(args[n],XmNbottomShadowColor,
            new->manager.bottom_shadow_color); n++;
        XtSetArg(args[n],XmNbottomShadowPixmap,
            new->manager.bottom_shadow_pixmap); n++;
        XtSetArg(args[n],XmNminimum,new->table.hMin); n++; 
        XtSetArg(args[n],XmNmaximum,new->table.hMax); n++; 
        XtSetArg(args[n],XmNvalue,new->table.hOrigin); n++; 
        XtSetArg(args[n],XmNsliderSize,new->table.hExtent); n++; 
        XtSetArg(args[n],XmNincrement,preferred.width + RC_PAD); n++;
        XtSetArg(args[n], XmNincrementCallback, (XtArgVal) HSCallBack); n++;
        XtSetArg(args[n], XmNdecrementCallback, (XtArgVal) HSCallBack); n++;
        XtSetArg(args[n], XmNpageIncrementCallback, (XtArgVal) HSCallBack); n++;
        XtSetArg(args[n], XmNpageDecrementCallback, (XtArgVal) HSCallBack); n++;
        XtSetArg(args[n], XmNdragCallback, (XtArgVal) HSCallBack); n++;
        XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
        new->table.hScrollBar = XtCreateManagedWidget("hScrollBar", 
                          xmScrollBarWidgetClass,(Widget)new, args, n);
        XtManageChild(new->table.hScrollBar);
        XmAddTabGroup(new->table.hScrollBar);
    }

/**********************************************************
 * Set the core width and height to the size of the table.
 **********************************************************/
    if ( new->table.hasVSB ) {
        bzero((char *)&query,sizeof(XtWidgetGeometry));
        query.request_mode = CWWidth;
        XtQueryGeometry(new->table.vScrollBar, &query, &preferred);
    }
    new->core.width = 2*new->table.marginWidth + new->table.tableWidth + 2*shadow +
        ((new->table.hasVSB)?(new->table.spacing + preferred.width):0) +
        ((new->table.hasRowHeadings)?
            (new->table.rowHeadingWidth + new->table.headingSpacing + 2*shadow):0);
    if ( new->table.hasHSB ) {
        bzero((char *)&query,sizeof(XtWidgetGeometry));
        query.request_mode = CWHeight;
        XtQueryGeometry(new->table.hScrollBar, &query, &preferred);
    }
    new->core.height = 2*new->table.marginHeight + new->table.titleHeight +
        ((new->table.hasColHeadings)?(new->table.headingHeight + 2*shadow):0)
        + new->table.tableHeight + new->table.headingSpacing +  2*shadow +
        ((new->table.hasHSB)?(new->table.spacing + preferred.height):0);

}

static void
Destroy(tb)
    XmTableWidget tb;
{
    if ( tb->table.hasColHeadings )
        XtFree(tb->table.columnHeads);
    if ( tb->table.hasRowHeadings )
        XtFree(tb->table.rowHeads);
    XtFree(tb->table.tableElements);
}

static void
Resize(tb)
    /* ARGSUSED */
    XmTableWidget tb;
{
    /* do nothing */
}

static void
Realize(w, p_valueMask, attributes)
	/*ARGSUSED*/
    register Widget w;
    Mask *p_valueMask;
    XSetWindowAttributes *attributes;
{
    Mask valueMask = *p_valueMask;

    valueMask |= CWDontPropagate;
    attributes->do_not_propagate_mask =
        ButtonPressMask | ButtonReleaseMask |
        KeyPressMask | KeyReleaseMask | PointerMotionMask;

    XtCreateWindow (w, InputOutput, CopyFromParent, valueMask, attributes);
}

static void
Redisplay(tb, event, region)
    XmTableWidget tb;
    XEvent *event;
    Region region;
{
   _XmRedisplayGadgets (tb, event, region);
}

static Boolean
SetValues(current,request,new)
    /* ARGSUSED */
    XmTableWidget current,request,new;
{
    Boolean flag = FALSE;

    if ( new->manager.unit_type != XmPIXELS) {
        if (new->table.marginWidth != current->table.marginWidth) {
            new->table.marginWidth = _XmToHorizontalPixels (new, 
                new->manager.unit_type,new->table.marginWidth);
            flag = TRUE;
        }
        if (new->table.marginHeight != current->table.marginHeight) {
            new->table.marginHeight = _XmToHorizontalPixels (new, 
                new->manager.unit_type,new->table.marginHeight);
            flag = TRUE;
        }
        if (new->table.spacing != current->table.spacing) {
            new->table.spacing = _XmToHorizontalPixels (new, 
                new->manager.unit_type,new->table.spacing);
            flag = TRUE;
        }
        if (new->table.headingSpacing != current->table.headingSpacing) {
            new->table.headingSpacing = _XmToHorizontalPixels (new, 
                new->manager.unit_type,new->table.headingSpacing);
            flag = TRUE;
        }
    }
    if (new->table.marginWidth < 0) {
        new->table.marginWidth = current->table.marginWidth;
        _XmWarning(new,Message1);
    }
    if (new->table.marginHeight < 0) {
        new->table.marginHeight = current->table.marginHeight;
        _XmWarning(new,Message2);
    }
    if (new->table.spacing < 0) {
        new->table.spacing = current->table.spacing;
        _XmWarning(new,Message3);
    }
    if (new->table.headingSpacing < 0) {
        new->table.headingSpacing = current->table.headingSpacing;
        _XmWarning(new,Message4);
    }
    if (new->table.entryFontList != current->table.entryFontList) {
        if ( new->table.entryFontList != NULL ) {
            XmFontListFree(current->table.entryFontList);
            new->table.entryFontList = XmFontListCopy(new->table.entryFontList);
        }
    }
    if (new->table.headingFontList != current->table.headingFontList) {
        if ( new->table.headingFontList != NULL ) {
            XmFontListFree(current->table.headingFontList);
            new->table.headingFontList = 
                XmFontListCopy(new->table.headingFontList);
        }
    }
	return flag;
}

static XtGeometryResult
QueryGeometry(tb,intended,preferred)
    XmTableWidget tb;
    XtWidgetGeometry *intended,*preferred;
{
/*******************************************
 * We will allow no changes in size once the
 * table is defined so this function returns
 * the size of the widget in all cases.
 *******************************************/
    preferred->request_mode = CWWidth|CWHeight;
    preferred->width = tb->core.width;
    preferred->height = tb->core.height;
    if ( intended->request_mode != 0 )
        return(XtGeometryNo);
    return(XtGeometryAlmost);
}

static XtGeometryResult
GeometryManager(tb,intended,preferred)
    /* ARGSUSED */
    XmTableWidget tb;
    XtWidgetGeometry *intended,*preferred;
{
/*******************************************
 * We will allow no changes in geometry once 
 * the table is defined so this function 
 * returns XtGeometryNo in all cases.
 *******************************************/
    return(XtGeometryNo);
}

static void
InsertChild(w)
	/*ARGSUSED*/
    Widget w;
{
    XmManagerWidgetClass superclass;

    superclass = (XmManagerWidgetClass)xmManagerWidgetClass;

    (*superclass->composite_class.insert_child)(w);
    return;
}

/*************************************************
 * CopyHeadings - Copy headings.
 *************************************************/
static void 
CopyHeadings(tb,rowOrCol)
    XmTableWidget tb;
    int rowOrCol;
{
    register int column;
    register int row;
    XmString    *headings;

    if ( rowOrCol ) {
        if (tb->table.rowHeadings && tb->table.rows) {
            headings = (XmString *)XtMalloc(sizeof(XmString)*(tb->table.rows));
            for (row = 0; row < tb->table.rows; row++)
                headings[row] =  XmStringCopy(tb->table.rowHeadings[row]);
    
            tb->table.rowHeadings = headings;
            for (row = 0; row < tb->table.rows; row++)
                tb->table.rowHeadings[row] = headings[row];
        }
    } else {
        if (tb->table.colHeadings && tb->table.columns) {
            headings = (XmString *)XtMalloc(sizeof(XmString)*(tb->table.columns));
            for (column = 0; column < tb->table.columns; column++)
                headings[column] =  XmStringCopy(tb->table.colHeadings[column]);
    
            tb->table.colHeadings = headings;
            for (column = 0; column < tb->table.columns; column++)
                tb->table.colHeadings[column] = headings[column];
        }
    }
}

/****************
 * API Functions
 ****************/

Widget
XmCreateTable(parent, name, args, argCount)
    Widget   parent;
    char     *name;
    ArgList  args;
    Cardinal argCount;
{
    return(XtCreateWidget(name,xmTableWidgetClass,parent,args,argCount));
}

char *
XmTableGetValue(tb,row,col)
    XmTableWidget tb;
    int row, col;
{
    Arg targs[2];
    char *text;
    int element;
    int n = 0;

    element = ((row-1)+(col-1)*tb->table.rows);
    XtSetArg(targs[n],XmNvalue,&text); n++;
    XtGetValues(tb->table.tableElements[element],targs,n);
    return text;
}

void
XmTableSetValue(tb,row,col,value)
    XmTableWidget tb;
    int row, col;
    char *value;
{
    Arg targs[2];
    int element = ((row-1)+(col-1)*tb->table.rows);
    int n = 0;
    XtSetArg(targs[n],XmNvalue,value); n++;
    XtSetValues(tb->table.tableElements[element],targs,n);
}

char *
XmTableGetRow(tb,row)
    XmTableWidget tb;
    int row;
{
    char **coltext;
    char *text;
    Arg targs[2];
    int element;
    int bytes = 0;
    int i;

    coltext = (char **) XtCalloc(tb->table.columns, sizeof (char *));
    bzero((char *)coltext,tb->table.columns*sizeof (char *));

    for ( i = 0; i < tb->table.columns; i++ ) {
        element = ((row-1)+i*tb->table.rows);
        XtSetArg(targs[0],XmNvalue,&coltext[i]);
        XtGetValues(tb->table.tableElements[element],targs,1);
        bytes += strlen(coltext[i]) + 2;
    }
    text = XtMalloc(bytes);
    for ( i = 0; i < tb->table.columns; i++ ) {
        if ( i == 0 ) 
            strcpy(text,coltext[i]);
        else
            strcat(text,coltext[i]);
        if ( (i != (tb->table.rows - 1) || tb->table.rows == 1) &&
              i != (tb->table.columns - 1) ) {
            if ( tb->table.separator == '\0' )
                strcat(text," ");
            else {
                char sep[1];

                sprintf(sep,"%c",tb->table.separator);
                strcat(text,sep);
            }
        }
    }
    XtFree(coltext);
    return text;
}

void
XmTableSetRow(tb,row,values)
    XmTableWidget tb;
    int row;
    char *values[];
{
    int i;

    for ( i = 0; i < tb->table.columns; i++ ) {
        XmTableSetValue(tb,row,i+1,values[i]);
    }
}

void
XmTableSetColumn(tb,column,values)
    XmTableWidget tb;
    int column;
    char *values[];
{
    int i;

    for ( i = 0; i < tb->table.rows; i++ ) {
        XmTableSetValue(tb,i+1,column,values[i]);
    }
}

char *
XmTableGetColumn(tb,column)
    XmTableWidget tb;
    int column;
{
    char **rowtext;
    char *text;
    Arg targs[2];
    int element;
    int bytes = 0;
    int i;

    rowtext = (char **) XtCalloc(tb->table.rows, sizeof (char *));
    bzero((char *)rowtext,tb->table.rows*sizeof (char *));

    for ( i = 0; i < tb->table.rows; i++ ) {
        element = (i+(column-1)*tb->table.rows);
        XtSetArg(targs[0],XmNvalue,&rowtext[i]);
        XtGetValues(tb->table.tableElements[element],targs,1);
        bytes += strlen(rowtext[i]) + 2;
    }
    text = XtMalloc(bytes);
    for ( i = 0; i < tb->table.rows; i++ ) {
        if ( i == 0 )
            strcpy(text,rowtext[i]);
        else
            strcat(text,rowtext[i]);
        if ( (i != (tb->table.columns - 1) || tb->table.columns == 1) &&
               i != (tb->table.rows - 1) )
            if ( tb->table.separator == '\0' )
                strcat(text," ");
            else {
                char sep[1];

                sprintf(sep,"%c",tb->table.separator);
                strcat(text,sep);
            }
    }
    XtFree(rowtext);

    return text;
}
