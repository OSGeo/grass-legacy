#ifndef _XGRASS_DLIB_H
#define _XGRASS_DLIB_H

#include "xgrass_lib.h"
#include "bbox.h"
#include "object.h"
#include "patterns.h"
#include "gis.h"
#include "Caption.h"
#include "xpmicon.h"


#define CROSS   0
#define DIAMOND 1
#define RECT    2
#define PLUS    3
#define XGD_SITESIZE 0
#define XGD_SITELW   1 

void XgdUpdateSiteStd(
#ifndef _NO_PROTO
     XgdSiteInfo *sinfo,
     int icontype,
     int size,
     int width,
     Pixel color
#endif
);
void XgdSetStdSiteIconType(
#ifndef _NO_PROTO
    XgdSiteInfo *sinfo,
    int icontype
#endif
);

void XgdSetStdSiteSize(
#ifndef _NO_PROTO
    XgdSiteInfo *sinfo,
    int size
#endif
);

void XgdSetStdSiteWidth(
#ifndef _NO_PROTO
    XgdSiteInfo *sinfo,
    int width
#endif
);

void XgdSetStdSiteColor(
#ifndef _NO_PROTO
    XgdSiteInfo *sinfo,
    Pixel color
#endif
);

void XgdInitSite(
#ifndef _NO_PROTO
     XgdObject *obj,
     char      *name,
     char      *mapset,
     int       type
#endif
);

void XgdDrawSite(
#ifndef _NO_PROTO
     XgdObject *obj,
     Window    window,
     char      *name,
     char      *mapset,
     char      *sitefile
#endif
);

void _xgdDrawSiteStd(
#ifndef _NO_PROTO
     XgdObject *obj,
     XgdSiteInfo *s
#endif
);

void _xgdDrawSitePixmap(
#ifndef _NO_PROTO
     XgdObject *obj,
     Window    window,
     XgdSiteInfo *s,
     char	*name,
    char *mapset,
     Boolean flag
#endif
);

void XgdSiteCross(
#ifndef _NO_PROTO
     XgdObject *obj,
     XgdSiteInfo *s
#endif
);

void XgdSiteDiamond(
#ifndef _NO_PROTO
     XgdObject *obj,
     XgdSiteInfo *s
#endif
);

void XgdSiteBox(
#ifndef _NO_PROTO
     XgdObject *obj,
     XgdSiteInfo *s
#endif
);

void XgdSitePlus(
#ifndef _NO_PROTO
     XgdObject *obj,
     XgdSiteInfo *s
#endif
);

void XgdUpdateSitePixmap(
#ifndef _NO_PROTO
     XgdObject *obj,
     char *sitefile,
     char *name,
     char *mapset
#endif
);

void _xgdDrawSiteStd(
#ifndef _NO_PROTO
     XgdObject *obj,
     XgdSiteInfo *s
#endif
);

void _xgdReDrawSitePixmap(
#ifndef _NO_PROTO
     XgdObject *obj,
     XgdSiteInfo *s,
#endif
);			  

void XgdSitePixmap(
#ifndef _NO_PROTO
     XgdObject *obj,
     Window win,
     XpmIcon view,
     int xhotspot,
     int yhotspot;
     char *name,
     char *mapset;
#endif
);

/* updtestdsite.c */
void XgdSetStdSiteIconType(
#ifndef _NO_PROTO
XgdObject *obj, int icontype
#endif
);
void XgdSetStdSiteSize(
#ifndef _NO_PROTO
XgdObject *obj, int size
#endif
);
void XgdSetStdSiteWidth(
#ifndef _NO_PROTO
XgdObject *obj, int width
#endif
);
void XgdSetStdSiteColor(
#ifndef _NO_PROTO
XgdObject *obj, int color
#endif
);

/* drawsitepix.c */

void XgdDrawSitefile(
#ifndef _NO_PROTO
     XgdObject *obj,
     char	*name,
     char       *mapset
#endif
);

void XgdSitePixmap(
#ifndef _NO_PROTO
     XgdObject *obj,
     Window win,
     XpmIcon view,
     int xhotspot,
     int yhotspot
#endif
);

void _xgdDrawSitePixmap(
#ifndef _NO_PROTO
     XgdObject *obj,
     Window    window,
     XgdSiteInfo *s,
     char	*name,
     char       *mapset,
     Boolean flag
#endif
);

void XgdDrawSite(
#ifndef _NO_PROTO
     XgdObject *obj,
     Window    window,
     char      *name,
     char      *mapset,
     int       type,
     char      *sitefile
#endif
);

/***********/
/* label.c */
/***********/
void XgdSetLabelColor(
#ifndef _NO_PROTO
    Object *obj,
    Pixel color
#endif
);

void XgdSetLabelFontName(
#ifndef _NO_PROTO
    XgdObject *obj,
    char *name
#endif
);

void XgdSetLabelFont(
#ifndef _NO_PROTO
    XgdObject *obj,
    XFontStruct *font
#endif
);
void XgdSetLabelString(
#ifndef _NO_PROTO
    XgdObject *obj,
    char *string
#endif
);

void XgdDrawLabel(
#ifndef _NO_PROTO
    XgdObject *obj
#endif
);

/* alloccolor.c */
Status _XgdAllocCells(
#ifndef _NO_PROTO
Display dpy, XColor *xcolor
#endif
);


/*barscale.c*/
void XgdUpdateBarscale(
#ifndef _NO_PROTO
    XgdObject *obj,
    double length,
    int linewidth,
    Pixel color,
    int style,
    int unit,
    Font fid,
    Pixel textcolor,
    int winwidth
#endif
);
void XgdSetBarscaleLength(
#ifndef _NO_PROTO
XgdObject *obj, int length
#endif
);
void XgdSetBarscaleInterval(
#ifndef _NO_PROTO
XgdObject *obj, int interval
#endif
);
void XgdSetBarscaleLineWidth(
#ifndef _NO_PROTO
XgdObject *obj, int linewidth
#endif
);
void XgdSetBarscaleColor(
#ifndef _NO_PROTO
XgdObject *obj, Pixel color
#endif
);
void XgdSetBarscaleStyle(
#ifndef _NO_PROTO
XgdObject *obj, int style
#endif
);
void XgdSetBarscaleUnit(
#ifndef _NO_PROTO
XgdObject *obj, int unit
#endif
);
void XgdSetBarscaleFontName(
#ifndef _NO_PROTO
XgdObject *obj, char *name
#endif
);
void XgdSetBarscaleFont(
#ifndef _NO_PROTO
XgdObject *obj, Font fid
#endif
);
void XgdSetBarscaleTextColor(
#ifndef _NO_PROTO
XgdObject *obj, Pixel textcolor
#endif
);
void XgdSetBarscaleWinWidth(
#ifndef _NO_PROTO
XgdObject *obj, int winwidth
#endif
);
/* error.c */
void XgdError(
#ifndef _NO_PROTO
char *s
#endif
);

/* grid.c */
void XgdUpdateGrid(
#ifndef _NO_PROTO
XgdObject *obj, Boolean onOff, double gap, int spacing, 
Pixel color, int lw, int lp, Font fid, Pixel textcolor
#endif
);
void XgdCalculateGridOffset(
#ifndef _NO_PROTO
XgdObject *obj, double coord, int round, int nOrE
#endif
);
void XgdSetGridLabelOn(
#ifndef _NO_PROTO
    XgdObject *obj,
    int labelon
#endif
);

void XgdSetGridGap(
#ifndef _NO_PROTO
    XgdObject *obj,
    double gap
#endif
);

void XgdSetGridSpacing(
#ifndef _NO_PROTO
    XgdObject *obj,
    double spacing
#endif
);

void XgdSetGridColor(
#ifndef _NO_PROTO
    XgdObject *obj,
    Pixel fg
#endif
);

void XgdSetGridLinePattern(
#ifndef _NO_PROTO
   XgdObject *obj,
   int lw,
   int lp
#endif
);

void XgdSetGridFontName(
#ifndef _NO_PROTO
XgdObject *obj, char *name
#endif
);
void XgdSetGridFont(
#ifndef _NO_PROTO
XgdObject *obj, Font fid
#endif
);
void XgdSetGridTextColor(
#ifndef _NO_PROTO
XgdObject *obj, Pixel textcolor
#endif
);

void XgdSetGridOffset(
#ifndef _NO_PROTO
    XgdObject *obj,
    int xoff,
    int yoff
#endif
);

Boolean XgdIsGridOn(
#ifndef _NO_PROTO
XgdObject *obj
#endif
);

/* linebutton.c */
Widget XgdCreateLinePatternPad(
#ifndef _NO_PROTO
Widget parent, char *string, int cols, int orientation, void (*callback)()
#endif
);

#define XGD_DASHED_NONE  0
#define XGD_DASHED_SOLID 1
#define XGD_DASHED       2

#define XGD_LINE_PATTERN_NONE         0
#define XGD_LINE_PATTERN_SOLID        1
#define XGD_LINE_PATTERN_DOTTED       2
#define XGD_LINE_PATTERN_DOT_DASHED   3
#define XGD_LINE_PATTERN_ODD_DASHED   4
#define XGD_LINE_PATTERN_LONG_DASHED  5
#define XGD_LINE_PATTERN_SHORT_DASHED 6
#define XGD_LINE_PATTERN_MAXIMUM      6

#define UNKNOWN        0
#define XGD_KILOMETERS 1
#define XGD_METERS     2
#define XGD_MILES      3
#define XGD_FEET       4

#define DASHED                  0
#define TICKED                  1

typedef struct _XGDlinePatternButtonTable {
    char *label;
    int mode;
    char *dashes;
    int n;
} XgdLinePattern;

static char __xgd_dot[] = { 3, 3 };
static char __xgd_dot_dash[] = { 9, 3, 3, 3};
static char __xgd_odd_dash[] = { 2, 2, 3, 2};
static char __xgd_long_dash[] = { 9, 3 };
static char __xgd_short_dash[] = { 6, 3 };

static XgdLinePattern __XGDlinePatternButtonTable[] = {
    { "None",       XGD_DASHED_NONE,  NULL, 0},
    { "Solid",      XGD_DASHED_SOLID, NULL, 0},
    { "Dot",        XGD_DASHED, __xgd_dot, XtNumber(__xgd_dot)},
    { "Dot Dash",   XGD_DASHED, __xgd_dot_dash, XtNumber(__xgd_dot_dash)},
    { "Odd Dash",   XGD_DASHED, __xgd_odd_dash, XtNumber(__xgd_odd_dash)},
    { "Long Dash",  XGD_DASHED, __xgd_long_dash, XtNumber(__xgd_long_dash)},
    { "Short Dash", XGD_DASHED, __xgd_short_dash, XtNumber(__xgd_short_dash)}
};

typedef struct _linePattButtonData {
    XgdLinePattern *linePattern;
    GC gc;
} _XgdLinePatternButtonData;

/* fillbutton.c */
Widget XgdCreateFillPatternPad(
#ifndef _NO_PROTO
    Widget parent,
    char *string
#endif
);

#define XGD_FILL_PATTERN_NONE       0
#define XGD_FILL_PATTERN_SOLID      1
#define XGD_FILL_PATTERN_VERTICAL   2
#define XGD_FILL_PATTERN_CHECK      3
#define XGD_FILL_PATTERN_CROSS      4
#define XGD_FILL_PATTERN_HORIZONTAL 5
#define XGD_FILL_PATTERN_LEFT       6
#define XGD_FILL_PATTERN_RIGHT      7
#define XGD_FILL_PATTERN_MAXIMUM    7

static struct _XGDfillPatternButtonTable {
    char *label;
    BITMAPS_H_TYPE *bitmap;
    int width, height;
} __XGDfillPatternButtonTable[] = {
    { "None", NULL, 16, 16 },
    { "Solid", NULL, 16, 16 },
    { "Vertical", verticalpat_bits, verticalpat_width, verticalpat_height},
    { "Check", check_bits, check_width, check_height },
    { "Cross", cross_bits, cross_width, cross_height },
    {"Horizontal", horizpat_bits, horizpat_width, horizpat_width},
    {"Ptleft", ptleft_bits, ptleft_width, ptleft_height},
    {"Ptright", ptright_bits, ptright_width, ptright_height}
};



/* sethilite.c */
Pixel XgdSetHighlightColor(
#ifndef _NO_PROTO
    Display * dpy,
    char *color
#endif
);

/*************/
/*  bbox.c   */
/*************/
Status XgdBoxesOverlap(
#ifndef _NO_PROTO
    XgdBox b1,
    XgdBox b2
#endif
);

Status XgdBoxContains(
#ifndef _NO_PROTO
     XgdBox b1,
     XgdBox b2
#endif
);

Status XgdPointInBox(
#ifndef _NO_PROTO
     XgdBox b,
     int    x,
     int    y
#endif
);

void XgdCreateBoxForObject(
#ifndef _NO_PROTO
XgdObject *obj
#endif
);

void XgdUpdateBoxForObject( 
#ifndef _NO_PROTO
XgdObject *obj, int t, int b, int l, int r
#endif
);

XgdBox *XgdSetBox(
#ifndef _NO_PROTO
     int t,
     int b,
     int l,
     int r
#endif
);

Status XgdLineIntersectBox(
#ifndef _NO_PROTO
     XgdBox box,
     XgdLine line
#endif			   
);

Status XgdLineContainedInBox(
#ifndef _NO_PROTO
     XgdLine l,
     XgdBox b
#endif
);

Boolean
XgdIsPointInObject(
#ifndef _NO_PROTO
XgdObject *obj, int x, int y
#endif
);

Boolean
XgdIsObjectInBox(
#ifndef _NO_PROTO
XgdObject *obj, XgdBox box
#endif
);

XgdBox *XgdGetBBoxOfObject(
#ifndef _NO_PROTO
XgdObject *obj
#endif
);

/*  object.c */

    /* Private Library Functions */
void _xgdDrawVector(
#ifndef _NO_PROTO		     
    XgdObject *obj,
    char      *name,
    char      *mapset
#endif
);

void _xgdDrawPixmapBorder(
#ifndef _NO_PROTO
     XgdObject *obj,
     GC        gc
#endif     
);

int _XgdGetBitmapPad(
#ifndef _NO_PROTO
        XVisualInfo     vinfo
#endif
);

   /* Regular library functions */

void XgdDeleteMap(
#ifndef _NO_PROTO
     XgdObject *obj,
     char      *map
#endif
);

void XgdSetLegendFontName(
#ifndef _NO_PROTO
     XgdObject *obj,
     char *name
#endif
);
void XgdSetLegendFont(
#ifndef _NO_PROTO
     XgdObject *obj,
     XFontStruct *font
#endif
);

void XgdDeleteRaster(
#ifndef _NO_PROTO
     XgdObject *obj
#endif
);

void XgdConfigureResizeHandles(
#ifndef _NO_PROTO
     XgdObject *obj
#endif     
);

void XgdSetLegendBorderColor(
#ifndef _NO_PROTO
     XgdObject *obj,
     Pixel      color
#endif
);

void XgdResizePixmap(
#ifndef _NO_PROTO
    XgdObject *obj,
    int nwidth,
    int nheight,
    int xoff,
    int yoff
#endif
);

XgdObject *XgdCreateObject(
#ifndef _NO_PROTO
     int type
#endif
);

void XgdDestroyObject(
#ifndef _NO_PROTO
     XgdObject *obj
#endif
);

GC XgdGetGCOfObject(
#ifndef _NO_PROTO
     XgdObject *obj
#endif
);

void XgdSetObjectPosition(
#ifndef _NO_PROTO
     XgdObject *obj,
     int       x,
     int       y
#endif
);

void XgdGetObjectPosition(
#ifndef _NO_PROTO
     XgdObject *obj,
     int       *x,
     int       *y
#endif
);

void XgdInitObject(
#ifndef _NO_PROTO
     XgdObject *obj,
     int       fp,
     int       lp,
     Pixel     fg,
     Pixel     bg,
     int       lw
#endif
);

void XgdSetObjectFillPattern(
#ifndef _NO_PROTO
     XgdObject *obj,
     Pixel fg,
     Pixel bg,
     int fp
#endif
);

void XgdSetObjectBackground(
#ifndef _NO_PROTO
     XgdObject *obj,
     Pixel     bg
#endif
);

void XgdSetObjectForeground(
#ifndef _NO_PROTO
     XgdObject *obj,
     Pixel     fg
#endif
);

void XgdSetObjectLinePattern(
#ifndef _NO_PROTO
     XgdObject *obj,
     int lw,
     int lp
#endif
);

void XgdConfigureObject(
#ifndef _NO_PROTO
     XgdObject *obj,
     int x,
     int y,
     int width,
     int height
#endif
);

Pixmap XgdCreateFillPixmap(
#ifndef _NO_PROTO
     XgdObject *obj, 
     Pixel     fg,
     Pixel     bg,			   
     int       fp
#endif
);

void XgdAddPointToPointList(
#ifndef _NO_PROTO
     XgdObject *obj,
     int       x,
     int       y
#endif
);

/**********/
/* draw.c */
/**********/
Boolean IsCatInList(
#ifndef _NO_PROTO
     XgdObject *obj,
     char *name
#endif
);

void XgdDrawLegend(
#ifndef _NO_PROTO
     XgdObject *obj
#endif
);

void XgdRedrawGeoframe(
#ifndef _NO_PROTO
     XgdObject *obj
#endif     
);

void XgdDrawObject(
#ifndef _NO_PROTO
     GC gc, XgdObject *obj, int fill
#endif
);
void XgdUnDrawObject(
#ifndef _NO_PROTO
     XgdObject *obj, Pixel bg, int fill
#endif
);

void XgdDrawVector(
#ifndef _NO_PROTO
     XgdObject *obj,
     char      *name,
     char      *mapset
#endif     
);

void _xgdDrawVector(
#ifndef _NO_PROTO
     XgdObject *obj,
     char      *name,
     char      *mapset
#endif     
);

void XgdDrawRaster(
#ifndef _NO_PROTO
     XgdObject *obj,
     char      *name,
     char      *mapset
#endif
);

void XgdUnDrawObject(
#ifndef _NO_PROTO
    XgdObject *obj,
    Pixel     bg,
    int       fill
#endif
);

void XgdDrawGrid(
#ifndef _NO_PROTO
    XgdObject * obj,
    double gfnorth,
    double gfsouth,
    double gfeast,
    double gfwest,
    double t,
    double b,
    double l,
    double r,
    double gridgap
#endif
);


void XgdDrawBarscale(
#ifndef _NO_PROTO
XgdObject *obj, int updtemode
#endif
);

void _XgdDrawDashedBarscale(
#ifndef _NO_PROTO
    Display *dpy, Window win, GC gc,
    int x,
    int y,
    int dlen,
    int gap,
    int num,
    int *ox,
    int *oy,
    int *ow,
    int *oh
#endif
);

void _XgdDrawTickedBarscale(
#ifndef _NO_PROTO
    Display *dpy, Window win, GC gc,
    int x,
    int y,
    int dlen,
    int gap,
    int num,
    int *ox,
    int *oy,
    int *ow,
    int *oh
#endif
);


void _XgdGetBarscaleLength(
#ifndef _NO_PROTO
    int *dlen, 
    int wwidth,
    int length
#endif
);



/**************/
/* d_spline.c */
/**************/
int _xgdInitPointArray(
#ifndef _NO_PROTO
     int init_size,
     int step_size
#endif
);

int _xgdAddPoint(
#ifndef _NO_PROTO
   int x,
   int y
#endif
);

void _xgdDrawSpline(
#ifndef _NO_PROTO
     XgdObject *obj,
     XgdSpline *spline
#endif
);

void _xgdDrawIntSpline(
#ifndef _NO_PROTO
     XgdObject *obj,
     XgdSpline *spline,
     int op
#endif
);

void _xgdDrawOpenSpline(
#ifndef _NO_PROTO
     XgdObject *obj,
     XgdSpline *spline
#endif
);

void _xgdDrawClosedSpline(
#ifndef _NO_PROTO
    XgdObject  *obj,
    XgdSpline  *spline
#endif
);

void _xgdQuadraticSpline(
#ifndef _NO_PROTO
    double a1,
    double b1,
    double a2,
    double b2,
    double a3,
    double b3,
    double a4,
    double b4
#endif
);

void _xgdBezierSpline(
#ifndef _NO_PROTO
    double a0,
    double b0,
    double a1,
    double b1,
    double a2,
    double b2,
    double a3,
    double b3,
    int    op
#endif
);

void _xgdClearStack(
#ifndef _NO_PROTO
  void	 
#endif
);

void _xgdPush(
#ifndef _NO_PROTO
    double x1,
    double y1,
    double x2,
    double y2,
    double x3,
    double y3,
    double x4,
    double y4
#endif
);

int _xgdPop(
#ifndef _NO_PROTO
    double *x1,
    double *y1,
    double *x2,
    double *y2,
    double *x3,
    double *y3,
    double *x4,
    double *y4
#endif
);

/* setvectcolor.c */
/* 
 *                          *****WARNING*******
 * XGD_NUM_VECT_COLORS *MUST* equal the number of colors in this list!! 
 *                          *****WARNING*******
 */
#define XGD_NUM_VECT_COLORS 11
static char *__XGDMonoColors[] = {
    "white", 
    "black",
};

static char *__XGDVectColors[] = {
    "white", 
    "black",
    "red",
    "orange", 
    "yellow", 
    "green",
    "blue", 
    "brown", 
    "magenta",
    "aquamarine", 
    "gray"
};

Pixel __XGDVectPixels[XGD_NUM_VECT_COLORS];

Pixel * XgdInitVectorColors(
#ifndef _NO_PROTO
Display * dpy, int *num
#endif
);
char ** XgdGetVectColorNames(
#ifndef _NO_PROTO
void
#endif
);
Pixel XgdGetVectColorPixelByName(
#ifndef _NO_PROTO
char *name
#endif
);

/* warning.c */
void XgdWarning(
#ifndef _NO_PROTO
char *s
#endif
);

/* initxgd.c */
void XgdInit( 
#ifndef _NO_PROTO
Display *dpy, Colormap cmap, XVisualInfo *vinfo
#endif
);

void _XgdBuildFixedColorLut(
#ifndef _NO_PROTO
Display *dpy, Colormap cmap
#endif
);

/* color.c */
Boolean _XgdGetFirstAvailableLutIndex(
#ifndef _NO_PROTO
int *index
#endif
);


#define XG_LUT_MODE_RO     0
#define XG_LUT_MODE_RW     1

#define XG_LUT_STATUS_AVAILABLE 0
#define XG_LUT_STATUS_IN_USE    1
#define XG_LUT_STATUS_SHARED    2
#define XG_LUT_STATUS_FIXED     3
#define XG_LUT_STATUS_NO_FREE   4

typedef struct __xg_color_lut {
    Pixel pixel;
    int mode;
    int status;
    int accesses;
    unsigned short red;
    unsigned short green;
    unsigned short blue;
} XgLut;

typedef struct _xg_fixed_lut {
    unsigned short red[256];
    unsigned short green[256];
    unsigned short blue[256];
} XgFixedLut;


#define XGD_NO_DITHER 0
#define XGD_DITHER_COLORS 1
/* lut.c */
XgLut * _XgdBuildLut(
#ifndef _NO_PROTO
Display *dpy, Colormap cmap, int entries, int *private
#endif
);

#ifdef __XGD_INIT
Pixel __XGDHighlight;
Boolean __XGDMonochrome;
Colormap __XGDColormap;
XVisualInfo *__XGDVInfo;
Boolean __XGDFixedColors;
int __XGDTotalCells;
int __XGDPrivateCells;
int __XGDPrivateCellsLeft;
XgLut *__XGDLut;
XgFixedLut __XGDFixedLut;
#else
extern Pixel __XGDHighlight;
extern Boolean __XGDMonochrome;
extern Colormap __XGDColormap;
extern XVisualInfo *__XGDVInfo;
extern int __XGDTotalCells;
extern Boolean __XGDFixedColors;
extern int __XGDPrivateCells;
extern int __XGDPrivateCellsLeft;
extern XgLut *__XGDLut;
extern XgFixedLut __XGDFixedLut;
#endif

/* resizeh.c */
void XgdDrawResizeHandles(
#ifndef _NO_PROTO
XgdObject *obj, GC gc
#endif
);

/****************/
/* crtespline.c */
/****************/

void _xgdCreateSplineobject(
#ifndef _NO_PROTO
     XgdObject  *obj,
     int         mode
#endif
);

int _xgdCreateControlList(
#ifndef _NO_PROTO
     XgdSpline *s
#endif
);

int _xgdCreateControlPoints(
#ifndef _NO_PROTO
     XgdSpline *s
#endif
);

void _xgdComputeCP(
#ifndef _NO_PROTO
     XgdPointList     *points,
     XgdControlList   *controls,
     int              path
#endif
);

void _xgdControlPoints(
#ifndef _NO_PROTO
     double           x,
     double y,
     double l1,
     double l2,
     double theta1,
     double theta2,
     double t,
     XgdControlList *cp
#endif
);

/**********/
/* plot.c */
/**********/

void XgdSetupPlot(
#ifndef _NO_PROTO
     XgdObject *obj,
     double t,
     double b,
     double l,
     double r
#endif
);

void XgdPlotWhereXY(
#ifndef _NO_PROTO
    double east,
    double north,
    int *x,
    int *y
#endif
);

void XgdPlotWhereEN(
#ifndef _NO_PROTO
     int x,
     int y,
     double *east,
     double *north
#endif
);

void XgdPlotPoint(
#ifndef _NO_PROTO
     XgdObject *obj,
     double east,
     double north
#endif
);

void XgdPlotLine(
#ifndef _NO_PROTO
     XgdObject *obj,
     double east1,
     double north1,
     double east2,
     double north2
#endif
);

int XgdPlotPolygon(
#ifndef _NO_PROTO
     XgdObject *obj,
     double *x,
     double *y,
     int    n
#endif
);

/****************/
/* dithrimage.c */
/****************/

void XgdDitherImage(
#ifndef _NO_PROTO
    XgdObject *obj,
    int       scrn,
    char *name,
    char *mapset,
    int chcat,
    int lhcat
#endif
);

/************/
/* raster,c */
/************/

void XgdRasterImage(
#ifndef _NO_PROTO
    XgdObject * obj,
    int scrn
#endif
);

/**************/
/* abwcolor.c */
/**************/

void XgdMonoColor(
#ifndef _NO_PROTO
    XgdObject *obj,
    char *name,
    char *mapset,
    Pixel *lookup_tbl
#endif
);

/*************/
/* bwimage.c */
/*************/

void XgdBWImage(
#ifndef _NO_PROTO
    XgdObject *obj,
    int scrn,
    char *name,
    char *mapset
#endif
);

/***************/
/* crteimage.c */
/***************/

void XgdCreateImage(
#ifndef _NO_PROTO
    XgdObject *obj,
    int scrn,
    char *name,
    char *mapset,
    int mode
#endif
);

int XgdNextSite (
#ifndef _NO_PROTO
        struct Cell_head *region,
        double *U_X,
        double *U_Y
#endif
);

#endif /* _XGRASS_DLIB_H */

