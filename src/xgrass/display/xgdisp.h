#ifndef __XGDISP_H
#define __XGDISP_H


#include "xgrass_lib.h"
#include "xgrass_dlib.h"
#include "bitmaps.h"
#include "Browser.h"
#include "Interact.h"
#include "Pixel.h"
#include "Region.h"

#define XGD_ORIGINAL     1
#define XGD_HIGHLIGHT    0
#define XGD_BOGUS_EVENT  6000

typedef struct _objectList {
    XgdObject *object;
    struct _objectList *next;
} ObjectListRec, *ObjectList;

#include "xgdproto.h"

typedef struct _xgrass_disp_global {
        /* display and screen info */
	Display *display;
        int screenNo;
        Screen *screenPtr;

        /* color map, visual, etc.. */
        int depth;
        Colormap cmap;
        unsigned int visualClass;
        Visual *visual;

        /* top level shell widget */
	Widget applShell;

	/* The application context */
	XtAppContext appContext;
	
        /* main drawing area widget id */
	Widget drawArea;
	
        /* popup menu widget id */
	Widget popup;

        /* readout widget ids */
	Widget xPosAreaLabel;
	Widget xPosArea;
	Widget yPosAreaLabel;
	Widget yPosArea;

        /* units for rulers and readout */
	int units;

        /* page dimensions in inches */
        double pageWidth, pageHeight;

        /* ruler widget dimensions in pixels */
        int hWidth, vHeight;

        /* last processed slider positions */
        int lastX, lastY;

        /* ruler pixmaps */
	Pixmap hRuler, vRuler;

        /* ruler font struct */
        XFontStruct *rulerFontStruct;

        /* scrollbar widget ids */
        Widget hsb, vsb;

        /* ruler widget and window ids */
        Widget hRulerWidget, vRulerWidget;
        Window hRulerWindow, vRulerWindow;

        /* ruler graphics context */
        GC rulerGC;

        /* widget id of main window */
	Widget mainWindow;

        /* widget id of scrolled window */
	Widget scrollWindow;

        /* widget id of clip window */
	Widget clipWindow;

        /* widget id of mode display area */
	Widget messageArea;

        /* widget id of tool box */
        Widget toolbox;


	/* widget id of grid attribute box */
	Widget gridbox;
	Widget gridtoggle;

        /* grid attributes */
        XgdGrid	gridattr;

	/* widget id of barscale attribute box */
	Widget barbox;

	/* barscale attributes */
	XgdBarscale barattr;

	/* widget id of set site standard
	attributes */
	Widget stdsitepl;

	/* standard site attributes */
	XgdStdSite  stdsiteattr;
	int setSite;

	/* to prevent redraw of site pixmap */
	int oldx, oldy, oldwidth, oldheight;
	char *sitefile;	

	/* changed vector name and mapset */
	char *vname;
	char *vmapset;

	/* changed site name and mapset */
	char *sname;
	char *smapset;

	/* widget id of query raster panel */
	Widget qrastpanel;

        /* current mode */
        unsigned int mode;
        int drawType;
 
        /* currently active object (during creation) */
        XgdObject *currentObject;
        
        /* currently selected object (during creation) */
        ObjectList selectedObjects;

        /* list of objects to redraw */
        ObjectList  redrawList;
        ObjectList  excludeList;
        
        /* master object list */
        ObjectList objectList;
        
        /* xor gc */
        GC xorGC;

        /* fill pattern, line pattern, line width, fg, bg */
        int fillPattern;
        int linePattern;
        int lineWidth;
        Pixel foreground;
        Pixel background;

        /* Font stuff */
        XFontStruct *fontStruct;
        char *fontName;

        /* highlight color */
        Pixel highlight;

        /* vector  colors */
        Pixel *vectColors;
        int numVectColors;
} XG_Global;

#ifdef MAIN
XG_Global Global;
char *yyfilename;
#else
extern XG_Global Global;
extern char *yyfilename;
#endif /* MAIN */

/* band.c */
#define XGD_BAND_INIT 0
#define XGD_BAND      1
#define XGD_BAND_END  2

/* coloropt.c */

/* font.c */
#define XGD_DEFAULT -1

/* handler.c */

/* layout.c */

/* menubar.c */

/* menucb.c */

/* mode.c */

#define XGD_MODE_SELECT		    0
#define XGD_MODE_HIGHLIGHT	    1
#define XGD_MODE_MODIFY_COLORS	    2
#define XGD_MODE_QUERY_RASTER	    3
#define XGD_MODE_QUERY_VECTOR	    4
#define XGD_MODE_ZOOM		    5
#define XGD_MODE_DRAW		    6
#define XGD_MODE_MOVE		    7
#define XGD_MODE_RESIZE		    8
#define XGD_MODE_POLYRESHAPE        9
#define XGD_MODE_HIGHLIGHT_SELECT  10

static char *ModeStringTable[] = {
"Select",
"Highlight",
"Modify Colors",
"Query Raster",
"Query Vector",
"Zoom",
"Draw",
"Move",
"Resize",
"Poly Reshape",
NULL
};


/* objbutton.c */

static struct _objectButtonTable {
    char *label;
    BITMAPS_H_TYPE *bitmap;
    int width, height;
    int type;
} objectButtonTable[] = {
{"Square",		square_bits,	square_width,	square_height,
     XGD_SQUARE},
{"Rectangle",		rect_bits,	rect_width,	rect_height,
     XGD_RECTANGLE},
{"Circle",		circle_bits,	circle_width,	circle_height,
     XGD_CIRCLE},
{"Ellipse",		arc_bits,	arc_width,	arc_height,
     XGD_ELLIPSE},
{"Polyline",		line_bits,	line_width,	line_height,
    XGD_POLYLINE},
{"Polygon",		polygon_bits,	polygon_width,	polygon_height,
    XGD_POLYGON},
{"App. Closed Spline",	acspl_bits,	acspl_width,	acspl_height,
    XGD_CLOSED_APPROX_SPLINE},
{"App. Open Spline",	aospl_bits,	aospl_width,	aospl_height,
    XGD_OPEN_APPROX_SPLINE},
{"Closed Spline",	cspl_bits,	cspl_width,	cspl_height,
    XGD_CLOSED_INTERP_SPLINE},
{"Open Spline",		ospl_bits,	ospl_width,	ospl_height,
    XGD_OPEN_INTERP_SPLINE},
{"Label",		label_bits,	label_width,	label_height,
    XGD_LABEL},
{"Grid",		grid_bits,	grid_width,	grid_height,
    XGD_GRID},
{"Legend",		legend_bits,	legend_width,	legend_height,
    XGD_LEGEND},
{"Barscale",		barscale_bits,	barscale_width,	barscale_height,
    XGD_BARSCALE},
/*{"Histogram",		histogram_bits, histogram_width, histogram_height,
    XGD_HISTOGRAM},*/
};

/* ruler.c */

/* scroll.c */

static XtCallbackRec vsbCBList[] = {
    {(XtCallbackProc) VerticalSliderMoved, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};

static XtCallbackRec hsbCBList[] = {
    {(XtCallbackProc) HorizontalSliderMoved, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};

/* toolbox.c */

/* units.c */
#define XGD_UNITS_PIXELS 0
#define XGD_UNITS_INCHES 1
#define XGD_UNITS_MILLI  2





/* bartextgadg.c */
#define XGD_BARLENGTH	0 
#define XGD_BARINTERVAL 1 
#define XGD_BARTHICK	2 

Widget barlengthw;
Widget barintervalw;
Widget barthickw;
Widget bardashedw;
Widget bartickedw;
Widget barkmw;
Widget barmw;
Widget barmiw;
Widget barftw;
Widget barfgw;
Widget bartcw;


/* drawbarscale.c */

#define DASHED			0
#define TICKED			1 

/*gridgadg.c */
#define XGD_GRIDGAP		0
#define XGD_GRIDSPACING 	1
#define XGD_GRIDLINEWIDTH	2

Widget  gridonw;
Widget  gridlabelonw;
Widget  gridgapw;
Widget  gridspacew;
Widget  gridlww;

/* qrastpl.c */
Widget  qrastnamew;
Widget  qrastmapsetw;
Widget  qrastnorthw;
Widget  qrasteastw;
Widget  qrastcatnumw;
Widget  qrastcatnamew;

Widget stdsitecrossw;
Widget stdsitediamondw;
Widget stdsiterectw;
Widget stdsiteplusw;
Widget stdsitesizew;
Widget stdsitelww;

/* dellist.c */
#define VECTOR 0
#define SITE   1

/* drawoutsitefh.c */
struct sitefh
{
	int count;
	char *other;
};

struct sitefh sitefh;

#endif /* __XGDISP_H */
