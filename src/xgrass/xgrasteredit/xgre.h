
/*
 * FILE: xgre.h
 * 
 * PROGRAMMER: David M. Johnson
 *
 */

#ifndef __XGDISP_H
#define __XGDISP_H

#include "xgrass_lib.h"
#include "xgrass_dlib.h"
#include "Browser.h"
#include "Interact.h"
#include "Pixel.h"
#include "Region.h"
#include "xgreproto.h"
#include "segment/segment.h"

#define XGRE_BROWS   7   /* max. no. of brush rows */
#define XGRE_BCOLS   7   /* max. no. of brush columns */

/************/
/* typedefs *
/************/

typedef struct _brush_cell
   {
   int input;    /* see XGRE_INP defines under brushedit.c below */ 
   int op;       /* see XGRE_OP defines under brushedit.c below */
   int value;
   } BrushCell;

/***************/
/* XGRE_Global */
/***************/

typedef struct _xgre_global 
   {
                              /* X, XT AND XGRASS STUFF */
   XtAppContext appContext;   /* The application context */
   Display  *display;         /* display and screen info */
   int       screenNo;        /* screen number */
   Screen   *screenPtr;       /* pointer to screen */
   Colormap  cmap;            /* X colormap */
   int       depth;           /* X colormap depth */ 
   Visual   *visual;          /* X visual */
   unsigned  int visualClass; /* X visual class */
   Pixel     highlight;       /* highlight color */
   Pixel    *vectColors;      /* vector  colors */
   int       numVectColors;   /* number of vector colors */
   XgdObject *obj;            /* XGRASS Display Lib. object */ 

                              /* MAIN WINDOW WIDGETS */
   Widget    applShell;       /* top level shell widget */
   Widget    mainWindow;      /* widget id of main window */
   Widget    scrollWindow;    /* widget id of scrolled window */
   Widget    colText;         /* column text field */
   Widget    rowText;         /* row text field */
   Widget    eastText;        /* easting text field */
   Widget    northText;       /* northing text label */
   Widget    catText;         /* category text label */
   Widget    attText;         /* attribute text field */
   Widget    modeText;        /* mode message text field */
   Widget    brushText;       /* brush message text field */
   Widget    btnLock;         /* Edit-Lock button-bar button */
   Widget    mbtnLock;        /* Edit-Lock menu button */
   Widget    mbtnHoriz;       /* Edit-Contrain-Horizontal menu button */
   Widget    mbtnVerti;       /* Edit-Contrain-Vertical menu button */
   Widget    mbtnBox;         /* Edit-Box-Region menu button */
   Widget    mbtnPoly;        /* Edit-Polygon-Region menu button */

                              /* RASTER AND SEGMENT FILE MAP */
   char      rname[200];      /* raster map file name */
   char      rmapset[200];    /* raster map file mapset */
   char      segname[200];    /* segment file name */
   int       segfd;           /* segment file descriptor */
   SEGMENT   seg;             /* segment structure pointer */
   struct    Cell_head seghd; /* segment file header */
   struct    Categories cats; /* category structure */
   struct    Colors colors;   /* color-table structure */
   struct    Range range;     /* range structure */
   int       segtype;         /* see defines under ras2seg.c below */

                              /* MAIN IMAGE */
   Widget    imageArea;       /* main image drawing area widget id */
   XImage   *image;           /* image of the entire raster map */
   GC        imageGC;         /* image graphics context */ 
   int       imageHeight;     /* image height */
   int       imageWidth;      /* image width */
   int       modX1, modY1;    /* upper-left corner of modified area */
   int       modX2, modY2;    /* bottom-right corner of modified area */

                              /* ZOOM IMAGE */
   Widget    zoomArea;        /* zoom drawing area widget id */
   XImage   *zoomImage;       /* image of the zoom area */
   float     zoomMag;         /* magnification factor */
   int       zoomRowOff;      /* raster row offset */ 
   int       zoomColOff;      /* raster col offset */
   int       zoomX,zoomY;     /* raster coords of center of zoom */
   int       zoomWidth;       /* pixel width of zoomArea */
   int       zoomHeight;      /* pixel height of zoomArea */

                              /* BRUSH */
   BrushCell brush[XGRE_BROWS][XGRE_BCOLS];
   char      bname[200];      /* brush file name */ 
   char      bmapset[200];    /* brush file mapset */
   CELL      brushCat;        /* brush category value */
   int       brushRows;       /* no. of brush rows */
   int       brushCols;       /* no. of brush cols */
   int       brushHotRow;     /* brush "hot-spot" row */ 
   int       brushHotCol;     /* brush "hot-spot" col */
   int       brushLoaded;     /* flag: true if brush is loaded */

                              /* MODE FLAGS */
   int       mode;            /* see defines under main.c below */
   int       zoomRunning;     /* flag: zoom image loaded? */
   int       zoomLoaded;      /* flag: zoom image loaded? */
   int       indexLoaded;     /* flag: index window loaded? */ 

                              /* WINDOW AND DIALOG WIDGETS */
   Widget    fileOpenD;
   Widget    fileResumeD;
   Widget    fileSaveD;
   Widget    viewZoomW;
   Widget    viewIndexW;
   Widget    brushCatD;
   Widget    brushColorD;
   Widget    brushEditD;
   Widget    brushCreateD;
   Widget    brushLoadD;
   Widget    brushSaveD;
   Widget    editCatsD;

                              /* WINDOW AND DIALOG WIDGET FLAGS */
   int       FfileOpenD;
   int       FfileResumeD;
   int       FfileSaveD;
   int       FbrushCatD;
   int       FbrushColorD;
   int       FbrushEditD;
   int       FbrushCreateD;
   int       FbrushLoadD;
   int       FbrushSaveD;
   int       FeditCatsD;
   int       FeditRangeD;
   int       FviewZoomW;
   int       FviewIndexW;

} XGRE_Global;

#ifdef MAIN
XGRE_Global Global;
#else
extern XGRE_Global Global;
#endif /* MAIN */

/**********/
/* main.c */
/**********/

#define XGRE_UNLOADED        0
#define XGRE_LOCKED          1
#define XGRE_NORMAL          2
#define XGRE_BOX_EDIT        3
#define XGRE_POLY_EDIT       4
#define XGRE_VERTI_DRAG      5
#define XGRE_HORIZ_DRAG      6

/***************/
/* brushedit.c */
/***************/

/* brush operation input options */
 
#define XGRE_INPCOUNT   3   /* number of input options */
#define XGRE_INPCAT     0   /* input = brush category */
#define XGRE_INPREAD    1   /* input = segment cell value  */
#define XGRE_INPVALUE   2   /* input = brush cell value */

/* brush operation options */

#define XGRE_OPCOUNT    6   /* number of operation options */
#define XGRE_OPCOPY     0   /* segment cell = input */
#define XGRE_OPNULL     1   /* do nothing */
#define XGRE_OPERASE    2   /* segment cell = 0 */
#define XGRE_OPADD      3   /* segment cell += input */
#define XGRE_OPMULT     4   /* segment cell *= input */
#define XGRE_OPCALC     5   /* mapcalc style thing yet to be */

#define MAX_CELL	2000000000
#define MIN_CELL       -2000000000

/*************/
/* ras2seg.c */
/*************/

/* database element names of segment file components */

#define XGRE_SEG          "seg"
#define XGRE_SEGHD        "seghd"
#define XGRE_SEGCATS      "segcats"
#define XGRE_SEGCOLR      "segcolr"
#define XGRE_SEGRANGE     "segrange"

/* segmentation parameters */

#define XGRE_RASTER_REG    1   /* segment using raster's region */ 
#define XGRE_CURRENT_REG   2   /* segment using current region */
#define XGRE_SEG_EDGE     64   /* no. of cells along segment edge */
#define XGRE_MEM_SEGS      4   /* no. segments in memory */

#endif /* __XGDISP_H */

