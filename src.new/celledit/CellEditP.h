#include <X11/copyright.h>

#ifndef _CellEditP_h
#define _CellEditP_h

/* include superclass private header file This should get me Core too*/
#include <X11/Xaw/SimpleP.h>
#include "CellEdit.h"
#include "lib/misc.h"
#include "lib/grass.h"
#include "lib/Xutils.h"
#include <math.h>

/* We get Grass's libgis with this */
#include "/home/grass3/src/libes/gis.h"
#include "/home/grass3/src/libes/segment.h"

/* define unique representation types not found in <X11/StringDefs.h> */

#define XtRCellFile "Cellfile"
#define XtRMapset "Mapset"
#define XtRLocation "Location"
#define XtRMouseCallback "MouseCallback"

/* private macros */
#define MIN(a,b) 	(((a) < (b)) ? (a) : (b))
#define MAX(a,b) 	(((a) > (b)) ? (a) : (b))
#define streq(a,b) 	(strcmp( (a), (b) ) == 0)

/* private constants */
#define MAX_WIDTH 	400	/* default start up width */
#define MAX_HEIGHT 	400	/* default start up height */
#define SEG_SIZE 	 64 	/* Segment width and height dimension */
#define NUM_SEGS   	  4 	/* Keep 4 segments in RAM */

/* this struct holds the current state of the editor brush */

typedef struct {
    /* resources */
    String cellFileName;
    String mapset;
    String location;
    Boolean resize;
    XtCallbackList callbacks;
    /* private state */
    Pixmap		image_backup;	/* redraw backup */
    GC			gc;		/* our gc */
    unsigned long	*pixelList;	/* a list of pixels in color table */
    int			numPixels;	/* the number of pixels */
    CELL 		*dataList;	/* a list of all data 	     */
    int			numData;	/* the number of data values */
    int			rows;		/* the number of rows in the WIND */
    int			cols;		/* the number of cols in the WIND */
    String		cellFilePath;	/* path to cell files */
    struct Cell_head    user_window;	/* user's default window */
    struct Colors	colors;		/* Grass color table */
    int			cell_res;	/* cell resolution (zoom factor) */
    String		segFileName;	/* Segment File Name */
    int			segFileDesc;	/* Segment File Descriptor */
    SEGMENT		segment;	/* the cell segment */
    int			min_color;	/* minimum value of color cat */
    Boolean		modified;	/* Has the user made an edit? */
    String		saveFileName;	/* save file name */
    Widget		propShell;	/* property sheet shell */
    struct Categories	cats;		/* categories */
    int 		pairsCnt;	/* where we are in the pairs loop */
} CellEditPart;

typedef struct _CellEditClassPart
    {
    int			empty;		/* The number of cols in the WIND */
    } CellEditClassPart;

typedef struct _CellEditClassRec {
    CoreClassPart	core_class;
    SimpleClassPart	simple_class;
    CellEditClassPart	celledit_class;
} CellEditClassRec;

extern CellEditClassRec cellEditClassRec;


typedef struct _CellEditRec {
    CorePart		core;
    SimplePart		simple;
    CellEditPart	celledit;
} CellEditRec;

#endif /* _CellEditP_h */
