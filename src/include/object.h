#ifndef _XGD_OBJECT_H
#define _XGD_OBJECT_H

#include "gis.h"
#include "bbox.h"

#define XGD_SQUARE                   300
#define XGD_RECTANGLE                301
#define XGD_CIRCLE                   302
#define XGD_ELLIPSE                  303
#define XGD_POLYLINE                 304
#define XGD_POLYGON                  305
#define XGD_OPEN_INTERP_SPLINE       306
#define XGD_CLOSED_INTERP_SPLINE     307
#define XGD_OPEN_APPROX_SPLINE       308
#define XGD_CLOSED_APPROX_SPLINE     309
#define XGD_GEOFRAME                 310
#define XGD_LABEL                    311
#define XGD_LEGEND                   312
#define XGD_GRID                     313
#define XGD_BARSCALE                 314
#define XGD_HISTOGRAM                315
#define XGD_COMPOUND_XGD_OBJECT      316
#define XGD_SITE_STANDARD            317
#define XGD_SITE_PIXMAP              318
#define XGD_SITE_FREEHAND            319
#define XGD_LINE                     320
#define XGD_FAKE_POLYLINE            321
#define XGD_VECTOR                   322
#define XGD_SITE                     323

typedef struct _XgdRect{
  int x, y;
  int width, height;
} XgdRect;

typedef struct _XgdStdSite {
  int icontype;
/* length of the icon */
  int size;
/* line width */  
  int width;
  Pixel color;
} XgdStdSite;

typedef struct _XgdPixSite {
/* How many windows are there? */
  int count;
  int xhotspot;
  int yhotspot;
/* File that describes the pixmap */
  char	*pixmapfile;
/* Array of windows */
  Window *pwin;
} XgdPixSite;

typedef struct _XgdBarscale {
  float   length;
  int     intervals;
  int     linewidth;
  Pixel   color;
  int     style;
  int     unit;
  Font    fid;
  char   *fontname;
  Pixel   textcolor;
  int     winwidth;
  struct _XgdObject *gfobj;
} XgdBarscale;

typedef struct _XgdSiteInfo {
/* site name and mapset */
  char   *sname;
  char   *smapset;

/* type of site ... Pixmap or standard */  
  int    type;
  union {
    XgdStdSite        def;
    XgdPixSite	      pixdef;
  } Site;

/* next in the list */  
  struct _XgdSiteInfo *next;
} XgdSiteInfo;

typedef struct _XgdSite {
/* Pointers to the head and tail of the sites list */
  XgdSiteInfo *site;
  XgdSiteInfo *Tail;
} XgdSite;

typedef struct _XgdVectInfo {
/* Vector map name and mapset */
  char *vname;
  char *vmapset;

/* Map color */  
  Pixel color;

/* line width and pattern */  
  int   linewidth;
  int   linepattern;

/* next map in the list */
  struct _XgdVectInfo *next;
} XgdVectInfo;

typedef struct _XgdVector {
/* head and tail pointers to the list of vector maps */
  XgdVectInfo *vect;
  XgdVectInfo *Tail;
} XgdVector;

typedef struct _XgdGrid {
/* Is the label displayed? */
  int      labelon;

/* The distance between grid bars */  
  double   gridgap;

/* How many grid lines there are between labels */  
  int      spacing;

/* COlor for the grid*/  
  Pixel    color;

/* Gird linewidth and line pattern */  
  int      linewidth;
  int      linepattern;

/* Font id and name for the grid labels */  
  Font     fid;
  char   *fontname;

/* Label color */  
  Pixel    textcolor;

  GC       gc;

/* Offsets due to the label being drawn */
  int	   xoff;
  int 	   yoff;
} XgdGrid;

typedef struct _XgdLabel {
/* pointer to the string to be drawn */
  char       **lblstr;

/* number of lines in the string */
  int          numlines;

/* FOnt and font name for the string */  
  XFontStruct *font;
  char   *fontname;
} XgdLabel;


#define XGD_DISPLAY_CAT_NAMES     1
#define XGD_DISPLAY_CAT_NUMS      2
#define XGD_DISPLAY_CAT_NONE      3

typedef struct _XgdLegend {
/* Geoframe the the legend is associated with */  
  struct _XgdObject  *geoFrame;

/* The font for the legend's text and the name of that font */
  XFontStruct        *font;
  char               *fontname;

/* shaded box border color */  
  Pixel               brdrcolor;

/* The number of columns */  
  int                 numCols;

/* Category name, cat number or nothing */  
  int                 toDisplay;

/* Display the selected cats? */  
  Boolean             displaySelected;

/* Width of the shaded box border */  
  int                 borderWidth;

/* the categories associted with the geoframes raster map */  
  struct Categories   cats;

/* list of slected categories */  
  char              **catsToList;

/* Numbers of selected categories */  
  int                *catNums;
} XgdLegend;

#define XGD_DITHERED_IMAGE 1
#define XGD_BORROWED_IMAGE 2
#define XGD_STANDARD_IMAGE 3

typedef struct _XgdGeoFrame {
/* region associated with the geoframe */
  struct Cell_head   region;
  
/* the raster map name and mapset*/
  char              *rname;
  char              *rmapset;

/* Is the map dithered, normal? */  
  int                colormode;

/* The XImage built when drawing the raster map */  
  XImage            *image;

/* The pixmap that is actually drawn into the swapped onto the window */
  Pixmap             pixmap;

/* The color look up table */  
  Pixel             *lookup_tbl;

/* Colors that are borrowed ... */  
  Pixel             *borrowed;

/* The GRASS color structure for the raster map */  
  struct Colors      colors;
  
/* Do the colors for the GeoFrame exist? */
  Boolean            colorsExist;
  
/* Is the lut a fixed one? */
  Boolean            fixedLut;

/* The fixed lut entries and the number of entries */  
  Pixel             *fixed;
  int                fixedEntries;
  
/* How many color cells have been borrowed */
  int                numborrowed;

/* Number of colors in the lut */  
  int                numcols;

/* Vector maps associated with a geoframe */  
  XgdVector          vects;

/* The number of sites */
  int                numsites;

/* Sites associated with the geoframe */  
  XgdSite            sites;

/* Is there a grid drawn on the geoframe? */  
  int                gridOn;

/* The grid information */  
  XgdGrid            grid;

/* number of barscales for the geoframe */
  int                numbarscales;

/* pointer to the arry of barscale structures */
  struct _XgdObject **barscales;

/* Pointer to the legend */  
  struct _XgdObject *legend;
} XgdGeoFrame;

typedef struct _XgdPointList {
  int    x;
  int    y;

/* box used when moving the point */  
  XgdBox *box;
  struct _XgdPointList *next;
} XgdPointList;

typedef struct _XgdControlList {
  double lx;
  double ly;
  double rx;
  double ry;
  struct _XgdControlList *next;
} XgdControlList;

typedef struct _XgdSpline {
/* Head and tail of the list of points */
  XgdPointList   *points;
  XgdPointList   *ptail;

/* Head and tail of the list of control points */  
  XgdControlList *controls;
  XgdControlList *ctail;
} XgdSpline;

typedef struct _XgdPolyline {
  XgdPointList *Tail;
  XgdPointList *pts;
} XgdPolyline;

#define XGD_OUTSIDE_HANDLE    -1
#define XGD_UPPER_LEFT        0
#define XGD_TOP               1
#define XGD_UPPER_RIGHT       2
#define XGD_RIGHT             3
#define XGD_BOTTOM_RIGHT      4
#define XGD_BOTTOM            5
#define XGD_BOTTOM_LEFT       6
#define XGD_LEFT              7

typedef struct _XgdObject {
/* object type and ID */
  int type;
  int oid;

/* x and y coordinates, width & height */
  int x, y;
  int width, height;

/* line width, line pattern and fill pattern */  
  int lw, lp, fp;

/* Foreground and Background colors */  
  Pixel fg, bg;
  
/* Information common to all objects */

/* Pointer to the display */
  Display *display;

/* The Window to draw in */  
  Window window;

/* The Graphics Context used for drawing the object */  
  GC objgc;

/* The Bounding box of the object */  
  XgdBox *bbox;

/* The resize handles for the object */
  XgdBox handles[8];

/* compound objects ... not currently used */  
  struct _XgdObject *compound;

/* Union for all the different types of objects that need object specific
   information */
  union {
    XgdLine     Line;
    XgdGeoFrame GeoFrame;
    XgdSpline   Spline;
    XgdPolyline Polyline;
    XgdBarscale Barscale;
    XgdLabel    Label;
    XgdLegend   Legend;
  } Obj;
  
} XgdObject;


#endif /* _XGD_OBJECT_H */
