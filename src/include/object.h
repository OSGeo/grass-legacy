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
  int size;
  int width;
  Pixel color;
} XgdStdSite;

typedef struct _XgdPixSite {
  int count;
  int xhotspot;
  int yhotspot;
  char	*pixmapfile;
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
  char   *sname;
  char   *smapset;
  int    type;
  union {
    XgdStdSite        def;
    XgdPixSite	      pixdef;
    struct _XgdObject *obj;
  } Site;
  struct _XgdSiteInfo *next;
} XgdSiteInfo;

typedef struct _XgdSite {
  XgdSiteInfo *site;
  XgdSiteInfo *Tail;
} XgdSite;



typedef struct _XgdVectInfo {
  char *vname;
  char *vmapset;
  Pixel color;
  int   linewidth;
  int   linepattern;
  struct _XgdVectInfo *next;
} XgdVectInfo;

typedef struct _XgdVector {
  XgdVectInfo *vect;
  XgdVectInfo *Tail;
} XgdVector;

typedef struct _XgdGrid {
  int      labelon;
  double   gridgap;
  int      spacing;
  Pixel    color;
  int      linewidth;
  int      linepattern;
  Font     fid;
  char   *fontname;
  Pixel    textcolor;
  GC       gc;
  int	   xoff;
  int 	   yoff;
} XgdGrid;

typedef struct _XgdLabel {
  char       **lblstr;
  int          numlines;
  XFontStruct *font;
  char   *fontname;
} XgdLabel;


#define XGD_DISPLAY_CAT_NAMES     1
#define XGD_DISPLAY_CAT_NUMS      2
#define XGD_DISPLAY_CAT_NONE      3

typedef struct _XgdLegend {
  struct _XgdObject  *geoFrame;
  XFontStruct        *font;
  char   *fontname;
  Pixel               brdrcolor;
  int                 numCols;
  int                 toDisplay;
  Boolean             displaySelected;
  int                 borderWidth;
  struct Categories   cats;
  Boolean             fit;
  char              **catsToList;
  int                *catNums;
} XgdLegend;

#define XGD_DITHERED_IMAGE 1
#define XGD_BORROWED_IMAGE 2
#define XGD_STANDARD_IMAGE 3

typedef struct _XgdGeoFrame {
  struct Cell_head   region;
  char              *rname;
  char              *rmapset;
  int                colormode;
  XImage            *image;
  Pixmap             pixmap;
  Pixel             *lookup_tbl;
  Pixel             *borrowed;
  struct Colors      colors;
  Boolean            colorsExist;
  Boolean            fixedLut;
  Pixel             *fixed;
  int                fixedEntries;
  int                numborrowed;
  int                numcols;
  XgdVector          vects;
  int                numsites;
  XgdSite            sites;
  int                gridOn;
  XgdGrid            grid;
  int                numbarscales;
  struct _XgdObject **barscales;
  struct _XgdObject *legend;
} XgdGeoFrame;

typedef struct _XgdPointList {
  int    x;
  int    y;
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
  XgdPointList     *points;
  XgdPointList     *ptail;
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
  int x, y;
  int width, height;
  int lw, lp, fp;
  Pixel fg, bg;
  
/* junk that everybody needs */
  Display           *display;
  Window            window;
  GC                objgc;
  XgdBox            *bbox;
  XgdBox            handles[8];
  struct _XgdObject *compound;

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
