/**********************************************************************
   grammar.y    - the xgrass display parser grammar (yacc)
 *********************************************************************/
%{

#define __DEBUG 0
#include "xgdisp.h"
extern double atof();

typedef struct _xgdisp_parser_globs {
    Boolean docSizeSet;
    char *docSize;
    Boolean pageWidthSet;
    double pageWidth;
    Boolean pageHeightSet;
    double pageHeight;
    Boolean unitsSet;
    char *units;
} XgdDisplayParserGlobals;

XgdDisplayParserGlobals parserGlobals;

XgdObject *curObject;
XgdObject templateObject;
XgdObject *geoFrameObject;
Boolean geoFrame = False;
Boolean specificsSet = False;
int gridLinePattern;

struct _vect_template {
    Pixel fg;
    char *name;
    char *mapset;
    int lp;
    int lw;
} vectTempl;

struct _site_template {
    char *name;
    char *mapset;
    int type;
    char *pixmapfile;
    int icontype;
    int size;
    int linewidth;
    Pixel color;
} siteTempl;

#define GEOFRAME_OBJECT_TYPE(x) ((x)->type == XGD_BARSCALE || \
                                 (x)->type == XGD_LEGEND || \
                                 (x)->type == XGD_GRID )

#define TYPE_WITH_SPECIFICS(x) ((x)->type == XGD_POLYLINE || \
                                (x)->type == XGD_POLYGON || \
                                (x)->type == XGD_GEOFRAME || \
                                (x)->type == XGD_LABEL || \
                                (x)->type == XGD_CLOSED_APPROX_SPLINE || \
                                (x)->type == XGD_OPEN_APPROX_SPLINE || \
                                (x)->type == XGD_CLOSED_INTERP_SPLINE || \
                                (x)->type == XGD_OPEN_INTERP_SPLINE )

%}

%union {
 char *cval;
 int ival;
};

%start PageDescription

%token DOCUMENTSIZE
%token PAGEWIDTH
%token PAGEHEIGHT
%token UNITS
%token NIL
%token OBJECT
%token GEOFRAME
%token REGION
%token RASTER_MAP
%token VECTOR_MAP
%token SITE_MAP
%token STANDARDSITE
%token PIXMAPSITE
%token FREEHANDSITE
%token GRID
%token BARSCALE
%token LEGEND
%token SQUARE
%token RECTANGLE
%token CIRCLE
%token ELLIPSE
%token POLYLINE
%token POLYGON
%token SPLINE
%token OPEN_INTERP_SPLINE
%token CLOSED_INTERP_SPLINE
%token OPEN_APPROX_SPLINE
%token CLOSED_APPROX_SPLINE
%token LABEL

%token  String

%type <cval> String 
%type <ival> ObjectType
%%

PageDescription
    : {
          bzero((char *)&parserGlobals, sizeof(XgdDisplayParserGlobals));
      }
      OptionalGlobals 
      {
          if ( parserGlobals.unitsSet ) {
              /* XXX */;
          }
          if ( parserGlobals.docSizeSet ) {
              if ( parserGlobals.pageWidthSet ) {
          /* XXX */;
              }
              if ( parserGlobals.pageHeightSet ) {
          /* XXX */;
              }
          }
      }
      ObjectList
    ;

OptionalGlobals
    : /* none they are optional */
    | Globals
    ;

Globals
    : Globals GlobalElement
    | GlobalElement
    ;

GlobalElement
    : DOCUMENTSIZE String /* sets the documentsize */
      {
          parserGlobals.docSizeSet = True;
          parserGlobals.docSize = malloc(strlen($2) + 1);
          strcpy(parserGlobals.docSize, $2);
          if (__DEBUG ) fprintf(stderr, " Documentsize -> %s\n", $2);
      }
    | PAGEWIDTH String /* sets the page width, String is an integer or real */
      {
          double pageWidth;
          char *copy = malloc(strlen($2) + 1);
          char *ptr = NULL;

          if ( (pageWidth = (double)strtod($2, &ptr, 10)) == 0 ) {
              if ( ptr != NULL && !strcmp($2, ptr) ) {
                  yyerror("Illegal type: PAGEWIDTH must be an integer/real");
                  return;
              }
          }
          strcpy(copy, $2);
          if ( G_rindex(copy,'.') != NULL ) {
              if (__DEBUG ) fprintf(stderr, " PageWidth -> %lf\n", pageWidth);
          } else {
              if (__DEBUG ) fprintf(stderr, " PageWidth -> %d\n", (int)pageWidth);
          }
          parserGlobals.pageWidthSet = True;
          parserGlobals.pageWidth = pageWidth;
          free(copy);
      }
    | PAGEHEIGHT String /* sets the page height, String is an integer or real */
      {
          double pageHeight;
          char *copy = malloc(strlen($2) + 1);
          char *ptr = NULL;

          if ( (pageHeight = (double)strtod($2, &ptr, 10)) == 0 ) {
              if ( ptr != NULL && !strcmp($2, ptr) ) {
		  yyerror("Illegal type: PAGEHEIGHT must be an integer/real");
		  return;
              }
          }
          strcpy(copy, $2);
          if ( G_rindex(copy,'.') != NULL ) {
              if (__DEBUG ) fprintf(stderr, " PageHeight -> %lf\n", pageHeight);
          } else {
              if (__DEBUG ) fprintf(stderr, " PageHeight -> %d\n", (int)pageHeight);
          }
          parserGlobals.pageHeightSet = True;
          parserGlobals.pageHeight = pageHeight;
          free(copy);
      }
    | UNITS String /* 
                    * sets the units on the ruler and the readout, 
                    * String must be 
                    * "mm", "millimeters", "in", "inches", "pixel", or "pixels"
                    */
      {
          char *units;
          char errorbuf[1024];

          if ( !strcmp($2,"mm") || !strcmp($2,"millimeters") ) {
              if (__DEBUG ) fprintf(stderr,"Units -> millimeters\n");
          } else if ( !strcmp($2,"in") || !strcmp($2,"inches") ) {
              if (__DEBUG ) fprintf(stderr,"Units -> inches\n");
          } else if ( !strcmp($2,"pixel") || !strcmp($2,"pixels") ) {
              if (__DEBUG ) fprintf(stderr,"Units -> pixels\n");
          } else {
              yywarning("Illegal value: UNITS must be one of:");
              if (__DEBUG ) fprintf(stderr,"\t\"mm\", \"millimeters\"\n");
              if (__DEBUG ) fprintf(stderr,"\t\"in\", \"inches\"\n");
              if (__DEBUG ) fprintf(stderr,"\t\"pixel\", \"pixels\"\n");
              exit(1);
          }
          parserGlobals.unitsSet = True;
          parserGlobals.units = malloc(strlen($2) + 1);
          strcpy(parserGlobals.units, $2);
      }
    ;

ObjectList
    : ObjectList ObjectDescription

    | ObjectDescription
    ;

ObjectDescription
    : '<' OBJECT 
          {
              if (__DEBUG ) fprintf(stderr, "PARSING OBJECT DESCRIPTION\n");
              specificsSet = False;
          }
      String  /* String is the object id of the object, not currently used.. */
          {
              if (__DEBUG ) fprintf(stderr,"\toid = %s\n", $4);
          }
      ObjectType /* String must be a valid object type (see ObjectType below) */
          {
              curObject = XgdCreateObject($6);
          }
      String  /* String represents the integer x position of the object */
          {
              int x;
              char *ptr = NULL;

              if ( (x = (int)strtol($8, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($8, ptr) ) {
                      yyerror("Illegal type: x position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tx = %d\n", x);
              templateObject.x = x;
          }
      String   /* String represents the integer y position of the object */
          {
              int y;
              char *ptr;

              if ( (y = (int)strtol($10, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($10, ptr) ) {
                      yyerror("Illegal type: y position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\ty = %d\n", y);
              templateObject.y = y;
          }
      String   /* String represents the integer width of the object */
          {
              int width;
              char *ptr;

              if ( (width = (int)strtol($12, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($12, ptr) ) {
                      yyerror("Illegal type: width must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\twidth = %d\n", width);
              templateObject.width = width;
          }
      String   /* String represents the integer height of the object */
          {
              int height;
              char *ptr;

              if ( (height = (int)strtol($14, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($14, ptr) ) {
                      yyerror("Illegal type: height must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\theight = %d\n", height);
              templateObject.height = height;
          }
      String   /* String represents the integer fill pattern of the object 
                * The values for these are listed in object.h
                */
          {
              int fillpat;
              char *ptr;

              if ( (fillpat = (int)strtol($16, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($16, ptr) ) {
                      yyerror("Illegal type: fill pattern must be an integer");
                      return;
                  }
              }
              if ( fillpat < 0 || fillpat > XGD_FILL_PATTERN_MAXIMUM ) {
                  fprintf(stderr,
                      "Warning: Fill pattern outside of range [%d-%d].\n",
                      0, XGD_FILL_PATTERN_MAXIMUM);
                  fillpat = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tfillpat = %d\n", fillpat);
              templateObject.fp = fillpat;
          }
      String   /* String represents the integer line pattern of the object 
                * The values for these are listed in object.h
                */
          {
              int linepat;
              char *ptr;

              if ( (linepat = (int)strtol($18, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($18, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if ( linepat < 0 || linepat > XGD_LINE_PATTERN_MAXIMUM ) {
                  fprintf(stderr,
                      "Warning: Line pattern outside of range [%d-%d].\n",
                      0, XGD_LINE_PATTERN_MAXIMUM);
                  linepat = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlinepat = %d\n", linepat);
              templateObject.lp = linepat;
          }
      String   /* String represents the integer line width of the object */
          {
              int linewidth;
              char *ptr;

              if ( (linewidth = (int)strtol($20, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($20, ptr) ) {
                      yyerror("Illegal type: line width must be an integer");
                      return;
                  }
              }
              if ( linewidth < 0 ) {
                  fprintf(stderr,
                      "Warning: Line width must be non-negative\n");
                  linewidth = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlinewidth = %d\n", linewidth);
              templateObject.lw = linewidth;
          }
      String   /* String represents the foreground color of the object
                * This must be one of the vector colors...
                * If it is not, you will get vector color[0]...
                */
          {
              if (__DEBUG ) fprintf(stderr,"\tfg = %s\n", $22);
              templateObject.fg = XgdGetVectColorPixelByName($22);
          }
      String   /* 
                * String represents the background color of the object 
                * This must be one of the vector colors... 
                * If it is not, you will get vector color[0]...
                */
          {
              templateObject.bg = XgdGetVectColorPixelByName($24);
              if (__DEBUG ) fprintf(stderr,"\tbg = %s\n", $24);

              XgdInitObject(Global.display, XtWindow(Global.drawArea), 
                curObject, templateObject.fp, templateObject.lp,
                templateObject.fg, templateObject.bg, 
                templateObject.lw);
          }
      OptionalSpecifics '>'  /* see Optionalspecific below... */
          {
              XgdObject *obj;
              if ( geoFrame ) {
                  obj = geoFrameObject;
              } else {
                  obj = curObject;
              }

              if ( TYPE_WITH_SPECIFICS(obj) && !specificsSet ) {
                  yyerror("Missing specifics...");
              }
              if ( yyerrflag ) {
                  return;
              }
              if ( geoFrame ) {
		  XgdConfigureObject(XgdGetGCOfObject(obj), 
                    obj, templateObject.x, templateObject.y, 
                    templateObject.width, templateObject.height, True);
                  geoFrame = False;
              } else if ( GEOFRAME_OBJECT_TYPE(obj) ) {
		  XgdConfigureObject(XgdGetGCOfObject(obj), obj, 
		    obj->x, obj->y, obj->width, 
                    obj->height, True);
              } else {
		  XgdConfigureObject(XgdGetGCOfObject(obj), obj, 
		    templateObject.x, templateObject.y, templateObject.width,
		    templateObject.height, True);
              }
	      /*AddObjectToList(&Global.selectedObjects, obj);*/
	      AddObjectToList(&Global.objectList, obj);
	      if ( GEOFRAME_OBJECT_TYPE(obj) ) {
		XgdRedrawGeoframe(obj, TRUE, TRUE, NULL);
	      } else
		XgdDrawObject(XgdGetGCOfObject(obj), obj, True, NULL);
	      /*XgdDrawResizeHandles(obj, Global.xorGC);*/

              if (__DEBUG ) fprintf(stderr, "OBJECT COMPLETE\n");
          }
    ;

ObjectType
    :  GEOFRAME
       { 
           $$ = XGD_GEOFRAME; 
       }
    |  SQUARE
       { 
           $$ = XGD_SQUARE; 
       }
    |  RECTANGLE
       { 
           $$ = XGD_RECTANGLE; 
       }
    |  CIRCLE
       { 
           $$ = XGD_CIRCLE; 
       }
    |  ELLIPSE
       { 
           $$ = XGD_ELLIPSE; 
       }
    |  POLYLINE
       { 
           $$ = XGD_POLYLINE; 
       }
    |  POLYGON
       { 
           $$ = XGD_POLYGON; 
       }
    |  OPEN_INTERP_SPLINE
       { 
           $$ = XGD_OPEN_INTERP_SPLINE; 
       }
    |  CLOSED_INTERP_SPLINE
       { 
           $$ = XGD_CLOSED_INTERP_SPLINE; 
       }
    |  OPEN_APPROX_SPLINE
       { 
           $$ = XGD_OPEN_APPROX_SPLINE; 
       }
    |  CLOSED_APPROX_SPLINE
       { 
           $$ = XGD_CLOSED_APPROX_SPLINE; 
       }
    |  LABEL
       { 
           $$ = XGD_LABEL; 
       }
    ;

OptionalSpecifics
    : /* none, its optional */

    | Specifics
    ;

Specifics
    : '<' GEOFRAME 
          {
              if (__DEBUG ) fprintf(stderr,"GeoFrame Specifics\n");
	      /*  
	       * set geoFrameObject to point to curObject so we can use 
	       * curobject in subobject specifications...
	       */
	      geoFrameObject = curObject;
          }
          OptionalGeoSpecifics '>'  
          {
              geoFrame = True;
              specificsSet = True;
          }

    | '<' SPLINE 
          {
              if (__DEBUG ) fprintf(stderr,"Spline Specifics\n");
          }
          PointList '>'
          {
              specificsSet = True;
          }

    | '<' POLYLINE 
          {
              if (__DEBUG ) fprintf(stderr,"Polyline Specifics\n");
          }
          PointList '>'
          {
              specificsSet = True;
          }

    | '<' POLYGON 
          {
              if (__DEBUG ) fprintf(stderr,"Polygon Specifics\n");
          }
          PointList '>'
          {
              specificsSet = True;
          }

    | '<' LABEL 
          {
              if (__DEBUG ) fprintf(stderr,"Label Specifics\n");
          }
          String 
          {
              ParseString($4, &curObject->Obj.Label.numlines, 
                              &curObject->Obj.Label.lblstr);
          }
          String '>'
          {
              if ((curObject->Obj.Label.font = 
                   XLoadQueryFont(Global.display, $6)) == NULL ) {
                  char buf[1024];
    
                  sprintf (buf, 
                     "Sorry, the selected font:\n[%s]\nIs not available.\n",
                     $6);
                  yyerror(buf);
                  return;
              }
              XgdSetLabelFont(curObject, curObject->Obj.Label.font, False);
              XgdSetLabelFontName(curObject, $6);
              specificsSet = True;
          }
    ;

OptionalGeoSpecifics
    : /* none, its optional */

    | GeoSpecificsList
    ;

GeoSpecificsList
    : GeoSpecificsList GeoSpecifics

    | GeoSpecifics
    ;

GeoSpecifics
    : '<' REGION 
          {
              ;
          }
          String  /* projection, must be PROJECTION_XY, PROJECTION_UTM,
                   * PROJECTION_SP, PROJECTION_LL, or PROJECTION_OTHER
                   * as defined in gis.h (integer 0-3, 99)
                   */
          {
              int projection;
              char *ptr;

              if ( (projection = (int)strtol($4, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($4, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if ( !(projection == PROJECTION_XY ||
                    projection == PROJECTION_UTM ||
                    projection == PROJECTION_SP ||
                    projection == PROJECTION_LL ||
                    projection == PROJECTION_OTHER) ) {
                    yywarning("Illegal projection in region specification.");
                    projection = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tprojection = %d\n", projection);
              curObject->Obj.GeoFrame.region.proj = projection;
          }
          String  /* zone (integer) */
          {
              int zone;
              char *ptr;

              if ( (zone = (int)strtol($6, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($6, ptr) ) {
                      yyerror("Illegal type: zone must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tzone = %d\n", zone);
              curObject->Obj.GeoFrame.region.zone = zone;
          }
          String  /* east west resolution (double) */
          {
	      double ew_res;
	      char *ptr = NULL;

	      if ( (ew_res = (double)strtod($8, &ptr, 10)) == 0 ) {
		  if ( ptr != NULL && !strcmp($8, ptr) ) {
		  yyerror("Illegal type: east-west res. must be an int/real");
		  return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\tew_res = %lf\n", ew_res);
              curObject->Obj.GeoFrame.region.ew_res = ew_res;
          }
          String  /* north-south resolution (double) */
          {
	      double ns_res;
	      char *ptr = NULL;

	      if ( (ns_res = (double)strtod($10, &ptr, 10)) == 0 ) {
		  if ( ptr != NULL && !strcmp($10, ptr) ) {
		  yyerror("Illegal type: north-south res. must be an int/real");
		  return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\tns_res = %lf\n", ns_res);
              curObject->Obj.GeoFrame.region.ns_res = ns_res;
          }
          String  /* north edge (double) */
          {
	      double north;
	      char *ptr = NULL;

	      if ( (north = (double)strtod($12, &ptr, 10)) == 0 ) {
		  if ( ptr != NULL && !strcmp($12, ptr) ) {
		      yyerror("Illegal type: north edge must be an int/real");
		      return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\tnorth = %lf\n", north);
              curObject->Obj.GeoFrame.region.north = north;
          }
          String  /* south edge (double) */
          {
	      double south;
	      char *ptr = NULL;

	      if ( (south = (double)strtod($14, &ptr, 10)) == 0 ) {
		  if ( ptr != NULL && !strcmp($14, ptr) ) {
		      yyerror("Illegal type: south edge must be an int/real");
		      return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\tsouth = %lf\n", south);
              curObject->Obj.GeoFrame.region.south = south;
          }
          String  /* east edge (double) */
          {
	      double east;
	      char *ptr = NULL;

	      if ( (east = (double)strtod($16, &ptr, 10)) == 0 ) {
		  if ( ptr != NULL && !strcmp($16, ptr) ) {
		      yyerror("Illegal type: east edge must be an int/real");
		      return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\teast = %lf\n", east);
              curObject->Obj.GeoFrame.region.east = east;
          }
          String  /* west edge  (double) */
          {
	      double west;
	      char *ptr = NULL;

	      if ( (west = (double)strtod($18, &ptr, 10)) == 0 ) {
		  if ( ptr != NULL && !strcmp($18, ptr) ) {
		      yyerror("Illegal type: west edge must be an int/real");
		      return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\twest = %lf\n", west);
              curObject->Obj.GeoFrame.region.west = west;
          }
          String  /* rows (integer) */
          {
              int rows;
              char *ptr;

              if ( (rows = (int)strtol($20, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($20, ptr) ) {
                      yyerror("Illegal type: rows must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\trows = %d\n", rows);
              curObject->Obj.GeoFrame.region.rows = rows;
          }
          String '>' /* columns (integer) */
          {
              int cols;
              char *ptr;

              if ( (cols = (int)strtol($22, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($22, ptr) ) {
                      yyerror("Illegal type: cols must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tcols = %d\n", cols);
              curObject->Obj.GeoFrame.region.cols = cols;
          }

    | '<' RASTER_MAP 
          {
              ;
          }
          String  /* mapset */
          {
              curObject->Obj.GeoFrame.rmapset = XtNewString($4);
          }
          String '>' /* name */
          {
              curObject->Obj.GeoFrame.rname = XtNewString($6);
          }

    | '<' VECTOR_MAP
          {
              ;
          }
          String  /* mapset */
          {
              vectTempl.mapset = XtNewString($4);
          }
          String  /* name */
          {
              vectTempl.name = XtNewString($6);
          }
          String  /* color, must be one of the vect colors */
          {
              vectTempl.fg = XgdGetVectColorPixelByName($8);
          }
          String  /* line pattern */
          {
              int linepat;
              char *ptr;

              if ( (linepat = (int)strtol($10, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($10, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if ( linepat < 0 || linepat > XGD_LINE_PATTERN_MAXIMUM ) {
                  fprintf(stderr,
                      "Warning: Line pattern outside of range [%d-%d].\n",
                      0, XGD_LINE_PATTERN_MAXIMUM);
                  linepat = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlinepat = %d\n", linepat);
              vectTempl.lp = linepat;
          }
          String '>' /* line width */
          {
              int linewidth;
              char *ptr;

              if ( (linewidth = (int)strtol($12, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($12, ptr) ) {
                      yyerror("Illegal type: line width must be an integer");
                      return;
                  }
              }
              if ( linewidth < 0 ) {
                  fprintf(stderr,
                      "Warning: Line width must be non-negative\n");
                  linewidth = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlinewidth = %d\n", linewidth);
              vectTempl.lw = linewidth;

              XgdInitVector(curObject, vectTempl.name, vectTempl.mapset,
                  vectTempl.fg, vectTempl.lp, vectTempl.lw);
          }

    | '<' SITE_MAP 
          {
              ;
          }
          String  /* name */
          {
              siteTempl.name = XtNewString($4);
fprintf(stderr,"DOING A SITE MAP\nname = %s\n",  siteTempl.name);
          }
          String  /* mapset */
          {
              siteTempl.mapset = XtNewString($6);
fprintf(stderr,"mapset = %s\n",  siteTempl.mapset);
          }
          SiteSpecifics '>' /* specifics for each type of site */
          {
              ;
          }

    | '<' GRID 
          {
            geoFrameObject->Obj.GeoFrame.gridOn = True;
	    if ( geoFrameObject->Obj.GeoFrame.grid.gc == NULL ) {
		XGCValues gcv;

		gcv.background = geoFrameObject->bg;
		geoFrameObject->Obj.GeoFrame.grid.gc =
		    XCreateGC(Global.display, XtWindow(Global.drawArea), 0, 0);
	    }
          }
          String  /* grid labels on  ? 1 : 0 */
          {
              int labelon;
              char *ptr;

              if ( (labelon = (int)strtol($4, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($4, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if ( !(labelon == 1 ||
                    labelon == 0) ) {
                    yywarning("Illegal label on/off flag in grid specification.");
                    labelon = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlabelon = %d\n", labelon);
              XgdSetGridLabelOn(geoFrameObject, labelon);
          }
          String  /* grid gap, or the geographic distance between grid lines */
          {
              double gap;
              char *ptr = NULL;

              if ( (gap = (double)strtod($6, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($6, ptr) ) {
		      yyerror("Illegal type: grid gap must be an int/real");
		      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tgap = %lf\n", gap);
              XgdSetGridGap(geoFrameObject, gap);
          }
          String  /* spacing, or the number of grid lines between labels */
          {
              int spacing;
              char *ptr;

              if ( (spacing = (int)strtol($8, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($8, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tspacing = %d\n", spacing);
              XgdSetGridSpacing(geoFrameObject, spacing);
          }
          String  /* grid line color */
          {
              Pixel color  = XgdGetVectColorPixelByName($10);
              if (__DEBUG ) fprintf(stderr,"\tcolor = %s\n", $10);
	      XgdSetGridColor(geoFrameObject, color);
          }
          String  /* grid line pattern */
          {
              int linepat;
              char *ptr;

              if ( (linepat = (int)strtol($12, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($12, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if ( linepat < 0 || linepat > XGD_LINE_PATTERN_MAXIMUM ) {
                  fprintf(stderr,
                      "Warning: Line pattern outside of range [%d-%d].\n",
                      0, XGD_LINE_PATTERN_MAXIMUM);
                  linepat = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlinepat = %d\n", linepat);
              gridLinePattern = linepat;
          }
          String  /* grid line width */
          {
              int linewidth;
              char *ptr;

              if ( (linewidth = (int)strtol($14, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($14, ptr) ) {
                      yyerror("Illegal type: line width must be an integer");
                      return;
                  }
              }
              if ( linewidth < 0 ) {
                  fprintf(stderr,
                      "Warning: Line width must be non-negative\n");
                  linewidth = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlinewidth = %d\n", linewidth);
              XgdSetGridLinePattern(geoFrameObject, linewidth, gridLinePattern);
          }
          String  /* grid label font */
          {
              XFontStruct *fs;
              int xoff = 0, yoff = 0;

              if ((fs = XLoadQueryFont(Global.display, $16)) == NULL ) {
                  char buf[1024];

                  sprintf (buf,
                     "Sorry, the selected font:\t\n[%s]\t\nIs not available.",
                     $16);
                  yyerror(buf);
                  return;
              }
              XgdSetGridFont(geoFrameObject, fs->fid, False);
              XgdSetGridFontName(geoFrameObject, $16);
              if ( geoFrameObject->Obj.GeoFrame.grid.labelon )
                  XgdCalculateGridOffset(geoFrameObject, &xoff, &yoff);
              XgdSetGridOffset(geoFrameObject, xoff, yoff);
          }
          String '>' /* grid label text color */
          {
              Pixel color  = XgdGetVectColorPixelByName($18);
              if (__DEBUG ) fprintf(stderr,"\ttextcolor = %s\n", $18);
	      XgdSetGridTextColor(geoFrameObject, color);
          }

    | '<' BARSCALE 
          {
	      /* create a new object */
	      curObject = XgdCreateObject(XGD_BARSCALE);
              XgdInitObject(Global.display, XtWindow(Global.drawArea), 
                curObject, Global.fillPattern, Global.linePattern,
                Global.foreground, Global.background, 
                Global.lineWidth);
          }
          String  /* x position */
          {
              int x;
              char *ptr = NULL;

              if ( (x = (int)strtol($4, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($4, ptr) ) {
                      yyerror("Illegal type: x position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tx = %d\n", x);
	      curObject->x = x;
          }
          String  /* y position */
          {
              int y;
              char *ptr = NULL;

              if ( (y = (int)strtol($6, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($6, ptr) ) {
                      yyerror("Illegal type: y position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\ty = %d\n", y);
	      curObject->y = y;
          }
          String  /* width */
          {
              int width;
              char *ptr = NULL;

              if ( (width = (int)strtol($8, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($8, ptr) ) {
                      yyerror("Illegal type: width must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\twidth = %d\n", width);
              curObject->width = width;
          }
          String  /* height */
          {
              int height;
              char *ptr = NULL;

              if ( (height = (int)strtol($10, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($10, ptr) ) {
                      yyerror("Illegal type: height must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\theight = %d\n", height);
              curObject->height = height;
          }
          String  /* style, dashed, or ticked */
          {
              int style;
              char *ptr;

              if ( (style = (int)strtol($12, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($12, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if ( !(style == DASHED ||
                    style == TICKED) ) {
                    yywarning("Illegal style in barscale specification.");
                    style = DASHED;
              }
              if (__DEBUG ) fprintf(stderr,"\tstyle = %d\n", style);
              XgdSetBarscaleStyle(curObject, style);
          }

          String  /* length in geographic units */
          {
	      double length;
	      char *ptr = NULL;

	      if ( (length = (double)strtod($14, &ptr, 10)) == 0 ) {
		  if ( ptr != NULL && !strcmp($14, ptr) ) {
		  yyerror("Illegal type: barscale length must be an int/real");
		  return;
		  }
	      }
              if (__DEBUG ) fprintf(stderr,"\tlength = %lf\n", length);
	      XgdSetBarscaleLength(curObject, length);
          }
          String  /* the number of intervals in the bar scale */
          {
              int intervals;
              char *ptr = NULL;

              if ( (intervals = (int)strtol($16, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($16, ptr) ) {
                      yyerror("Illegal type: intervals must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tintervals = %d\n", intervals);
	      XgdSetBarscaleInterval(curObject, intervals);
          }
          String  /* units of measure, kilometers, meter, miles, or feet */
          {
              int units;
              char *ptr;

              if ( (units = (int)strtol($18, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($18, ptr) ) {
                      yyerror("Illegal type: expecting integer");
                      return;
                  }
              }
              if ( !(units == XGD_KILOMETERS ||
                    units == XGD_METERS ||
                    units == XGD_MILES ||
                    units == XGD_FEET) ) {
                    yywarning("Illegal units in barscale specification.");
                    units = XGD_KILOMETERS;
              }
              if (__DEBUG ) fprintf(stderr,"\tunits = %d\n", units);
	      XgdSetBarscaleUnit(curObject, units);
          }
          String  /* line width */
          { 
              int linewidth;
              char *ptr;

              if ( (linewidth = (int)strtol($20, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($20, ptr) ) {
                      yyerror("Illegal type: line width must be an integer");
                      return;
                  }
              }
              if ( linewidth < 0 ) {
                  fprintf(stderr,
                      "Warning: Line width must be non-negative\n");
                  linewidth = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlinewidth = %d\n", linewidth);
	      XgdSetBarscaleLineWidth(curObject, linewidth);
          }
          String  /* color */
          {
              Pixel color  = XgdGetVectColorPixelByName($22);
              if (__DEBUG ) fprintf(stderr,"\tcolor = %s\n", $22);
	      XgdSetBarscaleColor(curObject, color);
          }
          String  /* font */
          {
	      XFontStruct *fs;

              if ((fs = XLoadQueryFont(Global.display, $24)) == NULL ) {
                  char buf[1024];

                  sprintf (buf,
                     "Sorry, the selected font:\t\n[%s]\t\nIs not available.",
                     $24);
                  yyerror(buf);
                  return;
              }
              XgdSetBarscaleFont(curObject, fs->fid, False);
              XgdSetBarscaleFontName(curObject, $24);
          }
          String '>' /* color of text */
          {
              Pixel textcolor = XgdGetVectColorPixelByName($26);
              if (__DEBUG ) fprintf(stderr,"\ttextcolor = %s\n", $26);
	      XgdSetBarscaleTextColor(curObject, textcolor);
	      /* set the window width */
              if ( geoFrameObject->Obj.GeoFrame.gridOn &&
                   geoFrameObject->Obj.GeoFrame.grid.labelon ) {
                  XgdSetBarscaleWinWidth(curObject, geoFrameObject->width - 
                      geoFrameObject->Obj.GeoFrame.grid.xoff);
              } else {
                  XgdSetBarscaleWinWidth(curObject, geoFrameObject->width);
              }
	      /* add the barscale into the geoFrameObject... */
              if (geoFrameObject->Obj.GeoFrame.numbarscales != 0) {
		int i;
		int oldcount = geoFrameObject->Obj.GeoFrame.numbarscales;
		XgdObject **newbs =
		    (XgdObject **)XtCalloc(oldcount + 1, sizeof(XgdObject *));

		for (i = 0; i < geoFrameObject->Obj.GeoFrame.numbarscales; i++ )
		    newbs[i] = geoFrameObject->Obj.GeoFrame.barscales[i];
		newbs[i] = curObject;
		XtFree(geoFrameObject->Obj.GeoFrame.barscales);
		geoFrameObject->Obj.GeoFrame.barscales = newbs;
		geoFrameObject->Obj.GeoFrame.numbarscales++;
	       } else {
		geoFrameObject->Obj.GeoFrame.barscales = (XgdObject **)
		    XtCalloc(1, sizeof(XgdObject*));
		geoFrameObject->Obj.GeoFrame.barscales[0] = curObject;
		geoFrameObject->Obj.GeoFrame.numbarscales++;
	       }

	      /*AddObjectToList(&Global.selectedObjects, curObject);*/
	      AddObjectToList(&Global.objectList, curObject);
              /*XgdDrawBarscale(curObject, True);*/
	      XgdConfigureObject(XgdGetGCOfObject(curObject), curObject, 
		curObject->x, curObject->y, curObject->width, 
		curObject->height, True);
	      /*XgdDrawResizeHandles(curObject, Global.xorGC);*/
              specificsSet = True;
          }

    | '<' LEGEND 
          {
	      /* create a new object */
	      curObject = XgdCreateObject(XGD_LEGEND);
              XgdInitObject(Global.display, XtWindow(Global.drawArea), 
                curObject, Global.fillPattern, Global.linePattern,
                Global.foreground, Global.background, 
                Global.lineWidth);
          }
          String  /* x position */
          {
              int x;
              char *ptr = NULL;

              if ( (x = (int)strtol($4, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($4, ptr) ) {
                      yyerror("Illegal type: x position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\tx = %d\n", x);
	      curObject->x = x;
          }
          String  /* y position */
          {
              int y;
              char *ptr = NULL;

              if ( (y = (int)strtol($6, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($6, ptr) ) {
                      yyerror("Illegal type: y position must be an integer");
                      return;
                  }
              }
              if (__DEBUG ) fprintf(stderr,"\ty = %d\n", y);
	      curObject->y = y;
          }
          String  /* XXX */
          {
              ;
          }
          String  /* XXX */
          {
              ;
          }
          String  /* XXX */
          {
              ;
          }
          String  /* XXX */
          {
              ;
          }
          String  /* XXX */
          {
              ;
          }
          String '>' /* XXX */
          {
              ;
          }
    ;

SiteSpecifics
    : '<' STANDARDSITE
          {
              siteTempl.type = XGD_SITE_STANDARD;
          }
          String /* icontype */
          {
              int icontype;
              char *ptr;

              if ( (icontype = (int)strtol($4, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($4, ptr) ) {
                      yyerror("Illegal type: icontype must be an integer");
                      return;
                  }
              }
              if ( icontype != CROSS && icontype != DIAMOND && 
                   icontype != RECT && icontype != PLUS ) {
                  fprintf(stderr,
                      "Warning: Invalid icon type\n");
                  icontype = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\ticontype = %d\n", icontype);
              siteTempl.icontype = icontype;
          }
          String /* size */
          {
              int size;
              char *ptr;

              if ( (size = (int)strtol($6, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($6, ptr) ) {
                      yyerror("Illegal type: size must be an integer");
                      return;
                  }
              }
              if ( size < 0 ) {
                  fprintf(stderr,
                      "Warning: Size must be non-negative\n");
                  size = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tsize = %d\n", size);
              siteTempl.size = size;
          }
          String /* linewidth */
          {
              int linewidth;
              char *ptr;

              if ( (linewidth = (int)strtol($8, &ptr, 10)) == 0 ) {
                  if ( ptr != NULL && !strcmp($8, ptr) ) {
                      yyerror("Illegal type: line width must be an integer");
                      return;
                  }
              }
              if ( linewidth < 0 ) {
                  fprintf(stderr,
                      "Warning: Line width must be non-negative\n");
                  linewidth = 0;
              }
              if (__DEBUG ) fprintf(stderr,"\tlinewidth = %d\n", linewidth);
              siteTempl.linewidth = linewidth;
          }
          String '>' /* color */
          {
              siteTempl.color  = XgdGetVectColorPixelByName($10);
              if (__DEBUG ) fprintf(stderr,"\tcolor = %s\n", $10);
              XgdInitSite(geoFrameObject, siteTempl.name, siteTempl.mapset, 
                  siteTempl.type);
              XgdUpdateSiteStd(geoFrameObject->Obj.GeoFrame.sites.Tail,
                  siteTempl.icontype, siteTempl.size, siteTempl.linewidth,
                  siteTempl.color);
          }

    | '<' PIXMAPSITE
          {
              siteTempl.type = XGD_SITE_PIXMAP;
          }
          String '>' /* pixmap file */
          {
              siteTempl.pixmapfile = XtNewString($4);

              XgdInitSite(geoFrameObject, siteTempl.name, siteTempl.mapset, 
                  siteTempl.type);
              XgdDrawSite(geoFrameObject, XtWindow(Global.drawArea),
			  siteTempl.name, siteTempl.mapset,
			  siteTempl.pixmapfile);
          }

    | '<' FREEHANDSITE 
          {
              ;
          }
          ObjectList '>' /* XXX */
    ;

PointList
    : Points NIL
    ;
    
Points
    : Points Point
    | Point

Point
    : '(' String ',' String ')' /* the Strings represent x and y */
      {
          int x, y;
          char *ptr = NULL;

          if ( (x = (int)strtol($2, &ptr, 10)) == 0 ) {
              if ( ptr != NULL && !strcmp($2, ptr) ) {
              yyerror("Illegal type: x value in point must be an integer");
                      return;
              }
          }
          if ( (y = (int)strtol($4, &ptr, 10)) == 0 ) {
              if ( ptr != NULL && !strcmp($4, ptr) ) {
              yyerror("Illegal type: x value in point must be an integer");
                      return;
              }
          }
          XgdAddPointToPointList(curObject, x, y);
      }
    ;
%%
