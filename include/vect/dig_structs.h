/*
   **  Written by Dave Gerdes  5/1988
   **  US Army Construction Engineering Research Lab
 */
#include "config.h"

#ifndef  DIG___STRUCTS___
#define DIG___STRUCTS___
#include "dig_head.h"

/*  this file depends on  <stdio.h> */
#ifndef _STDIO_H
#include <stdio.h>
#endif

#include "shapefil.h"


#ifdef HAVE_POSTGRES
#include "libpq-fe.h"
#endif

#define HEADSTR	50

/*
   **  NOTE: 3.10 changes plus_t to  ints.
   **    This assumes that any reasonable machine will use 4 bytes to
   **    store an int.  The mapdev code is not guaranteed to work if
   **    plus_t is changed to a type that is larger than an int.
 */
/*
   typedef short plus_t;
 */
typedef int plus_t;


#define P_NODE struct P_node
#define P_AREA struct P_area
#define P_LINE struct P_line
#define P_CAT struct P_cat
#define P_ISLE struct P_isle
#define P_ATT struct P_att



/* Non-native format inforamtion */
/* Shapefile */
struct Format_info_shp {
    char       *file;    /* path to shp file without .shp */
    char       *cat_col; /* category column */
    SHPHandle  hShp;     
    DBFHandle  hDbf;
    int        type;     /* shapefile type */
    int        nShapes;  /* number of shapes */
    int        shape;    /* offset: next shape */ 
    int        part;     /* offset: next part */ 
};
/* PostGIS */
#ifdef HAVE_POSTGRES
struct Format_info_post {
    char       *host;        /* host name */
    char       *port;        /* port number */
    char       *database;    /* database name */
    char       *user;        /* user name */ 
    char       *password;    /* user password */
    char       *geom_table;  /* geometry table name */
    char       *cat_table;   /* category table name */
    char       *geom_id;     /* geometry table: id column */
    char       *geom_type;   /* geometry table: type column */
    char       *geom_geom;   /* geometry table: column name */
    char       *cat_id;      /* category table: id column */
    char       *cat_field;   /* category table: field column */
    char       *cat_cat;     /* category table: category column */
    PGconn     *conn;        /* connection */
    int        selected;     /* 0 - data not selected, 1 - data selected */
    PGresult   *geomRes;    /* results from geometry table */
    PGresult   *catRes;     /* results from category table */
    int        nGeom;        /* number of selected geometry records */ 
    int        nCat;         /* number of selected category records */ 
    int        nextRow;      /* number of the next row in geometry 
				selection to be read */ 
    int        nextId;       /* id number of the next geometry record to be read */ 
};
#endif
struct Format_info {
    int i;
    struct Format_info_shp shp;
#ifdef HAVE_POSTGRES
    struct Format_info_post post;
#endif
} ;


struct Map_info
  {
    int format;         /* format */
    struct Format_info fInfo;  /* format information */
      
    P_NODE *Node;		/* P_NODE array *//* 1st item is 1 for  */
    P_AREA *Area;		/* P_AREA array *//* all these (not 0) */
    P_LINE *Line;		/* P_LINE array */
    P_CAT *Cat;			/* P_CAT  array */
    P_ATT *Att;
    P_ISLE *Isle;

    plus_t n_nodes;		/* Current Number of nodes */
    plus_t n_lines;		/* Current Number of lines */
    plus_t n_cats;		/* Current Number of category lists */
    plus_t n_areas;		/* Current Number of areas */
    plus_t n_atts;		/* Current Number of attributes */
    plus_t n_isles;
    plus_t n_alines;		/* Current Number of area lines */
    plus_t n_llines;		/* Current Number of line lines */
    plus_t n_plines;		/* Current Number of point lines */
    int n_points;		/* Current Number of points */

    plus_t alloc_nodes;		/* # of nodes we have alloc'ed space for */
    plus_t alloc_lines;		/* # of lines we have alloc'ed space for */
    plus_t alloc_cats;		/* # of cat lists we have alloc'ed space for */
    plus_t alloc_areas;		/* # of areas we have alloc'ed space for */
    plus_t alloc_atts;		/* # of atts  we have alloc'ed space for */
    plus_t alloc_isles;		/* # of isles  we have alloc'ed space for */

    /*  Note that dig_plus does not have a pointer, cuz it does
       **    not remain open.  All of its contents are currently read
       **    into memory at time of open
       **    att_fp also is not kept open, but for some reason it 
       **    was in here, so I will leave it for now.
       **  Note that dig_fp and att_fp are new names for 4.0
       **    they used to be digit and att
     */
    FILE *dig_fp;		/* Dig file pointer */
    char *digit_file;		/* digit file */

    char *plus_file;		/* Dig+ file */
    char *coor_file;		/* Point registration file */

    /*struct dig_head *head; */
    struct dig_head head;

    double snap_thresh;
    double prune_thresh;

    int all_areas;		/* if TRUE, all areas have just been calculated */
    int all_isles;		/* if TRUE, all islands have just been calculated */


/*********************  New Vlib data for 4.0 ***********************/
    /*  All of these apply only to runtime, and none get written out
       **  to the dig_plus file
     */

    int open;			/* should be 0x5522AA22 if opened correctly */
    /* or        0x22AA2255 if closed           */
    /* anything else implies that structure has */
    /* never been initialized                   */
    int mode;			/*  Read, Write, RW                           */
    int level;			/*  1, 2, (3)                               */
    plus_t next_line;		/* for Level II sequential reads */

    char *name;			/* for 4.0  just name, and mapset */
    char *mapset;

    /* Constraints for reading in lines  (not polys yet) */
    int Constraint_region_flag;
    int Constraint_type_flag;
    double Constraint_N;
    double Constraint_S;
    double Constraint_E;
    double Constraint_W;
    int Constraint_type;
    int proj;
  };


struct Plus_head
  {
    int Major;			/* version codes */
    int Minor;

    plus_t n_nodes;
    plus_t n_lines;
    plus_t n_cats;
    plus_t n_areas;
    plus_t n_atts;
    plus_t n_isles;
    plus_t n_llines;
    plus_t n_alines;
    plus_t n_plines;
    int n_points;

    long Node_offset;
    long Line_offset;
    long Cat_offset;
    long Area_offset;
    long Att_offset;
    long Isle_offset;

    long Dig_size;		/* size of dig file */
    long Att_size;		/* size of attribute file */
    long Dig_code;		/* internal check codes */
    long Att_code;

    int all_areas;		/* if TRUE, all areas have just been calculated */
    int all_isles;		/* if TRUE, all islands have just been calculated */

    double snap_thresh;
    double prune_thresh;

    long Back_Major;		/* earliest version that can use this data format */
    long Back_Minor;
    long future3;
    long future4;
    double F1, F2, F3, F4;	/* in anticipation of future needs */

    char Dig_name[HEADSTR];
    char filler[HEADSTR];
  };

struct P_line
  {
    plus_t N1;			/* start node */
    plus_t N2;			/* end node */
    /* left and right are negative if an island */
    plus_t left;		/* area number to left */
    plus_t right;		/* area number to right */

    double N;			/* Bounding Box */
    double S;
    double E;
    double W;

    long offset;		/* offset in DIG file for line */
    plus_t att;			/* attribute index number if such, else 0 */
    char type;			/*  see mode.h */
  };

struct P_cat
  {

    void *table_meta_info;	/* Use this to store information about
				   the true location, other info, about the
				   associated fields. Needed (?)
				 */

    plus_t alloc_cats;
    plus_t n_cats;

    long offset;
  };

struct P_node
  {
    double x;			/* X ord */
    double y;			/* Y ord */
    plus_t alloc_lines;
    plus_t n_lines;		/* Number of attached lines (size of lines, angle) */
    /*  If 0, then is degenerate node, for snapping */
    plus_t *lines;		/* Connected lines */
    float *angles;		/* Respected angles */
    char alive;			/* deleted or not   0 or !0  */
  };

struct P_area
  {
    double N;			/* Bounding Box */
    double S;
    double E;
    double W;
    plus_t n_lines;		/* Number of boundary lines */
    plus_t alloc_lines;
    plus_t *lines;		/* Boundary Lines (Negative means N2 to N1 clockwise) */
    char alive;			/* deleted or not   0 or !0  */
/*********  Above this line is compatible with P_isle **********/

    plus_t att;			/* attribute index number, if labeled */
    plus_t n_isles;		/* Number of boundary areas inside */
    plus_t alloc_isles;
    plus_t *isles;		/* 1st generation interior islands */
  };


struct P_isle
  {
    double N;			/* Bounding Box */
    double S;
    double E;
    double W;
    plus_t n_lines;		/* Number of boundary lines */
    plus_t alloc_lines;
    plus_t *lines;		/* Boundary Lines (Negative means N2 to N1
				   counter-clockwise) */
    char alive;			/* deleted or not   0 or !0  */
/*********  Above this line is compatible with P_area **********/

    plus_t area;		/* area it exists w/in, if any */
  };

struct P_att
  {
    double x;			/* x ord location point */
    double y;			/* y ord location point */
    long offset;		/* Offset into Attr file */
    int cat;			/* Category Number */
    plus_t index;		/* Area or Line number it's attached to */
    char type;			/* see mode.h */
  };

struct new_node
  {
    plus_t N1;
    plus_t N2;
    int cnt;
  };

struct line_pnts
  {
    double *x;
    double *y;
    double *z;
    int n_points;
    int alloc_points;
  };

typedef char GV_NCATS;	      /* number of vector categories on one element */
  typedef char GRASS_V_NCATS; /* deprecated */
typedef int GV_FIELD;	      /* field number i.e. identifier for
			         one category */
  typedef int GRASS_V_FIELD;  /* deprecated */
typedef int GV_CAT;	      /* vector category */
  typedef int GRASS_V_CAT;    /* deprecated */

struct line_cats
  {
    GV_FIELD *field;		/* pointer to array of fields */
    GV_CAT *cat;			/* pointer to array of categories */
    GV_NCATS n_cats;		/* number of vector categories attached to element */
    int alloc_cats;		/* allocated space */
  };

struct cat_list
  {
    GV_FIELD field;      /* category field */	  
    GV_CAT *min;         /* pointer to array of minimun values */
    GV_CAT *max;         /* pointer to array of maximum values */
    int         n_ranges;     /* number ranges */
    int         alloc_ranges; /* allocated space */
  };

/* category field information */
struct field_info
  {
    char *driver;
    char *database;
    char *table;
    char *key;
  };

#endif /* DIG___STRUCTS___ */

