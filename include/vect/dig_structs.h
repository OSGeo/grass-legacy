/*
   **  Written by Dave Gerdes  5/1988
   **  US Army Construction Engineering Research Lab
 */
#include "config.h"

#ifndef  DIG___STRUCTS___
#define DIG___STRUCTS___

/*  this file depends on  <stdio.h> */
#ifndef _STDIO_H
#include <stdio.h>
#endif

#include "dgl.h"
#include "shapefil.h"
#include "btree.h"
#include "rtree.h"


#ifdef HAVE_POSTGRES
#include "libpq-fe.h"
#endif
#ifdef HAVE_OGR
#include "ogr_api.h"
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

#define BOUND_BOX struct bound_box

#define P_NODE struct P_node
#define P_AREA struct P_area
#define P_LINE struct P_line
#define P_ISLE struct P_isle

struct bound_box        /* Bounding Box */
  {
    double N;	/* north */			
    double S;   /* south */
    double E;   /* east */
    double W;   /* west */
    double T;   /* top */
    double B;   /* bottom */
  };

/* category field information */
struct field_info
  {
    int  number;   /* field number */
    char *name;    /* field name */
    char *driver;
    char *database;
    char *table;
    char *key;
  };

struct dblinks 
  {
    struct field_info *field;
    int alloc_fields, n_fields;
  };

/* Portability info */
struct Port_info
  {	  
    /* portability stuff, set in V1_open_new/old() */
    /* file byte order */
    int byte_order; 
      
    /* conversion matrices between file and native byte order */
    unsigned char dbl_cnvrt[PORT_DOUBLE];
    unsigned char flt_cnvrt[PORT_FLOAT];
    unsigned char lng_cnvrt[PORT_LONG];
    unsigned char int_cnvrt[PORT_INT];
    unsigned char shrt_cnvrt[PORT_SHORT];
    
    /* *_quick specify if native byte order of that type 
     * is the same as byte order of vector file (TRUE) 
     * or not (FALSE);*/
    int dbl_quick;
    int flt_quick;
    int lng_quick;
    int int_quick;
    int shrt_quick;
  };

struct dig_head
  {	  
    /*** HEAD_ELEMENT ***/
    char *organization;
    char *date;
    char *your_name;
    char *map_name;
    char *source_date;
    long orig_scale;
    char *line_3;
    int plani_zone;
    /* double W, E, S, N; */
    double digit_thresh;
    /* double map_thresh; */ /* not used in g51 */

    /* Programmers should NOT touch any thing below here */
    /* Library takes care of everything for you          */
    /*** COOR_ELEMENT ***/
    int Version_Major;
    int Version_Minor;
    int Back_Major;
    int Back_Minor;
    int with_z;
    
    long size;                  /* coor file size */
    long head_size;             /* coor header size */

    struct Port_info port;      /* Portability information */
   
    long last_offset;           /* offset of last read line */
    
    struct Map_info *Map;	/* X-ref to Map_info struct ?? */
  };

/* Coor info */
struct Coor_info
  {	  
    long size;     /* total size, in bytes */ 
    long mtime;    /* time of last modification */
  };

/* Non-native format inforamtion */
/* Shapefile */
struct Format_info_shp {
    char       *file;    /* path to shp file without .shp */
    char       *baseName; /* Name of the file without .shp */
    char       *dirName; /* Name of the directory shape is inside */
    /* char       *cat_col; */ /* category column */
    /* int        cat_col_num; */ /* category column number */
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
    int        lastRead;   /* id of last read line, 0 if no one was yet read */
};
#endif
/* OGR */
#ifdef HAVE_OGR
struct Format_info_ogr {
    char           *dsn;
    char           *layer_name;
    OGRDataSourceH ds;
    OGRLayerH      layer;
    int            nFeatures;
    int            feature;
    int            part;
} ;
#endif
struct Format_info {
    int i;
    struct Format_info_shp shp;
#ifdef HAVE_POSTGRES
    struct Format_info_post post;
#endif
#ifdef HAVE_OGR
    struct Format_info_ogr ogr;
#endif
} ;

struct Plus_head
  {
    int Version_Major;		/* version codes */
    int Version_Minor;
    int Back_Major;		/* earliest version that can use this data format */
    int Back_Minor;
    
    int spidx_Version_Major;		/* version codes for spatial index */
    int spidx_Version_Minor;
    int spidx_Back_Major;		/* earliest version that can use this data format */
    int spidx_Back_Minor;
    
    int with_z;
    int spidx_with_z;

    long head_size;             /* topo header size */
    long spidx_head_size;       /* spatial index header size */

    struct Port_info port;      /* Portability information */
    struct Port_info spidx_port;      /* Portability information for spatial index */
    int mode;			/* Read, Write, RW */

    struct bound_box box;      /* box */
    
    P_NODE **Node;	/* P_NODE array of pointers *//* 1st item is 1 for  */
    P_LINE **Line;	/* P_LINE array of pointers *//* all these (not 0) */
    P_AREA **Area;		
    P_ISLE **Isle;
   
    plus_t n_nodes;		/* Current Number of nodes */
    plus_t n_edges;		/* Current Number of edges */
    plus_t n_lines;		/* Current Number of lines */
    plus_t n_areas;		/* Current Number of areas */
    plus_t n_isles;
    plus_t n_volumes;		/* Current Number of volumes */
    plus_t n_holes;		/* Current Number of holes */

    plus_t n_plines;		/* Current Number of point    lines */
    plus_t n_llines;		/* Current Number of line     lines */
    plus_t n_blines;		/* Current Number of boundary lines */
    plus_t n_clines;		/* Current Number of centroid lines */
    plus_t n_flines;		/* Current Number of face lines */
    plus_t n_klines;		/* Current Number of kernel lines */

    plus_t alloc_nodes;		/* # of nodes we have alloc'ed space for 
				     i.e. array size - 1 */
    plus_t alloc_edges;
    plus_t alloc_lines;		/* # of lines we have alloc'ed space for */
    plus_t alloc_areas;		/* # of areas we have alloc'ed space for */
    plus_t alloc_isles;		/* # of isles we have alloc'ed space for */
    plus_t alloc_volumes;
    plus_t alloc_holes;

    long Node_offset;           /* offset of array of nodes in topo file */
    long Edge_offset;
    long Line_offset;
    long Area_offset;
    long Isle_offset;
    long Volume_offset;
    long Hole_offset;

    /* Spatial index */
    long Node_spidx_offset;     /* offset of spindex */
    long Edge_spidx_offset;
    long Line_spidx_offset;
    long Area_spidx_offset;
    long Isle_spidx_offset;
    long Volume_spidx_offset;
    long Hole_spidx_offset;
    
    struct Node *Node_spidx;
    struct Node *Line_spidx;
    struct Node *Area_spidx;
    struct Node *Isle_spidx;

    long coor_size;		/* size of coor file */
    long coor_mtime;		/* time of last coor modification */

    /* Level2 update: list of lines and nodes updated (topo info for the line was changed) 
    *                 by last write/rewrite/delete operation.
    *                 Lines/nodes in the list may be deleted (e.g. delete boundary: first added for
    *                 delete area and then delete */
    int do_uplist;     /* used internaly in diglib to know if list is maintained */
    
    int *uplines;      /* array of updated lines */
    int alloc_uplines; /* allocated array */
    int n_uplines;     /* number of updated lines */
    int *upnodes;      /* array of updated nodes */
    int alloc_upnodes; /* allocated array */
    int n_upnodes;     /* number of updated nodes */
  };

struct Map_info
  {
    /* Common info for all formats */  
    int format;                /* format */

    struct dblinks *dblnk;      /* info about tables */ 
    
    struct Plus_head plus;      /* topo file *head; */
    dglGraph_s   graph;       	/* graph structure */
    dglSPCache_s spCache;     /* Shortest path cache */ 

    /*  All of these apply only to runtime, and none get written out
    **  to the dig_plus file 
    */
    int open;			/* should be 0x5522AA22 if opened correctly */
                                /* or        0x22AA2255 if closed           */
                                /* anything else implies that structure has */
                                /* never been initialized                   */
    int mode;			/*  Read, Write, RW                         */
    int level;			/*  1, 2, (3)                               */
    plus_t next_line;		/* for Level II sequential reads */

    char *name;			/* for 4.0  just name, and mapset */
    char *mapset;

    /* Constraints for reading in lines  (not polys yet) */
    int    Constraint_region_flag;
    int    Constraint_type_flag;
    double Constraint_N;
    double Constraint_S;
    double Constraint_E;
    double Constraint_W;
    double Constraint_T;
    double Constraint_B;
    int    Constraint_type;
    int    proj;

    /* format specific */
    /* native */
    FILE   *dig_fp;		/* Dig file pointer */
    char   *digit_file;		/* digit file */
    struct dig_head head;	/* coor file head */
    
    /* non native */
    struct Format_info fInfo;  /* format information */
  };

struct P_node
  {
    double x;			/* X coordinate */
    double y;			/* Y coordinate */
    double z;			/* Z coordinate */
    plus_t alloc_lines;  
    plus_t n_lines;	/* Number of attached lines (size of lines, angle) */
    /*  If 0, then is degenerate node, for snappingi ??? */
    plus_t *lines;		/* Connected lines */
    float  *angles;		/* Respected angles. Angles for lines/boundaries are in radians between
				 * -PI and PI. Value for points or lines with identical points (degenerated)
				 * is set to -9. */  
  };

struct P_line
  {
    plus_t N1;		/* start node */
    plus_t N2;		/* end node   */
    plus_t left;	/* area/isle number to left, negative for isle  */
                        /* !!! area number for centroid, negative for   */
			/* duplicate centroid                           */ 
    plus_t right;	/* area/isle number to right, negative for isle */

    double N;		/* Bounding Box */
    double S;
    double E;
    double W;
    double T;           /* top */
    double B;           /* bottom */ 

    long offset;	/* offset in coor file for line */
    char type;	
  };

struct P_area
  {
    double N;		/* Bounding Box */
    double S;
    double E;
    double W;
    double T;           /* top */
    double B;           /* bottom */ 
    plus_t n_lines;	/* Number of boundary lines */
    plus_t alloc_lines;
    plus_t *lines;	/* Boundary Lines, negative means direction N2 to N1,
			   lines are in  clockwise order */
    
    /*********  Above this line is compatible with P_isle **********/
    
    plus_t centroid;		/* Number of first centroid within area */

    plus_t n_isles;		/* Number of islands inside */
    plus_t alloc_isles;
    plus_t *isles;		/* 1st generation interior islands */
  };

struct P_isle
  {
    double N;		/* Bounding Box */
    double S;
    double E;
    double W;
    double T;           /* top */
    double B;           /* bottom */ 
    plus_t n_lines;	/* Number of boundary lines */
    plus_t alloc_lines;
    plus_t *lines;	/* Boundary Lines, negative means direction N2 to N1,
			   lines are in counter clockwise order */

    /*********  Above this line is compatible with P_area **********/

    plus_t area;	/* area it exists w/in, if any */
  };

struct line_pnts
  {
    double *x;
    double *y;
    double *z;
    int n_points;
    int alloc_points;
  };

struct line_cats
  {
    int *field;		/* pointer to array of fields */
    int *cat;		/* pointer to array of categories */
    int n_cats;		/* number of vector categories attached to element */
    int alloc_cats;	/* allocated space */
  };

struct cat_list
  {
    int field;        /* category field */	  
    int *min;         /* pointer to array of minimun values */
    int *max;         /* pointer to array of maximum values */
    int n_ranges;     /* number ranges */
    int alloc_ranges; /* allocated space */
  };

/* list of integers */
struct ilist
  {
    int    *value;       /* items */	  
    int    n_values;     /* number of values */
    int    alloc_values; /* allocated space */
  };

#endif /* DIG___STRUCTS___ */

