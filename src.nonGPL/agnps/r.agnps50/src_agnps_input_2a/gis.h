#ifndef __GRASS_GIS_LIB__
#define __GRASS_GIS_LIB__
static char *GRASS_copyright = "GRASS (TM) Public Domain Software" ;
#define MAXEDLINES  50
#define RECORD_LEN  80
#define NEWLINE     '\n'
#define RECLASS_TABLE 1
#define RECLASS_RULES 2
#define RECLASS_SCALE 3

#define METERS    1
#define FEET      2
#define DEGREES   3

typedef int CELL;
extern CELL CELL_NODATA;

#define PROJECTION_XY  0
#define PROJECTION_UTM 1
#define PROJECTION_SP  2
#define PROJECTION_LL  3
#define PROJECTION_OTHER  99

#define PROJECTION_FILE "PROJ_INFO"
#define UNIT_FILE "PROJ_UNITS"

struct Cell_head
{
    int format       ;  /* max numer of bytes per cell minus 1          */
    int compressed   ;  /* 0 = uncompressed, 1 = compressed, -1 pre 3.0 */
    int rows, cols   ;  /* number of rows and columns in the data       */
    int proj         ;  /* Projection (see #defines above)              */
    int zone         ;  /* Projection zone                              */
    double ew_res    ;  /* East to West cell size                       */
    double ns_res    ;  /* North to South cell size                     */
    double north     ;  /* coordinates of layer                         */
    double south     ;
    double east      ;
    double west      ;
} ;

struct Categories
{
    CELL num              ;   /* total number of categories              */
    char *title           ;   /* name of data layer                      */ 
    char *fmt             ;   /* printf-like format to generate labels   */
    float m1              ;   /* Multiplication coefficient 1            */
    float a1              ;   /* Addition coefficient 1                  */
    float m2              ;   /* Multiplication coefficient 2            */
    float a2              ;   /* Addition coefficient 2                  */
    struct Cat_List
    {
	CELL num          ;   /* category number */
	char *label       ;   /* category label */
    } *list               ;
    int count             ;   /* number of labels lots used */
    int nalloc            ;   /* number of labels lots allocated */
} ;

struct _Color_Rule_
{
    struct
    {
	CELL cat;
	unsigned char red,grn,blu;
    } low, high;
    struct _Color_Rule_ *next;
    struct _Color_Rule_ *prev;
};

struct _Color_Info_
{
    struct _Color_Rule_ *rules;
    struct
    {
	unsigned char *red;
	unsigned char *grn;
	unsigned char *blu;
	unsigned char *set;
	unsigned char r0,g0,b0,s0;
	int nalloc;
	int active;
    } lookup;
    CELL min, max;
};

struct Colors
{
    int version;	/* set by read_colors: -1=old,1=new */
    int shift;
    int invert;
    struct _Color_Info_ fixed, modular;
    CELL cmin, cmax;
};

struct Reclass
{
    char name[50]           ; /* name of cell file being reclassed    */
    char mapset[50]         ; /* mapset in which "name" is found      */
    int type                ; /* type of reclass                      */
    int num                 ; /* size of reclass table                */
    CELL min,max            ; /* table range                          */
    CELL *table             ; /* reclass table                        */
} ;


struct History
{
    char    mapid[RECORD_LEN];
    char    title[RECORD_LEN];
    char    mapset[RECORD_LEN];
    char    creator[RECORD_LEN];
    char    maptype[RECORD_LEN];
    char    datsrc_1[RECORD_LEN];
    char    datsrc_2[RECORD_LEN];
    char    keywrd[RECORD_LEN];
    int     edlinecnt;
    char    edhist[MAXEDLINES][RECORD_LEN];
} ;

struct Cell_stats
{
    struct Cell_stats_node
    {
	int idx;
	long *count;
	int left;
	int right;
    } *node ;     /* tree of values */
    int tlen ;    /* allocated tree size */
    int N;        /* number of actual nodes in tree */
    int curp;
    int curoffset;
} ;

struct Histogram
{
    int num;
    struct Histogram_list
    {
	CELL cat;
	long count;
    } *list;
} ;

struct Range
{
    CELL nmin;		/* min negative */
    CELL nmax;		/* max negative */
    CELL pmin;		/* min positive */
    CELL pmax;		/* max posivive */
/*
 * nmin <= nmax <= 0 <= pmin <= pmax
 */
};


/*
** Structure for I/O of 3dview files  (view.c)
*/
struct G_3dview{
    char pgm_id[40];            /* user-provided identifier */
    float from_to[2][3];        /* eye position & lookat position */
    float fov;                  /* field of view */
    float twist;                /* right_hand rotation about from_to */
    float exag;                 /* terrain elevation exageration */
    int mesh_freq, poly_freq;   /* cells per grid line, cells per polygon */
    int display_type;           /* 1 for mesh, 2 for poly, 3 for both */
    int lightson, dozero, colorgrid, shading, fringe, surfonly, doavg;/*bools*/
    char grid_col[40], bg_col[40], other_col[40]; /* colors */
    float lightpos[4]; /* easting, northing, height, 1.0 for local 0.0 infin */
    float lightcol[3];       /* values between 0.0 to 1.0 for red, grn, blu */
    float ambient, shine;
    struct Cell_head vwin;
};


struct Key_Value
{
    int nitems;
    int nalloc;
    char **key;
    char **value;
};


struct Option                /* Structure that stores option info */
{
    char *key ;                     /* Key word used on command line    */
    int type ;                      /* Option type                      */
    int required ;                  /* REQUIRED or OPTIONAL             */
    int multiple ;                  /* Multiple entries OK              */
    char *options ;                 /* Approved values or range or NULL */
    char *key_desc;                 /* one word describing the key      */
    char *description ;             /* String describing option         */
    char *answer ;                  /* Option answer                    */
    char *def ;                     /* Where original answer gets saved */
    char **answers ;                /* Option answers (for multiple=YES)*/
    struct Option *next_opt ;       /* Pointer to next option struct    */
    char *gisprompt ;               /* Interactive prompt guidance      */
    int (*checker)() ;              /* Routine to check answer or NULL  */
    int count;
} ;

struct Flag                 /* Structure that stores flag info  */
{
    char key ;                      /* Key char used on command line    */
    char answer ;                   /* Stores flag state: 0/1           */
    char *description ;             /* String describing flag meaning   */
    struct Flag *next_flag ;        /* Pointer to next flag struct      */
} ;

/* for G_parser() */
#define TYPE_INTEGER  1
#define TYPE_DOUBLE   2
#define TYPE_STRING   3
#define YES           1
#define NO            0

#ifndef FILE
#include <stdio.h>
#endif

#include "gisdefs.h"
#endif /*__GRASS_GIS_LIB__*/
