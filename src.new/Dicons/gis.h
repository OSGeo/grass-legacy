/*  %W% %G% */

#define MAXEDLINES  25
#define RECORD_LEN  80
#define NEWLINE     '\n'
#define RECLASS_TABLE 1
#define RECLASS_RULES 2
#define RECLASS_SCALE 3

#define METERS 1
#define FEET   2

typedef int CELL;
typedef unsigned char uchar;

#define PROJECTION_XY  0
#define PROJECTION_UTM 1
#define PROJECTION_SP  2
#define PROJECTION_LL  3

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
    int count             ;   /* number of labels allocated               */
} ;

struct Colors
{
    CELL min,max    ;   /* min,max color numbers               */
    uchar *red      ;   /* red, green, blu (0-255)             */
    uchar *grn      ;   /* allocated as needed                 */
    uchar *blu      ;
    uchar r0,g0,b0  ;   /* red, green, blue for cat 0          */
} ;

struct Reclass
{
    char name[30]           ; /* name of cell file being reclassed    */
    char mapset[30]         ; /* mapset in which "name" is found      */
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

#define USAGE_LONG	1
#define USAGE_SHORT	0
struct Command_keys	/* used with G_parse_command() */
{
    char *alias;
    int position;
};

#ifndef FILE
#include <stdio.h>
#endif

#include "gisdefs.h"
