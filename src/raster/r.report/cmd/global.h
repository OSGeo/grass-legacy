#ifndef GLOBAL
# define GLOBAL extern
# define INIT(x)
#else
# define INIT(x) = x
#endif

#include "gis.h"

GLOBAL struct Cell_head window ;

#define LAYER struct _layer_
GLOBAL LAYER
{
    char *name;
    char *mapset;
    struct Categories labels;
    int nlen;               /* num chars of largest cat when printed */
    int clen;               /* num chars for cat label when printed */
} *layers INIT(NULL);
GLOBAL int nlayers INIT(0);

#define GSTATS struct _gstats_
GLOBAL GSTATS
{
    long *cats;
    double area;
    long count;
} *Gstats INIT(NULL);
GLOBAL int nstats INIT(0);

#define MAX_UNITS 10
#define UNITS struct _units_
GLOBAL UNITS
{
    double factor;
    int type;
    int len;
    int dp;
    int eformat;
    char *label[2];
}unit[MAX_UNITS];
GLOBAL int nunits INIT(0);

#define DEFAULT_PAGE_LENGTH 0
#define DEFAULT_PAGE_WIDTH  79
GLOBAL int page_width INIT(DEFAULT_PAGE_WIDTH);
GLOBAL int page_length INIT(DEFAULT_PAGE_LENGTH);
GLOBAL int masking INIT(1);
GLOBAL int use_formfeed INIT(0);
GLOBAL int nlines INIT(0);
GLOBAL int with_headers INIT(1);
GLOBAL int verbose INIT(1);
GLOBAL int e_format INIT(0);
GLOBAL int z_option INIT(0);

GLOBAL char *stats_file;
GLOBAL int stats_flag INIT(0);
#define EVERYTHING 0
#define REPORT_ONLY 1
#define STATS_ONLY 2

#define ACRES		1
#define HECTARES	2
#define SQ_MILES	3
#define PERCENT_COVER	4
#define CELL_COUNTS	5
#define SQ_METERS	6
#define SQ_KILOMETERS	7
