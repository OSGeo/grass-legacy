#ifndef GLOBAL
#define GLOBAL extern
#endif

#define TYPE_X  1
#define TYPE_PLUS   2
#define TYPE_BOX    3
#define TYPE_DIAMOND    4

#define RANGE_LOW_HIGH  1
#define RANGE_LESS  2
#define RANGE_GREATER  3
#define RANGE_EQUAL  4

#define USE_DIM  1
#define USE_CAT  2
#define USE_DBL  3

typedef struct {
    int use_this; /* USE_DIM USE_CAT USE_DBL */
    int range_type; /* RANGE_LOW_HIGH RANGE_LESS RANGE_GREATER RANGE_EQUAL */
    int param; /* use param_th dimension or param_th double */
    double lowval, highval, val;
} Site_rules;

GLOBAL int color ;
GLOBAL int size ;
GLOBAL int type ;
GLOBAL FILE *infile, *outfile;

