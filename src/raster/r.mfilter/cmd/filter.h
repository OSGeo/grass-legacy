#include "gis.h"
typedef struct
{
    int size;       /* size of filter matrix */
    int **matrix;   /* filter coefficient matrix */
    int **dmatrix;  /* divisor coefficient matrix */
    int divisor;    /* filter scale factor */
    int type;       /* sequential or parallel */
    int start;      /* starting corner */
} FILTER ;

#define PARALLEL 1
#define SEQUENTIAL 2
#define UL 1
#define UR 2
#define LL 3
#define LR 4

FILTER *get_filter();
