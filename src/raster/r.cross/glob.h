#include "gis.h"

#define NFILES 10  /* maximum number of layers */
#define SHIFT 6 /* 2^SHIFT cats per node */
#define INCR 16

#define FOUND 0
#define LEFT  1
#define RIGHT 2

#ifndef GLOBAL
# define GLOBAL extern
# define INIT(x)
#else
# define INIT(x) =(x)
#endif

GLOBAL int nfiles;
GLOBAL int nrows;
GLOBAL int ncols;
GLOBAL int NCATS INIT(1<<SHIFT);
GLOBAL char *names[NFILES];
GLOBAL struct Categories labels[NFILES];

typedef struct
{
    CELL *cat;
    CELL *result;
    int left;
    int right;
} NODE;

GLOBAL NODE *tree; /* tree of values */
GLOBAL int tlen;   /* allocate tree size */
GLOBAL int N;      /* number of actual nodes in tree */

typedef struct
{
    CELL *cat;
    CELL result;
} RECLASS;

GLOBAL RECLASS *reclass INIT(NULL);
GLOBAL CELL *table INIT(NULL);
