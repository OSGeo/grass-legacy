#include "gis.h"
#include "Vect.h"

#ifndef global
#define global extern
#endif 
struct pointlist {
    double *x;
    double *y;
    int npoints;
    int **neighbors;
    long *offset;
    char *reported;
    FILE *fd;
    char *fs;
};

global struct pointlist pointlist;

struct parms {
    char *input;
    char *output;
    char *fs;
    char **barriers;
    char **fields;
    int quiet;
    int region;
};

struct arc {
    double *x;
    double *y;
    int n;
};

struct box {
    double max_y;
    double min_y;
    double max_x;
    double min_x;
} ;
