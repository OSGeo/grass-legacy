#include <stdio.h>

#define INPUT   1
#define COLOR   2
#define HSIZE   3
#define VSIZE   4

#ifdef MAIN
    struct variables
    {
        char *alias ;
        int position ;
    } variables[] = {
        "input", INPUT,
        "in", INPUT,
        "vsize", VSIZE,
        "hsize", HSIZE,
        "vs", VSIZE,
        "hs", HSIZE,
        "color", COLOR,
        "c", COLOR,
    } ;
    static int n_variables = 8 ;

    int color ;
    double hsize ;
    double vsize ;
    FILE *infile ;
#else
    extern int color ;
    extern double hsize ;
    extern double vsize ;
    extern FILE *infile ;
#endif
