#include <stdio.h>
#ifdef MAIN
#define EXTERN
#else
#define EXTERN extern
#endif

EXTERN int color ;
EXTERN float hsize ;
EXTERN float vsize ;
EXTERN int t, b, l, r ;
EXTERN FILE *infile ;
