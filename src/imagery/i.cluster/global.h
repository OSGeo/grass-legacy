#include "imagery.h"

#ifndef GLOBAL
# define GLOBAL extern
#endif



GLOBAL struct Cluster C;
GLOBAL struct Signature in_sig;

GLOBAL int maxclass ;
GLOBAL double conv ;
GLOBAL double sep ;
GLOBAL int iters ;
GLOBAL int mcs;
GLOBAL char group[50];
GLOBAL char subgroup[50];
GLOBAL struct Ref ref;
GLOBAL char outsigfile[50];
GLOBAL char insigfile[50];
GLOBAL char *reportfile;
GLOBAL CELL **cell;
GLOBAL int *cellfd;
GLOBAL FILE *report;
GLOBAL int sample_rows, sample_cols;
