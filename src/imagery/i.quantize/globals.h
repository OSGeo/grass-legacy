#include "imagery.h"

#ifndef GLOBAL
# define GLOBAL extern
#endif

GLOBAL int *RED;
GLOBAL int *GRN;
GLOBAL int *BLU;

GLOBAL int r_level;
GLOBAL int g_level;
GLOBAL int b_level;

GLOBAL struct Ref ref;

GLOBAL char group[40], result[40];
