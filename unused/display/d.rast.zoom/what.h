#include "gis.h"
#define MAX_LAYERS 15

#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL int fd[MAX_LAYERS];
GLOBAL int nlayers;
/*
GLOBAL char *name[40];
struct Cell_head window, window1 ;
*/

