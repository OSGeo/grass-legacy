
#include "gis.h"
#define MAX_LAYERS 15

#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL struct Categories cats[MAX_LAYERS];
GLOBAL int fd[MAX_LAYERS];
GLOBAL int nlayers;
GLOBAL char name[MAX_LAYERS][40];
GLOBAL char mapset[MAX_LAYERS][40];
