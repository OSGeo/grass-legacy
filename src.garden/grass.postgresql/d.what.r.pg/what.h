#include "gis.h"

#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL struct Categories cats;
GLOBAL int fd;
GLOBAL int dbCat;
GLOBAL char name[80];
GLOBAL char mapset[80];
