#include "gis.h"

#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL struct Categories cats;
GLOBAL int fd;
GLOBAL int dbCat;
GLOBAL char name[80];
GLOBAL char mapset[80];

int getAllOpts (int, char **);
int getSelectOpts (int, char **);
int buildInfxQry(struct Option *,struct Option *, int);
int runInfxQry(char *, char *);
int opencell (char *, char *);
int getCat (char *);
