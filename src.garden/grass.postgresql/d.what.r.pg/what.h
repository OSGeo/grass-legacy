#include "gis.h"

#ifndef GLOBAL
#define GLOBAL extern
#endif

#define QRY_LENGTH 1024

GLOBAL struct Categories cats;
GLOBAL int fd;
GLOBAL int dbCat;
GLOBAL char name[80];
GLOBAL char mapset[80];

int getAllOpts (int, char **);
int getSelectOpts (int, char **);
int buildPg(struct Option *,struct Option *, int, char *);
int runPg(char *, char *);
int opencell (char *, char *);
int getCat (char *);
