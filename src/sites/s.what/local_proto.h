#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "site.h"
#include "raster.h"
#include "display.h"

#ifndef GLOBAL
#define GLOBAL extern
#endif

#define MAXSITES 25000


GLOBAL char **site;
GLOBAL int nsites;
GLOBAL int *Snum;
GLOBAL Site ***CurSites;


/* loadsites.c */
int load_sites(int, int);
int site_mem(Site *);
int compress_cached_site(Site *);
int free_cached_sites(void);
Site *closest_site(int, double, double);
/* what.c */
int what(double, double, int, int);

