#include <string.h>
#include "gis.h"
#include "site.h"
#include "raster.h"
#include "display.h"

#ifndef GLOBAL
#define GLOBAL extern
#endif

#define MAXSITES 25000


/*
GLOBAL double SL_e[MAXSITES];
GLOBAL double SL_n[MAXSITES];
GLOBAL char *SL_desc[MAXSITES];
*/
GLOBAL char **site;
GLOBAL int nsites;
GLOBAL struct Cell_head *Wind;
GLOBAL int *Snum;
GLOBAL Site ***CurSites;


/* opensite.c */
int load_sites(int, struct Cell_head *, int);
int site_mem(Site *);
int compress_cached_site(Site *);
int free_cached_sites(void);
Site *closest_site(int, double, double);
/* show.c */
int show_buttons(int);
/* what.c */
int what(int, int, int, int);

