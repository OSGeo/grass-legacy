#include "gis.h"
#include "site.h"
#include "raster.h"
#include "display.h"


#ifndef GLOBAL
#define GLOBAL extern
#endif

#define MAXSITES 25000

GLOBAL double SL_e[MAXSITES];
GLOBAL double SL_n[MAXSITES];
GLOBAL char *SL_desc[MAXSITES];
GLOBAL int Snum;

/* opensite.c */
int open_sites(char *);
int load_sites(struct Cell_head *, int);
int site_mem(Site *);
int compress_cached_site(Site *);
int free_cached_sites(void);
Site *closest_site(double, double);
/* show.c */
int show_buttons(int);
/* what.c */
int what(int, int);
