#ifndef _SITES_H_
#define _SITES_H_

#include "gis.h"

typedef struct {double x; double y; char *desc;} Site;

int open_sites(char *sname);

int load_sites(struct Cell_head *wind, int verbose);

void free_sites(void);

Site *closest_site(double pick_e, double pick_n);

char *site_format(Site *s);

#endif
