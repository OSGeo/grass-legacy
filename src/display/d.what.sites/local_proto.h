#include "gis.h"

/* ask.c */
GLOBAL char **site;
GLOBAL int nsites;

int what (int, int);
int show_buttons (int);
int open_sites(char *);
int load_sites (struct Cell_head *, int);
