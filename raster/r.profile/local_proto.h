#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "glocale.h"

/* main.c */
int do_profile(double, double, double, double, char *, int, double, int, int, FILE *, char *);

/* read_rast.c */
int read_rast(double, double, double, int, int, RASTER_MAP_TYPE, FILE *, char *);
/* input.c */
int input(char *, char *, char *, char *, char *);

int clr;
struct Colors colors;
