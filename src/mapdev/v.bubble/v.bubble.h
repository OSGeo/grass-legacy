#ifndef V_BUBBLE_H
#define V_BUBBLE_H
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include "Vect.h"
#include "gis.h"
#include "site.h"

/* #define DEBUG */


/*--------------------functions */

/*support.c*/
int Date(char *);

/*bubbling.c*/
int bubbling (SITE_XYZ *, int, struct Map_info*, double );

#endif /*V_BUBBLE_H*/
