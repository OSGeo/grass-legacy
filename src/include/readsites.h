#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "site.h"

/*---------------------typedefs*/

typedef struct {
    double x,y;
    double z;
}site;

/*--------------------functions */
int readsites(FILE *,int,int,int, site**);

