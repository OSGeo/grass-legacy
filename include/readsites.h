#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "site.h"

/*---------------------typedefs*/

struct zstruct
{
  double x, y, z;
};
typedef struct zstruct Z;
  
/*--------------------functions */
int G_readsites(FILE *,int,int,int, Z**);
