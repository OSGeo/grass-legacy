#ifndef V_CIRCLE_H
#define V_CIRCLE_H
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include "Vect.h"
#include "gis.h"
#include "site.h"

#define DEBUG

/*---------------------typedefs*/

typedef struct {
    double x,y;
    double z;
}circlesite;




/*--------------------functions */

/*support.c*/
int Date(char *);
int readsites(FILE *,int,int,int, circlesite**);

/*circling.c*/
int  circling (circlesite *  , int, struct Map_info*,double units );





#endif /*V_CIRCLE_H*/
