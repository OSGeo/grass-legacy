#ifndef V_BUBBLE_H
#define V_BUBBLE_H
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include "Vect.h"
#include "gis.h"

#define DEBUG

/*---------------------typedefs*/

typedef struct {
    double x,y;
    double z;
}bubblesite;




/*--------------------functions */

/*support.c*/
int Date(char *);
int readsites(FILE *,int,int,int, bubblesite**);

/*bubbling.c*/
int  bubbling (bubblesite *  , int, struct Map_info*,double units );





#endif /*V_BUBBLE_H*/
