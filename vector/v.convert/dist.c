#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "conv.h"
#include "local_proto.h"

double 
ldist (double x, double y, struct Line *p)
{
    int  i;
    double dist, ndist;
    
    i = (p->n_points == 1) ? 0 : 1;		
    dist = dig_distance2_point_to_line(x, y, p->x[0], p->y[0], p->x[i], p->y[i]);
    
    for (i=1; i < p->n_points - 1; i++){
        ndist = dig_distance2_point_to_line(x, y, p->x[i], p->y[i], 
			                         p->x[i+1], p->y[i+1]);
	
	if (ndist < dist){
	     dist = ndist;
	}
    }
    return (dist);	
}


