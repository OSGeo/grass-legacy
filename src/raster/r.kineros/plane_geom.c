#include <stdio.h>
#include "define.h"
#include "global.h"

plane_geom(slopes, half_basin)

float **slopes;
int **half_basin;

{
    int cc, ee, rr;

    float *num;
    float *sum;

    num = fvector(0,num_ele);
    sum = fvector(0,num_ele);
/*
 *  Initialize.  In GRASS planes do not have any lateral contributions
 *  from other planes or channels.
 */
    for(ee=1;ee<=num_ele;ee++) {
	num[ee] = 0.0;
	sum[ee] = 0.0;
    }

    for(rr=0;rr<nrows;rr++)  {
        for(cc=0;cc<ncols;cc++)  {

	    ee = index(half_basin[rr][cc],PLANE);
/*
 *  Average slope.  Note that element[ee].slope was initialized in 
 *  find_stream.c
 */
	    if(ee > 0) {
/*
 *	        element[ee].slope = element[ee].slope + slopes[rr][cc];
 */
		sum[ee] = sum[ee] + slopes[rr][cc];
		num[ee] = num[ee] + 1.0;
	    }
	}
    }

    for(ee=1;ee<=num_ele;ee++) {
/*
 *  Stream segments have lateral planes.  Plane segements do not.
 *  Compute slopes and widths for plane elements only.
 */
	if((ee%3) != 0) {
	    element[ee].slope = sum[ee]/num[ee];
	    element[ee].length = res*res*num[ee]/element[ee].width;
	    printf("\n plane slope = %f",element[ee].slope);
	    printf("  num = %f",num[ee]);
        }
/*
 *  Characteristic length:
 */
	char_length = (char_length > element[ee].length) 
                        ?  char_length : element[ee].length;
	char_length = (char_length > element[ee].width) 
                        ?  char_length : element[ee].width;
    }
}
