#include<stdio.h>
#include "define.h"
#include "global.h"

find_stream(accum, dem, stream, drain)

int **accum;
int **dem;
int **drain;
int **stream;


{
    char string[80];

    int absolute;
    int cc;
    int rr;
    int ee;

    int *acc_head;
    int *acc_mouth;
    int *col_mouth;
    int *dem_head;
    int *dem_mouth;
    int *row_mouth;
/*
 *  Allocate space for vectors.
 */
    acc_head = ivector(0,num_ele+1);
    col_head = ivector(0,num_ele+1);
    dem_head = ivector(0,num_ele+1);
    row_head = ivector(0,num_ele+1);

    acc_mouth = ivector(0,num_ele+1);
    col_mouth = ivector(0,num_ele+1);
    dem_mouth = ivector(0,num_ele+1);
    row_mouth = ivector(0,num_ele+1);
/*
 *  Accumulations are negative numbers.  The highest point of a tributary
 *  should be the pixas with the smallest absolute value.  Therefore 
 *  initialize the accumulation at the head of the stream to a large 
 *  number.
 */
    for(ee=1;ee<=num_ele;ee++) {
	acc_head[ee] = nrows*ncols*2;
	acc_mouth[ee] = 0;
	dem_head[ee] = -1.0;
	dem_mouth[ee] = -1.0;
    }
/*
 *  Loop through all points to find the stream head.
 */
    for(rr=0;rr<nrows;rr++) {
	for(cc=0;cc<ncols;cc++) {
	    ee = index(stream[rr][cc],STREAM);
/*
 *  Identify that this is a point on a stream.
 */
	    if(stream[rr][cc] > 0) {
/*
 *  Compute stream length.  Adjust distance if drainage follows a diagonal
 *  path across the cell (drain is odd number).  Lengths were initialized 
 *  to zero in initialize().
 */
		if(drain[rr][cc] % 2)
		    element[ee].length = element[ee].length + res;
		else
		    element[ee].length = element[ee].length + res_diag;
/*
 *  Find the point on the segement where the minimum runoff is 
 *  accumulated.  This occurs where accum has the minimum absolute
 *  value.
 */
		absolute = (accum[rr][cc] < 0) ? -accum[rr][cc] : accum[rr][cc];
	        if(absolute < acc_head[ee]) {
		    acc_head[ee] = absolute;
		    dem_head[ee] = dem[rr][cc];
		    col_head[ee] = cc;
		    row_head[ee] = rr;
		}
		if(absolute > acc_mouth[ee]) {
		    acc_mouth[ee] = absolute;
		    dem_mouth[ee] = dem[rr][cc];
		    col_mouth[ee] = cc;
		    row_mouth[ee] = rr;
		}
	    }
	}
    }

    for(ee=3;ee<=num_ele;ee=ee+3) {
	if(element[ee].index > 0) {
            element[ee].slope = (float)(dem_head[ee]-dem_mouth[ee]);
	    element[ee].slope = element[ee].slope/element[ee].length;

	    strcpy(string," ");
	    if(element[ee].slope < 0.0) {
		strcpy(string," PIT - RESET TO MIN_SLOPE");
		element[ee].slope = MIN_SLOPE;
	    }
	    else if(element[ee].slope < MIN_SLOPE) {
		strcpy(string," RESET TO MIN_SLOPE");
		element[ee].slope = MIN_SLOPE;
	    }

	    printf("\n element = %3d",ee);
/*
 *  Widths of associated planes are the same as the stream lengths.
 */
	    element[ee-1].width = element[ee].length;
	    element[ee-2].width = element[ee].length;

	    printf(" head = %d",dem_head[ee]);
	    printf(" mouth = %d",dem_mouth[ee]);
	    printf(" length = %f",element[ee].length);
	    printf(" slope = %f %s",element[ee].slope, string);
	}
    }
}
