#include <stdio.h>
#include "define.h"
#include "global.h"

initialize(stream)

int **stream;

{
    int ee, ii, rr, cc;

    for(ee=1;ee<=num_ele;ee++) {
        element[ee].order = 0;
        element[ee].index = 0;
	for(ii=0;ii<3;ii++) 
            element[ee].plane[ii] = 0;
	for(ii=0;ii<2;ii++) 
            element[ee].trib[ii] = 0;
	if((ee%3) == 0)
	    element[ee].chan_shape = 1;
	else
	    element[ee].chan_shape = 0;
        element[ee].print = 1;
        element[ee].pond = 0;
        element[ee].print_rain = 0;
        element[ee].length = 0.0;
        element[ee].width = 0.0;
        element[ee].slope = 0.0;
	for(ii=0;ii<2;ii++) {
	    if((ee%3) == 0)
                element[ee].bank[ii] = 2.0;
	    else
                element[ee].bank[ii] = 0.0;
	}
        element[ee].stream_width = 0.0;
        element[ee].diameter = 0.0;
	if((ee%3) == 0)
            element[ee].mannings_n = 0.06;
	else
            element[ee].mannings_n = 0.13;
        element[ee].laminar_k = 0.0;
        element[ee].fmin = 0.0;
        element[ee].G = 0.0;
        element[ee].porosity = 0.0;
        element[ee].Sint = 0.0;
        element[ee].Smax = 0.0;
        element[ee].Rock = 0.0;
	if((ee%3) == 0)
            element[ee].recess = 0.0;
	else
            element[ee].recess = 0.0;
        element[ee].intercept = 0.0;
        element[ee].res_law = 0;
        element[ee].Cf = 0.0;
        element[ee].Cg = 0.0;
        element[ee].Ch = 0.0;
        element[ee].Co = 0.0;
        element[ee].d50 = 0.0;
        element[ee].rho_s = 0.0;
	if((ee%3) == 0)
            element[ee].pave = 0.0;
	else
            element[ee].pave = 0.0;
        element[ee].sigma_s = 0.0;
    }
/*
 *  Assign index to elements that have stream data.  If an index is not assigned
 *  to an element, the element will be disregared in calculations or printing.
 */
    for(rr=0;rr<nrows;rr++) {
        for(cc=0;cc<ncols;cc++) {

            if(stream[rr][cc] > 1) {
		ee = index(stream[rr][cc],STREAM);

		if(element[ee].index == 0) {
		    element[ee  ].index = stream[rr][cc];
		    element[ee-1].index = stream[rr][cc];
		    element[ee-2].index = stream[rr][cc] - 1;
		}
                else {
		    if(element[ee].index != stream[rr][cc]) {
		        printf("\n ERROR: invalid index for element %d",ee);
		        exit(0);
		    }
		}
	    }
        }
    }   

    printf("\n");
    for(ee=1;ee<=num_ele;ee++)
	printf("\n element[%4d].index = %4d",ee,element[ee].index);
    printf("\n");
    printf("\n");
}
