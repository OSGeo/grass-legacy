#include<stdio.h>
#include "define.h"
#include "global.h"

stream_order()

{
    extern int num_ele;

    int done;
    int ee;
    int ii;
    int loop;
/*
 *  Initialize counting variables.
 */
    ii = 3;
    loop = 1;
/*
 *  Write header for Kineros.
 */
    printf("\n");
    printf("\n");
    printf("\n *****************************************************************");
    printf("\n *****************************************************************");
    printf("\n Computational Order of Stream Segments:");
    printf("\n");
    printf("\n LOOP: %3d",loop);
/*
 *  Find all first-order streams.
 */
    printf("\n First order streams:");
    for(ee=3;ee<=num_ele;ee=ee+3) {
	if(element[ee].index > 0) {
	    if((element[ee].trib[0] == 0) && (element[ee].trib[1] == 0)) {
	        element[ee].order = ii;
	        printf("\n Order: %4d",ii);
	        printf("   Stream:%4d",ee);
	        printf("   Tributary 0:%4d",element[ee].trib[0]);
	        printf("   Tributary 1:%4d",element[ee].trib[1]);
	        ii=ii+3;
	    }
	    else 
	        element[ee].order = 0;
	}
    }
/*
 *  Find all streams in which the tributaries have already been
 *  printed out.
 */
    done = 0;
    while(!done) {
        loop++;
 	done = 1;
        printf("\n -----------------------------------------------------------------");
        printf("\n");
        printf("\n LOOP: %3d",loop);
        for(ee=3;ee<=num_ele;ee=ee+3) {
	    if(element[ee].index > 0) {
	        if(element[ee].order == 0) {
	            if((element[element[ee].trib[0]].order != 0) && 
	               (element[element[ee].trib[1]].order != 0)) {
		        done = 0;
		        element[ee].order = ii;
	                printf("\n Order: %4d",ii);
	                printf("   Stream:%4d",ee);
	                printf("   Tributary 0:%4d",element[ee].trib[0]);
	                printf("   Tributary 1:%4d",element[ee].trib[1]);
	                ii=ii+3;
	            }
	        }
	    }
	}
    }
}
