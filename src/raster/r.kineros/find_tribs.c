#include<stdio.h>
#include<math.h>
#include "define.h"
#include "global.h"
#define DB_FIND_TRIBS 1

int *ivector();
int **imatrix();

find_tribs(accum,stream,drain,dem)

int **accum;
int **dem;
int **drain;
int **stream;

{
    int absolute;
    int cm1, cc, cp1;
    int ee;
    int ii;
    int rm1, rr, rp1;
    int ss;

    int *done;
/*
 *  Loop through all stream heads.
 */

    done = ivector(0,num_ele+1);
/*
 *  Initailize for all elements.
 */
    for(ee=1;ee<=num_ele;ee++)  {
	done[ee] = 0;
	element[ee].trib[0] = 0;
	element[ee].trib[1] = 0;
    }
/*
 *  Loop through stream elements (every third, starting from 2).
 */
    for(ee=3;ee<=num_ele;ee=ee+3) {

	if(element[ee].index > 0) {

            printf("\n Stream: %4d    ",ee);
/*
 *  Indicies of stream heads
 */
	    rr = row_head[ee];
	    cc = col_head[ee];
	    ss = index(stream[rr][cc],STREAM);
/*
 *  Indicies of neighbors.
 */
	    rp1 = rr+1;
	    cp1 = cc+1;
	    rm1 = rr-1;
	    cm1 = cc-1;
/*
 *  Compute which neighbors are streams given flow directions and
 *  the stream matrix, which locates which nodes are streams.
 *
 *  Flow directions.
 *  1 = se
 *  2 = s
 *  3 = sw
 *  4 = w
 *  5 = nw
 *  6 = n
 *  7 = ne
 *  8 = e
 */
	    tribs(drain, 3, stream, rp1, cp1, ss, element[ee].trib);
	    tribs(drain, 2, stream, rp1, cc , ss, element[ee].trib);
	    tribs(drain, 1, stream, rp1, cm1, ss, element[ee].trib);
	    tribs(drain, 4, stream, rr,  cp1, ss, element[ee].trib);
	    tribs(drain, 8, stream, rr,  cm1, ss, element[ee].trib);
	    tribs(drain, 5, stream, rm1, cp1, ss, element[ee].trib);
	    tribs(drain, 6, stream, rm1, cc , ss, element[ee].trib);
	    tribs(drain, 7, stream, rm1, cm1, ss, element[ee].trib);

	    for(ii=0;ii<2;ii++) {
	        if((element[ee].trib[ii] != 0) && (done[element[ee].trib[ii]] == 1))
	            printf("\n WARNING: Segment %d already assigned as a tributary",element[ee].trib[ii]);
	        else
	            done[element[ee].trib[ii]] = 1;
    
                printf("Tributary: %4d  ",element[ee].trib[ii]);
	    }

	    if(DB_FIND_TRIBS)
	        debug(nrows,ncols,rm1,rr,rp1,cm1,cc,cp1,accum,stream,drain,dem);
	}
    }
}
