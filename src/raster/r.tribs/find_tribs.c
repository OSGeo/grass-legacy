#include<stdio.h>
#include<math.h>
#define DB_FIND_TRIBS 0

int *ivector();
int **imatrix();

find_tribs(nrows,ncols,accum,chann,aspect)

int nrows, ncols;

int **accum;
int **chann;
int **aspect;

{
    int rm1, rr, rp1;
    int cm1, cc, cp1;
    int absolute;
    int ch;
    int ii;
    int e0;

    int *aa;
    int *c0;
    int *r0;
    int *done;
    int **trib;

    int max_ch;
    int min_ch;
/*
 *  Allocate space for vectors.
 */
    aa = ivector(0,nrows*ncols);
    c0 = ivector(0,nrows*ncols);
    r0 = ivector(0,nrows*ncols);
    done = ivector(0,nrows*ncols);
    trib = imatrix(0,nrows*ncols,0,2);
/*
 *  Initialize the max and min index of channels.
 */
    max_ch = 2;
    min_ch = nrows*ncols*2;
/*
 *  Accumulations are negative numbers.  The highest point of a tributary
 *  should be the pixas with the maximum value.  Therefore initialize the 
 *  accumulation to a large negative number (which will later be reset to
 *  a smaller negative number.
 */
    for(ch=0;ch<nrows*ncols;ch++) {
	aa[ch]   = nrows*ncols*2;
	done[ch] = 0;
    }
/*
 *  Loop through all points to find the channel head.
 */
    for(rr=0;rr<nrows;rr++) {
	for(cc=0;cc<ncols;cc++) {

	    ch = chann[rr][cc];
	    max_ch = (ch < max_ch) ? max_ch : ch;
/*
 *  Identify that this is a point on a stream channel.
 */
	    if(ch>0) {
	        min_ch = (ch > min_ch) ? min_ch : ch;
/*
 *  Find the point on the segement where the minimum runoff is 
 *  accumulated.  This occurs where accum has the minimum absolute
 *  value.
 */
		absolute = (accum[rr][cc] < 0) ? -accum[rr][cc] : accum[rr][cc];
	        if(absolute < aa[ch]) {
		    aa[ch] = absolute;
		    c0[ch] = cc;
		    r0[ch] = rr;
		}
	    }
	}
    }

    printf("\n Max stream index = %d",max_ch);
    printf("\n Min stream index = %d",min_ch);
/*
 *  Loop through all channel heads.
 */
    for(ch=min_ch;ch<=max_ch;ch=ch+2) {

        printf("\n Stream: %4d    ",ch);
/*
 *  Indicies of channel heads
 */
	rr = r0[ch];
	cc = c0[ch];
/*
 *  Indicies of neighbors.
 */
	rp1 = rr+1;
	cp1 = cc+1;
	rm1 = rr-1;
	cm1 = cc-1;
/*
 *  Initialize tributary channels to index 0
 */
	trib[ch][0] = 0;
	trib[ch][1] = 0;
/*
 *  Compute which neighbors are channels given flow directions and
 *  the chann matrix, which locates which nodes are channels.
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
	if((rp1 < nrows) && (cp1 < ncols));
	    if(aspect[rp1][cp1] == 3)
	        neighbors(nrows, ncols, chann, rp1, cp1, ch, min_ch, trib[ch]);

	if(rp1 < nrows)
	    if(aspect[rp1][cc ] == 2)
	        neighbors(nrows, ncols, chann, rp1, cc , ch, min_ch, trib[ch]);

	if((rp1 < nrows) && (cm1 >= 0));
	    if(aspect[rp1][cm1] == 1)
	        neighbors(nrows, ncols, chann, rp1, cm1, ch, min_ch, trib[ch]);

	if(cp1 < ncols);
	    if(aspect[rr ][cp1] == 4)
	        neighbors(nrows, ncols, chann, rr,  cp1, ch, min_ch, trib[ch]);

	if(cm1 >= 0);
	    if(aspect[rr ][cm1] == 8)
	        neighbors(nrows, ncols, chann, rr,  cm1, ch, min_ch, trib[ch]);

	if((rm1 >= 0) && (cp1 < ncols))
	    if(aspect[rm1][cp1] == 5)
	        neighbors(nrows, ncols, chann, rm1, cp1, ch, min_ch, trib[ch]);

	if(rm1 >= 0)
	    if(aspect[rm1][cc ] == 6)
	        neighbors(nrows, ncols, chann, rm1, cc , ch, min_ch, trib[ch]);

	if((rm1 >= 0) && (cm1 >= 0))
	    if(aspect[rm1][cm1] == 7)
	        neighbors(nrows, ncols, chann, rm1, cm1, ch, min_ch, trib[ch]);

        printf("Tributary 0: %4d    ",trib[ch][0]);
        printf("Tributary 1: %4d    ",trib[ch][1]);

	for(ii=0;ii<2;ii++) {
	    if(trib[ch][ii] != 0) {
	        if(done[trib[ch][ii]] == 1) 
	            printf("\n WARNING: Segment %d already assigned as a tributary",trib[ch][ii]);
	        else
	            done[trib[ch][ii]] = 1;
	    }
	}

	if(DB_FIND_TRIBS)
	    debug(nrows,ncols,rm1,rr,rp1,cm1,cc,cp1,accum,chann,aspect);
    }
/*
 *  Print out streams in their proper computational order (i.e. such
 *  that data for trubutaries would be available).
 */
    stream_order(trib,min_ch,max_ch);
}
