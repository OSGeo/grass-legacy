#include<stdio.h>

neighbors(nrows, ncols, chann, rr, cc, ch_cent, min, trib)

int rr, cc;
int nrows, ncols;
int **chann;
int ch_cent, min;
int trib[2];

{
    int neighbor;
/*
 *  Check that we are not trying to look off the edge of the domain.
 *  If we are off the domain, then return.
 */
    if((rr < 0) || (rr >= nrows) || (cc < 0) || (cc >= ncols)) 
	return;
    else
	neighbor = chann[rr][cc];
/*
 *  If this point is not a channel then return.
 */
    if(neighbor <= min)
	return;
/*
 *  If we are on the channel, and not a tributary, or if we are on a tributary
 *  that we have already found, then return.
 */
    if((neighbor == ch_cent) || (neighbor == trib[0]) || (neighbor == trib[1]))
	return;
/*
 *  Otherwise, assign the point as a tributary.
 */
    if(trib[0] > 0) {
        if(trib[1] > 0 ) {
	    printf("\n WARNING: more than 2 tributaries assigned to channel %d",ch_cent);
	    printf("\n          tributary 0 = %d",trib[0]);
	    printf("\n          tributary 1 = %d",trib[1]);
	    printf("\n          tributary 2 = %d",neighbor);
	    printf("\n                  ");
	}
        else
	    trib[1] = neighbor;
    }
    else
	trib[0] = neighbor;
}
