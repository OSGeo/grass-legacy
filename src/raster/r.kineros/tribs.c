#include "define.h"
#include<stdio.h>

tribs(drain, direction, stream, rr, cc, home, trib)

int **drain;
int direction;
int **stream;
int rr;
int cc;
int home;
int trib[2];

{
    int neighbor;

    extern int ncols;
    extern int nrows;
    extern int index();
/*
 *  Check that we are not trying to look off the edge of the domain.
 *  If we are off the domain, then return.
 */
    if((rr < 0) || (rr >= nrows) || (cc < 0) || (cc >= ncols)) 
	return;
/*
 *  Check that the drainage direction is correct.
 */
    if(drain[rr][cc] != direction)
	return;
/*
 *  Compute the index of the neighboring cell.
 */
    neighbor = index(stream[rr][cc],STREAM);
/*
 *  If this point is not a stream then return.
 */
    if(neighbor <= 0)
	return;
/*
 *  If we are on the stream, and not a tributary, or if we are on a tributary
 *  that we have already found, then return.
 */
    if((neighbor == home) || (neighbor == trib[0]) || (neighbor == trib[1]))
	return;
/*
 *  Otherwise, assign the point as a tributary.
 */
    if(trib[0] > 0) {
        if(trib[1] > 0 ) {
	    printf("\n WARNING: more than 2 tributaries assigned to stream %d",home);
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
