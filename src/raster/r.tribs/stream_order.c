#include<stdio.h>

int *ivector();

stream_order(trib,min_ch,max_ch)

int **trib;
int max_ch;
int min_ch;

{
    int ch;
    int ii;
    int *order;
    int loop;
/*
 *  Allocate space for the vector order.
 */
    order = ivector(0,max_ch);
/*
 *  Initialize counting variables.
 */
    ii = 0;
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
    for(ch=min_ch;ch<=max_ch;ch=ch+2) {
	order[ch] = -1;
	if((trib[ch][0] == 0) && (trib[ch][1] == 0)) {
	    order[ch] = ii;
	    printf("\n Order: %4d",ii);
	    printf("   Stream:%4d",ch);
	    printf("   Tributary 0:%4d",trib[ch][0]);
	    printf("   Tributary 1:%4d",trib[ch][1]);
	    ii++;
	}
    }
/*
 *  Find all streams in which the tributaries have already been
 *  printed out.
 */
    while(ii < (max_ch/2)) {
        loop++;
        printf("\n -----------------------------------------------------------------");
        printf("\n");
        printf("\n LOOP: %3d",loop);
        for(ch=min_ch;ch<=max_ch;ch=ch+2) {
	    if(order[ch] == -1) {
	        if((order[trib[ch][0]] != -1) && (order[trib[ch][1]] != -1)) {
		    order[ch] = ii;
	            printf("\n Order: %4d",ii);
	            printf("   Stream:%4d",ch);
	            printf("   Tributary 0:%4d",trib[ch][0]);
	            printf("   Tributary 1:%4d",trib[ch][1]);
	            ii++;
	        }
	    }
	}
    }
}
