/* %W% %G% */

/*
 *            6) Get distances from user
 */

#include <stdio.h>
#define PI 	3.141592

#include "disfrm.h"

getdists(resolution, dist) 
    double resolution ;
    int *dist ;
{
    int atleastone ;
    int incr ;
    int total_area = 0 ;
    int j ;
    int k ;
    int temp ;

    for(incr=0; incr<MAXDIST; incr++)
	dist[incr] = 0 ;

    V_clear() ;
    V_line (  1,
"              ENTER MAXIMUM DISTANCE FOR EACH CATEGORY DESIRED") ;
    V_line (  2,
"               (ALL VALUES IN CELLS.   1 CELL =          METERS)") ;
    V_line (  3,
"CAT  1: 0 to              CAT 21:   to              CAT 41:   to      ") ;
    V_line (  4,
"CAT  2:   to              CAT 22:   to              CAT 42:   to     ") ;
    V_line (  5,
"CAT  3:   to              CAT 23:   to              CAT 43:   to     ") ;
    V_line (  6,
"CAT  4:   to              CAT 24:   to              CAT 44:   to     ") ;
    V_line (  7,
"CAT  5:   to              CAT 25:   to              CAT 45:   to     ") ;
    V_line (  8,
"CAT  6:   to              CAT 26:   to              CAT 46:   to     ") ;
    V_line (  9,
"CAT  7:   to              CAT 27:   to              CAT 47:   to     ") ;
    V_line ( 10,
"CAT  8:   to              CAT 28:   to              CAT 48:   to     ") ;
    V_line ( 11,
"CAT  9:   to              CAT 29:   to              CAT 49:   to     ") ;
    V_line ( 12,
"CAT 10:   to              CAT 30:   to              CAT 50:   to     ") ;
    V_line ( 13,
"CAT 11:   to              CAT 31:   to              CAT 51:   to     ") ;
    V_line ( 14,
"CAT 12:   to              CAT 32:   to              CAT 52:   to     ") ;
    V_line ( 15,
"CAT 13:   to              CAT 33:   to              CAT 53:   to     ") ;
    V_line ( 16,
"CAT 14:   to              CAT 34:   to              CAT 54:   to     ") ;
    V_line ( 17,
"CAT 15:   to              CAT 35:   to              CAT 55:   to     ") ;
    V_line ( 18,
"CAT 16:   to              CAT 36:   to              CAT 56:   to     ") ;
    V_line ( 19,
"CAT 17:   to              CAT 37:   to              CAT 57:   to     ") ;
    V_line ( 20,
"CAT 18:   to              CAT 38:   to              CAT 58:   to     ") ;
    V_line ( 21,
"CAT 19:   to              CAT 39:   to              CAT 59:   to     ") ;
    V_line ( 22,
"CAT 20:   to              CAT 40:   to              CAT 60:   to     ") ;

    V_const(&resolution, 'd',  2, 48, 8) ;
    V_ques(&dist[ 0], 'i',  3, 13, 4) ;
    V_ques(&dist[ 1], 'i',  4, 13, 4) ;
    V_ques(&dist[ 2], 'i',  5, 13, 4) ;
    V_ques(&dist[ 3], 'i',  6, 13, 4) ;
    V_ques(&dist[ 4], 'i',  7, 13, 4) ;
    V_ques(&dist[ 5], 'i',  8, 13, 4) ;
    V_ques(&dist[ 6], 'i',  9, 13, 4) ;
    V_ques(&dist[ 7], 'i', 10, 13, 4) ;
    V_ques(&dist[ 8], 'i', 11, 13, 4) ;
    V_ques(&dist[ 9], 'i', 12, 13, 4) ;
    V_ques(&dist[10], 'i', 13, 13, 4) ;
    V_ques(&dist[11], 'i', 14, 13, 4) ;
    V_ques(&dist[12], 'i', 15, 13, 4) ;
    V_ques(&dist[13], 'i', 16, 13, 4) ;
    V_ques(&dist[14], 'i', 17, 13, 4) ;
    V_ques(&dist[15], 'i', 18, 13, 4) ;
    V_ques(&dist[16], 'i', 19, 13, 4) ;
    V_ques(&dist[17], 'i', 20, 13, 4) ;
    V_ques(&dist[18], 'i', 21, 13, 4) ;
    V_ques(&dist[19], 'i', 22, 13, 4) ;
    V_ques(&dist[20], 'i',  3, 39, 4) ;
    V_ques(&dist[21], 'i',  4, 39, 4) ;
    V_ques(&dist[22], 'i',  5, 39, 4) ;
    V_ques(&dist[23], 'i',  6, 39, 4) ;
    V_ques(&dist[24], 'i',  7, 39, 4) ;
    V_ques(&dist[25], 'i',  8, 39, 4) ;
    V_ques(&dist[26], 'i',  9, 39, 4) ;
    V_ques(&dist[27], 'i', 10, 39, 4) ;
    V_ques(&dist[28], 'i', 11, 39, 4) ;
    V_ques(&dist[29], 'i', 12, 39, 4) ;
    V_ques(&dist[30], 'i', 13, 39, 4) ;
    V_ques(&dist[31], 'i', 14, 39, 4) ;
    V_ques(&dist[32], 'i', 15, 39, 4) ;
    V_ques(&dist[33], 'i', 16, 39, 4) ;
    V_ques(&dist[34], 'i', 17, 39, 4) ;
    V_ques(&dist[35], 'i', 18, 39, 4) ;
    V_ques(&dist[36], 'i', 19, 39, 4) ;
    V_ques(&dist[37], 'i', 20, 39, 4) ;
    V_ques(&dist[38], 'i', 21, 39, 4) ;
    V_ques(&dist[39], 'i', 22, 39, 4) ;
    V_ques(&dist[40], 'i',  3, 65, 4) ;
    V_ques(&dist[41], 'i',  4, 65, 4) ;
    V_ques(&dist[42], 'i',  5, 65, 4) ;
    V_ques(&dist[43], 'i',  6, 65, 4) ;
    V_ques(&dist[44], 'i',  7, 65, 4) ;
    V_ques(&dist[45], 'i',  8, 65, 4) ;
    V_ques(&dist[46], 'i',  9, 65, 4) ;
    V_ques(&dist[47], 'i', 10, 65, 4) ;
    V_ques(&dist[48], 'i', 11, 65, 4) ;
    V_ques(&dist[49], 'i', 12, 65, 4) ;
    V_ques(&dist[50], 'i', 13, 65, 4) ;
    V_ques(&dist[51], 'i', 14, 65, 4) ;
    V_ques(&dist[52], 'i', 15, 65, 4) ;
    V_ques(&dist[53], 'i', 16, 65, 4) ;
    V_ques(&dist[54], 'i', 17, 65, 4) ;
    V_ques(&dist[55], 'i', 18, 65, 4) ;
    V_ques(&dist[56], 'i', 19, 65, 4) ;
    V_ques(&dist[57], 'i', 20, 65, 4) ;
    V_ques(&dist[58], 'i', 21, 65, 4) ;
    V_ques(&dist[59], 'i', 22, 65, 4) ;

    V_call() ;

/* Sort the distances */
    for (k=0; k<MAXDIST-1; k++)
    {
	if ( dist[k] > dist[k+1] )
	{
	    temp = dist[k+1] ;
	    for (j=k; j>=0 && dist[j]>temp; j--)
		dist[j+1] = dist[j] ;
	    dist[j+1] = temp ;
	}
    }

    atleastone = 0 ;
    for(incr=0; incr<MAXDIST; incr++)
	if(dist[incr])
	{
	    atleastone++ ;
	    total_area += dist[incr]*dist[incr] ;
	}
    return((int)(PI*total_area)) ;
}
