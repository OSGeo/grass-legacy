/******************************************************************************
 *
 *	prob_invsqr.c  inverse square distance distributed probability generator
 *	
 * Usage: prob_invsqr <max distance as an integer> <number of experiments>
 *
 * Notes: prob_invsqr() generates a decreasing probability distribution 
 * outward in an inverse square rate. It use three consecutive random number
 * generator: 
 *	applying it once gets an UNIFORM distribution in the range of 0-max_num;
 *	doing it twice gets a SIMPLE INVERSE distribuion in that range;
 *	doing time times gets a INVERSE SQUARE distribution.
 * 
 * Author: Jianping Xu, Rutgers University
 * Date: 06/11/1994
 ******************************************************************************/
#include <limits.h>
#include <math.h>
#include <sys/types.h>

int main(argc, argv)
int argc; char **argv;
{
    int max, num, A[100000], i;

    srand(getpid());
    max = atoi(argv[1]);
    num = atoi(argv[2]);
    i=0;
    while (i<num) {
	A[i] = (int)((max+.999999999999)*(float)rand()/INT_MAX); i++;
	if (i==num) break;
	A[i] = (int)((A[i-1]+0.99999999999)*(float)rand()/INT_MAX); i++;
	if (i==num) break; 
	A[i] = (int)((A[i-1]+0.99999999999)*(float)rand()/INT_MAX); i++;
    }
    for (i=0; i<num; i++) printf ("%d\n", A[i]); 
}
