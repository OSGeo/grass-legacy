/*  @(#)m_mult.c	2.1  6/26/87  */
#include <stdio.h>

#define		N	3

/*
 * m_mult: matrix multiplication (return c = a * b)
 *  3x3 matric by 3x1 matric
 */

m_mult (a, b, c)
	double	a[N][N] ;
	double	b[N] ;
	double	c[N] ;
{
    register int i, j;

    for (i = 0; i < N; i++)
	 {
        c[i] = 0.0;

        for (j = 0; j < N; j++)
                c[i] += (a[i][j] * b[j]);
	 }

    return 1;
}
