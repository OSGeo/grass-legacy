#include <stdio.h>
#include <stdlib.h>

void pr_utdm_v (double *x, int N, int width, int precision)
     /* "print a upper triangular double matrix stored as a vector"
        The matrix is N x N, the vector has N*(N+1)/2 elements.
        Each element is formatted using fmt.
        There are no sanity checks at all. */
{
    int pos=0, i, j, leavespace;
    char s[100], fmt[100];      /* will be used in making fprintf (stdout,) formats */

    sprintf(fmt, "%%%d.%dg", width, precision);
    for (i=1; i<=N; i++) {
        leavespace = (i-1)*width;
        sprintf(s, "%%%ds", leavespace);
        fprintf (stdout,s, "");
        for (j=i; j<=N; j++) fprintf (stdout,fmt, x[pos++]);
        fprintf (stdout,"\n");
    }
}

double *
dvector (int l, int h)
{
    double *block;
    int size;

    size = h - l + 1;
    block = (double *) malloc(sizeof(double)*size);
    if (block == NULL) fprintf(stderr, "malloc failure in dvector()\n");
    return block-l;
}

float *
vector (int l, int h)
{
    float *block;
    int size;

    size = h - l + 1;
    block = (float *) malloc(sizeof(float)*size);
    if (block == NULL) fprintf(stderr, "malloc failure in dvector()\n");
    return block-l;
}

int *
ivector (int l, int h)
{
    int *block, size;

    size = h - l + 1;
    block = (int *) malloc(sizeof(int)*size);
    if (block == NULL) fprintf(stderr, "malloc failure in ivector()\n");
    return block-l;
}

float **
matrix (int rl, int rh, int cl, int ch)
{
    float *block;
    float **m;
    int size, i, rowsize, numrows;

    rowsize = ch - cl + 1;	/* #locations consumed by 1 row */
    numrows = rh -rl + 1;
    size = numrows*rowsize;
    block = (float *) malloc((unsigned) sizeof(float)*size);
    if (block == NULL) 
      fprintf(stderr, "malloc failure in matrix allocation\n");
    /* so we have the matrix. */

    /* Now for the row pointers */
    m = (float **) malloc((unsigned) sizeof(float *)*numrows);
    if (m == NULL) fprintf(stderr, "malloc failure in matrix allocation\n");
    m -= rl;	/* fixup m pointer so m[rl] == old m[0] */

    /* Finally, setup pointers to rows */
    block -= cl;
    for (i=rl; i<=rh; i++) {
        m[i] = block; block += rowsize;
    }
    return m;
}

double **
dmatrix (int rl, int rh, int cl, int ch)
{
    double *block;
    double **m;
    int size, i, rowsize, numrows;

    rowsize = ch - cl + 1;	/* #locations consumed by 1 row */
    numrows = rh -rl + 1;
    size = numrows*rowsize;
    block = (double *) malloc((unsigned) sizeof(double)*size);
    if (block == NULL) 
      fprintf(stderr, "malloc failure in matrix allocation\n");
    /* so we have the matrix. */

    /* Now for the row pointers */
    m = (double **) malloc((unsigned) sizeof(double *)*numrows);
    if (m == NULL) fprintf(stderr, "malloc failure in matrix allocation\n");
    m -= rl;	/* fixup m pointer so m[rl] == old m[0] */

    /* Finally, setup pointers to rows */
    block -= cl;
    for (i=rl; i<=rh; i++) {
        m[i] = block; block += rowsize;
    }
    return m;
}
