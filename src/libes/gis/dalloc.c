#include "gis.h"

double *
G_alloc_vector(n)
    int n;
{
    return (double *) G_calloc (n, sizeof(double));
}

double **
G_alloc_matrix(rows, cols)
    int rows, cols;
{
    double **m;
    int i;

    m = (double **) G_calloc (rows, sizeof(double *));
    m[0] = (double *) G_calloc (rows*cols, sizeof(double));
    for (i = 1; i < rows; i++)
	m[i] = m[i-1] + cols;
    return m;
}

float *
G_alloc_fvector(n)
    int n;
{
    return (float *) G_calloc (n, sizeof(float));
}

float **
G_alloc_fmatrix(rows, cols)
    int rows, cols;
{
    float **m;
    int i;

    m = (float **) G_calloc (rows, sizeof(float *));
    m[0] = (float *) G_calloc (rows*cols, sizeof(float));
    for (i = 1; i < rows; i++)
	m[i] = m[i-1] + cols;
    return m;
}

G_free_vector(v)
    double *v;
{
    free (v);
}

G_free_matrix(m)
    double **m;
{
    free (m[0]);
    free (m);
}

G_free_fmatrix(m)
    float **m;
{
    free (m[0]);
    free (m);
}
