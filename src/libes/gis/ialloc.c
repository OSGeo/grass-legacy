#include "gis.h"

int *
G_alloc_ivector(n)
    int n;
{
    return (int *) G_calloc (n, sizeof(int));
}

int **
G_alloc_imatrix(rows, cols)
    int rows, cols;
{
    int **m;
    int i;

    m = (int **) G_calloc (rows, sizeof(int *));
    m[0] = (int *) G_calloc (rows*cols, sizeof(int));
    for (i = 1; i < rows; i++)
	m[i] = m[i-1] + cols;
    return m;
}

G_free_ivector(v)
    int *v;
{
    free (v);
}

G_free_imatrix(m)
    int **m;
{
    free (m[0]);
    free (m);
}
