#include <stdlib.h>
#include "gis.h"

int * G_alloc_ivector(int n)
{
    return (int *) G_calloc (n, sizeof(int));
}

int **G_alloc_imatrix(int rows,int cols)
{
    int **m;
    int i;

    m = (int **) G_calloc (rows, sizeof(int *));
    m[0] = (int *) G_calloc (rows*cols, sizeof(int));
    for (i = 1; i < rows; i++)
	m[i] = m[i-1] + cols;
    return m;
}

int G_free_ivector(int *v)
{
    free (v);

    return 0;
}

int G_free_imatrix(int **m)
{
    free (m[0]);
    free (m);

    return 0;
}
