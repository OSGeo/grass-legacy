#include <stdio.h>


void nrerror(msg)
    char *msg;
{
    fprintf (stderr, "ERROR: %s\n", msg);
    exit(1);
}

static char *xalloc(n,size)
{
    char *ptr;
    char *calloc();

    ptr = calloc (n,size);
    if (ptr == NULL) nrerror ("Out of Memory");
    return ptr;
}

int *ivector(len)
{
    return (int *) xalloc (len, sizeof(int));
}

int **imatrix(rows, cols)
{
    int **m;
    int i;

    m = (int **) xalloc (rows, sizeof(int *));
    m[0] = (int *) xalloc (rows*cols, sizeof(int));

    for (i = 1; i < rows; i++)
	m[i] = m[i-1] + cols;
    
    return m;
}

double *vector(len)
{
    return (double *) xalloc (len, sizeof(double));
}

double **matrix(rows, cols)
{
    double **m;
    int i;

    m = (double **) xalloc (rows, sizeof(double *));
    m[0] = (double *) xalloc (rows*cols, sizeof(double));

    for (i = 1; i < rows; i++)
	m[i] = m[i-1] + cols;
    
    return m;
}

free_ivector(v)
    int *v;
{
    free(v);
}

free_imatrix(m)
    int **m;
{
    free(m[0]);
    free(m);
}

free_vector(v)
    double *v;
{
    free(v);
}

free_matrix(m)
    double **m;
{
    free(m[0]);
    free(m);
}
