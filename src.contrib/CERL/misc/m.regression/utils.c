#include <stdio.h>


void 
nrerror (char *msg)
{
    fprintf (stderr, "ERROR: %s\n", msg);
    exit(1);
}

static char *
xalloc (int n, int size)
{
    char *ptr;
    char *calloc();

    ptr = calloc (n,size);
    if (ptr == NULL) nrerror ("Out of Memory");
    return ptr;
}

int *
ivector (int len)
{
    return (int *) xalloc (len, sizeof(int));
}

int **
imatrix (int rows, int cols)
{
    int **m;
    int i;

    m = (int **) xalloc (rows, sizeof(int *));
    m[0] = (int *) xalloc (rows*cols, sizeof(int));

    for (i = 1; i < rows; i++)
	m[i] = m[i-1] + cols;
    
    return m;
}

double *
vector (int len)
{
    return (double *) xalloc (len, sizeof(double));
}

double **
matrix (int rows, int cols)
{
    double **m;
    int i;

    m = (double **) xalloc (rows, sizeof(double *));
    m[0] = (double *) xalloc (rows*cols, sizeof(double));

    for (i = 1; i < rows; i++)
	m[i] = m[i-1] + cols;
    
    return m;
}

int 
free_ivector (int *v)
{
    free(v);
}

int 
free_imatrix (int **m)
{
    free(m[0]);
    free(m);
}

int 
free_vector (double *v)
{
    free(v);
}

int 
free_matrix (double **m)
{
    free(m[0]);
    free(m);
}
