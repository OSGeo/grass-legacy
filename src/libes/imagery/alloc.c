#include <stdio.h>
#include <stdlib.h>
#include "imagery.h"

void *I_malloc(int n)
{
    void *b;

    b = malloc (n);
    if (b == NULL)
	fprintf (stderr, "Out of Memory\n");
    return b;
}

void *I_realloc(
    void *b,
    int n)
{
    b = realloc (b,n);
    if ((char *)b == NULL)
	fprintf (stderr, "Out of Memory\n");
    return b;
}

int I_free(void *b)
{
    if ((char *)b != NULL)
	free (b);

	return 0;
}

double **I_alloc_double2(int a,int b)
{
    double **x;
    int i,n;

    x = (double **) I_malloc ((a+1) * sizeof(double *));
    if (x == NULL)
	return (double **) NULL;
    for (i = 0; i < a; i++)
    {
	x[i]= (double *) I_malloc (b * sizeof(double));
	if (x[i] == NULL)
	{
	    for (n=0; n<i; n++)
		free (x[n]);
	    free(x);
	    return (double **) NULL;
	}
	else
	    for (n=0; n<b; n++)
		x[i][n]=0;
    }
    x[a] = NULL;
    return x;
}

int *I_alloc_int(int a)
{
    int *x;
    int i;

    x = (int *) I_malloc (a * sizeof(int));
    if (x == NULL)
	return (int *) NULL;
    for (i = 0; i < a; i++)
	x[i] = 0;
    return x;
}

int **I_alloc_int2 (int a,int b)
{
    int **x;
    int i,n;

    x = (int **) I_malloc ((a+1) * sizeof(int *));
    if (x == NULL)
	return (int **) NULL;
    for (i = 0; i < a; i++)
    {
	x[i]= (int *) I_malloc (b * sizeof(int));
	if (x[i] == NULL)
	{
	    for (n=0; n<i; n++)
		free (x[n]);
	    free(x);
	    return (int **) NULL;
	}
	else
	    for (n=0; n<b; n++)
		x[i][n]=0;
    }
    x[a] = NULL;
    return x;
}

int I_free_int2(int **x)
{
    int i;

    if (x != NULL)
    {
	for (i=0; x[i] != NULL; i++)
	    free (x[i]);
	free (x);
    }

	return 0;
}

int I_free_double2(double **x)
{
    int i;

    if (x != NULL)
    {
	for (i=0; x[i] != NULL; i++)
	    free (x[i]);
	free (x);
    }

	return 0;
}

double ***I_alloc_double3 (int a,int b,int c)
{
    double ***x;
    int i,n;

    x = (double ***) malloc ((a+1) * sizeof (double **));
    if (x == NULL)
	return x;
    for (i = 0; i < a; i++)
    {
	x[i] = I_alloc_double2 (b,c);
	if (x[i] == NULL)
	{
	    for (n=0; n<i; n++)
		free (x[n]);
	    free(x);
	    return (double ***) NULL;
	}
    }
    x[a] = NULL;

    return x;
}

int I_free_double3 (double ***x)
{
    int i;

    if (x != NULL)
    {
	for (i = 0; x[i] != NULL; i++)
	    I_free_double2 (x[i]);
	free (x);
    }

	return 0;
}
