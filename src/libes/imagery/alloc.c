#include <stdio.h>
char *
I_malloc(n)
{
    char *b;
    char *malloc();

    b = malloc (n);
    if (b == NULL)
	fprintf (stderr, "Out of Memory\n");
    return b;
}

char *
I_realloc(b,n)
    char *b;
{
    char *realloc();

    b = realloc (b,n);
    if (b == NULL)
	fprintf (stderr, "Out of Memory\n");
    return b;
}

I_free (b)
    char *b;
{
    if (b != NULL)
	free (b);
}

double **
I_alloc_double2 (a,b)
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

int *
I_alloc_int(a)
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

int **
I_alloc_int2 (a,b)
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

I_free_int2(x)
    int **x;
{
    int i;

    if (x != NULL)
    {
	for (i=0; x[i] != NULL; i++)
	    free (x[i]);
	free (x);
    }
}

I_free_double2(x)
    double **x;
{
    int i;

    if (x != NULL)
    {
	for (i=0; x[i] != NULL; i++)
	    free (x[i]);
	free (x);
    }
}

double ***
I_alloc_double3 (a,b,c)
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

I_free_double3 (x)
    double ***x;
{
    int i;

    if (x != NULL)
    {
	for (i = 0; x[i] != NULL; i++)
	    I_free_double2 (x[i]);
	free (x);
    }
}
