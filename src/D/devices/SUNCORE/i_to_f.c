#include <stdio.h>
static int a_num = 0 ;
static float *xarray ;
static float *yarray ;

i_to_f_array(xarr, yarr, x, y, number)
	int *xarr ;
	int *yarr ;
	float **x ;
	float **y ;
	int number ;
{
	char *G_calloc(), *G_realloc() ;
	float *xf, *yf ;
	int *yi, *xi ;

	if (a_num == 0)
	{
		a_num = 1024 ;
		xarray = (float *) G_calloc(a_num, sizeof(float)) ;
		yarray = (float *) G_calloc(a_num, sizeof(float)) ;
	}
	if (a_num < number)
	{
		a_num += 512 ;
		xarray = (float *) G_realloc((char *)xarray, a_num * sizeof(float)) ;
		yarray = (float *) G_realloc((char *)yarray, a_num * sizeof(float)) ;
	}

	xi = xarr ;
	yi = yarr ;
	xf = xarray ;
	yf = yarray ;
	while(number--)
	{
		*(xf++) = (float)(*xi++) ;
		*(yf++) = (float)(*yi++) ;
	}

	*x = xarray ;
	*y = yarray ;
}

char *
G_calloc (m,n)
{
	char *buf;
	char *calloc();

	if (buf = calloc(m,n))
		return buf;

	G_fatal_error ("G_calloc: out of memory");
	return 0;
}

char *
G_realloc (buf,n)
	char *buf;
{
	char *malloc();
	char *realloc();

	if (!buf)
		buf = malloc (n);
	else 
		buf = realloc(buf,n);

	if (buf)
		return buf;

	G_fatal_error ("G_realloc: out of memory");
	return 0;
}

G_fatal_error (s)
	char *s ;
{
	fprintf(stderr,"%s\n", s) ;
	exit(-1) ;
}
