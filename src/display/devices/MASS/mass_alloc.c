
#include <stdio.h>
static int *array_int ;
static int array_alloc = 0 ;

check_alloc_array(n, arr)
	int n ;
	int **arr ;
{
	char *realloc();
	char *malloc();
	int to_alloc ;

	if (n < array_alloc)
	{
		*arr = array_int ;
		return ;
	}

	to_alloc = array_alloc ;

	while (n > to_alloc)
		to_alloc += 512 ;
	
	if (! array_alloc)
	{
		array_int = (int *)malloc(to_alloc * sizeof(int)) ;
	}
	else
	{
		array_int = (int *)realloc((char *)array_int, to_alloc * sizeof(int)) ;
	}

	if(array_int != NULL)
	{
		array_alloc = to_alloc ;
		*arr = array_int ;
		return ;
	}
	
	fprintf(stderr,"ERROR: insufficient memory in check_alloc_array\n") ;
	exit(-1) ;
}
