#include <stdio.h>

check_alloc(num_have, num_want, size, array)
	int *num_have ;
	int num_want ;
	int size ;
	char **array ;
{
	char *malloc(), *realloc() ;
	if (num_want < *num_have)
		return(0) ;
	
	if (*num_have == 0)
		*array = malloc(size * num_want) ;
	else
		*array = realloc(*array, size * num_want) ;

	if (*array == NULL)
	{
		printf("Error: Insufficient memory in check_alloc: %d\n", num_want) ;
		exit(-1) ;
	}
	else
		*num_have = num_want ;
	return(0) ;
}
