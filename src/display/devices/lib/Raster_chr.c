#include <stdio.h>
#include <stdlib.h>
#include "driver.h"
#include "driverlib.h"

int 
Raster_char (int num, int nrows, unsigned char *array, int withzeros, int color_type)
{
	static int array_alloc = 0 ;
	static int *int_array ;

/* Check integer array allocation */
	if(! array_alloc)
	{
		array_alloc = num ;
		int_array = (int *)G_malloc((size_t) (array_alloc * sizeof(int))) ;
	}
	else
	{
		if (num > array_alloc)
		{
			array_alloc = num ;
			int_array = (int *)realloc((void *)int_array, (size_t)(num * sizeof(int))) ;
		}
	}

/* Copy char array to integer array */
	if (int_array == NULL)
	{
		fprintf(stderr, "ERROR: insufficient memory in Raster_char\n") ;
		exit(-1) ;
	}
		
	{
		register int i ;
		register int *iptr ;
		register unsigned char *cptr ;
		iptr = int_array ;
		cptr = array ;
		i = num ;
		while(i--)
			*(iptr++) = *(cptr++) ;
	}
	    
/* Call Raster_int */
	Raster_int(num, nrows, int_array, withzeros, color_type) ;

	return 0;
}
