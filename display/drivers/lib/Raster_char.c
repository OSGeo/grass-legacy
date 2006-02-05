#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "driver.h"
#include "driverlib.h"

void COM_Raster_char(int num, int nrows, const unsigned char *array, int withzeros, int color_type)
{
	static size_t array_alloc;
	static int *int_array;
	int i;

	if (num > array_alloc)
	{
		array_alloc = num + 100;
		int_array = G_realloc(int_array, array_alloc * sizeof(int));
	}
		
	for (i = 0; i < num; i++)
		int_array[i] = array[i];
	    
	COM_Raster_int(num, nrows, int_array, withzeros, color_type);
}

