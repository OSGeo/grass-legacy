#include <stdio.h>
#include <sys/types.h>
#include "driver.h"

Raster_char(num, nrows, array, withzeros, color_type)
	int num ;
	int nrows ;
	unsigned char *array ;
	int withzeros ;
	int color_type ;
{
	char *malloc() ;
	char *realloc() ;
	static int array_alloc = 0 ;
	static int *int_array ;

/* Check integer array allocation */
	if(! array_alloc)
	{
		array_alloc = num ;
		int_array = (int *)malloc(array_alloc * sizeof(int)) ;
	}
	else
	{
		if (num > array_alloc)
		{
			array_alloc = num ;
			int_array = (int *)realloc((char *)int_array, num * sizeof(int)) ;
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
}

Raster_int_def(num, nrows, array, withzeros, color_type)
int num, nrows;
unsigned int *array;
int withzeros, color_type;
{
    register unsigned cur_color, *arr;
    register int npixles;
    int our_x, our_y;
    int (*ColorFunc) ();
    int Color();                /* GRASS driver color call - maps color */
    int SetXColor();            /* device color call (generally useful*/

    if (color_type)
        ColorFunc = Color;
    else
        ColorFunc = SetXColor;

    arr = array;
    cur_color = *array;
    (*ColorFunc) (cur_color);
    npixles = 0;
    our_x = cur_x;
    our_y = cur_y;
    while (--num) {
        if (*(++arr) != cur_color) {
            if (nrows == 1) {
                if (withzeros || cur_color)
                    Cont_rel(npixles, 0);
                else
                    Move_rel(npixles, 0);
                cur_x++;
            } else {
                if (withzeros || cur_color)
                    Box_abs2(our_x, our_y, npixles, nrows);
                /* Box_abs(our_x, our_y, our_x+npixles, our_y+nrows); */
                our_x += npixles;
            }
            our_x++;
            cur_color = *arr;
            (*ColorFunc) (cur_color);
            npixles = 0;
        } else
            npixles++;
    }
    if (nrows == 1) {
        if (withzeros || cur_color)
            Cont_rel(npixles, 0);
        else
            Move_rel(npixles, 0);
        cur_x++;
    } else {
        if (withzeros || cur_color)
            Box_abs2(our_x, our_y, npixles, nrows);
        cur_y = our_y + nrows;
        cur_x = our_y + npixles;
    }
}

/*** end Raster_def.c ***/
