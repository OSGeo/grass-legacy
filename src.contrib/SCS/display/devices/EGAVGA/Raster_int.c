/* Function: Raster_int		Paul W. Carlson		April 1990  */

#include "driver.h"

Raster_int(num, nrows, array, withzeros, color_type)
int num, nrows, withzeros, color_type;
unsigned int *array;
{
    register int x;
    register unsigned int *aptr;
    register unsigned char *rptr;
    int color, prev_color, x1, y2, lastx;
    unsigned char rbuff[640];
	
    aptr = array;
    rptr = rbuff;
    for (x = 0; x < num; x++, aptr++, rptr++) 
    {	if (color_type) *rptr = (unsigned char)_get_color_index(*aptr);
	else *rptr = (unsigned char)*aptr;
    }

    if (nrows < 3 && withzeros) raster(cur_x, cur_y, nrows, num, rbuff) ;
    else
    { 	prev_color = *rbuff;
	x1 = cur_x;
	y2 = cur_y + nrows - 1;
	lastx = x1 + num - 1;
	rptr = rbuff;
	for( x = cur_x; x < lastx; x++, aptr++)
	{   color = *rptr;
	    if (color != prev_color)
	    {   if (withzeros || prev_color)
	        {   put_chr('C');
		    put_int(prev_color);
		    put_chr('F');
		    put_int(x1);
		    put_int(cur_y);
		    put_int(x - 1);
		    put_int(y2);
		}
		prev_color = color;
		x1 = x;
	    }
	}
	color = *rptr;
	if (color == prev_color)
	{   if (withzeros || color)
	    {   put_chr('C');
	    	put_int(color);
	        put_chr('F');
		put_int(x1);
		put_int(cur_y);
		put_int(lastx);
		put_int(y2);
	    }
	}
	else
	{   if (withzeros || prev_color)
	    {   put_chr('C');
	    	put_int(prev_color);
	        put_chr('F');
		put_int(x1);
		put_int(cur_y);
		put_int(lastx - 1);
		put_int(y2);
	    }
	    if (withzeros || color)
	    {   put_chr('C');
	    	put_int(color);
	        put_chr('F');
		put_int(lastx);
		put_int(cur_y);
		put_int(lastx);
		put_int(y2);
	    }
	}	
    }
}
