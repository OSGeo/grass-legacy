/* Function: Raster_chr		Paul W. Carlson		April 1990  */

#include "driver.h"

Raster_char(num, nrows, array, withzeros, color_type)
int num, nrows, withzeros, color_type;
unsigned char *array;
{
    register int x;
    register unsigned char *aptr;
    int color, prev_color, x1, y2, lastx;
	
    aptr = array;
    if (color_type)
    	for (x = 0; x < num; x++, aptr++) 
	     *aptr = (unsigned char)_get_color_index((int)(*aptr));

    if (nrows < 3 && withzeros) raster(cur_x, cur_y, nrows, num, array) ;
    else
    { 	prev_color = *array;
	x1 = cur_x;
	y2 = cur_y + nrows - 1;
	lastx = x1 + num - 1;
	aptr = array;
	for( x = cur_x; x < lastx; x++, aptr++)
	{   color = *aptr;
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
	color = *aptr;
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
