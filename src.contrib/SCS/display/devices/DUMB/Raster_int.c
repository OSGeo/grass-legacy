/* %W%   %G% */
#include "driver.h"


Raster_int(num, nrows, array, withzeros, color_type)
int num ;
int nrows ;
unsigned int *array ;
int withzeros ;
int color_type ;
{
	register int n, x;
	register int color, prev_color;
	register int x1, y2, lastx;
	unsigned int *ptr;
	
	ptr = array;
	for (n = 0; n < num; n++, ptr++) *ptr = get_fixed_color(*ptr);

	if (nrows < 3) raster_int(cur_x, cur_y, nrows, num, array, withzeros) ;
	else
	{ 	prev_color = array[0];
		x1 = cur_x;
		y2 = cur_y + nrows - 1;
		lastx = x1 + num - 1;
		for(n= 0, x = cur_x; x < lastx; x++, n++)
		{	color = array[n];
			if (color != prev_color)
			{   if (withzeros || prev_color)
			    {   put_chr('F');
				put_int(x1);
				put_int(cur_y);
				put_int(x-1);
				put_int(y2);
				put_int(prev_color);
			    }
			    prev_color = color;
			    x1 = x;
			}
		}
		color = array[n];
		if (color == prev_color)
		{   if (withzeros || color)
		    {   put_chr('F');
			put_int(x1);
			put_int(cur_y);
			put_int(lastx);
			put_int(y2);
			put_int(color);
		    }
		}
		else
		{   if (withzeros || prev_color)
		    {   put_chr('F');
			put_int(x1);
			put_int(cur_y);
			put_int(lastx-1);
			put_int(y2);
			put_int(prev_color);
		    }
		    if (withzeros || color)
		    {	put_chr('F');
			put_int(lastx);
			put_int(cur_y);
			put_int(lastx);
			put_int(y2);
			put_int(color);
		    }
		}	
	}
	reset_window() ;
}

extern int SCREEN_LEFT ;
extern int SCREEN_RIGHT ;
extern int SCREEN_BOTTOM ;
extern int SCREEN_TOP ;
static
reset_window()
{
	put_chr('V') ;
	put_int(SCREEN_LEFT) ;
	put_int(SCREEN_TOP) ;
	put_int(SCREEN_RIGHT) ;
	put_int(SCREEN_BOTTOM) ;
}
