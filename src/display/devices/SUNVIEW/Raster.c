#include "stdio.h"
#include "graphics.h"

static unsigned char scanline[2048];

Raster_int(num, nrows, array, withzeros, color_type)
	int num ;
	int nrows ;
	unsigned int *array ;
	int withzeros ;
	int color_type ;
{
	unsigned char *pscan ;
	unsigned int *parray;
	int x;
	Rect r;
	int cur_x, cur_y ;
	Pixrect *pr = mem_point(2048, 1, 8, scanline);  /* hard wired - YCBM */

/* If zeros are to be ignored, send this (for now) to the
 * default raster which uses moves and cont(inues).
 */
	if (! withzeros)
	{
		Raster_int_def(num, nrows, array, withzeros, color_type) ;
		return ;
	}

/* get system color numbers if necessary */
	if (color_type)
	{
		_get_color_index_array(array, num) ;
		for(parray=array,x=num;x;parray++,x--)
			*parray = *parray + 1 ;
	}

/* copy integer array to character array */
	parray = array;
	pscan = scanline;
	for(x=0; x < num; x++)
		*pscan++ = *parray++ ;

/* Place array on screen */
	Get_current_xy(&cur_x, &cur_y) ;

	r.r_top = r.r_left = 0;
	r.r_height = r.r_width = 2000;

	pw_batch_on(pixwin);
	pw_lock(pixwin, &r);
	for(x = 0; x < nrows; x++)
		pw_rop(pixwin, cur_x, cur_y + x, num, 1, PIX_SRC, pr, 0, 0);
	pw_unlock(pixwin);
	pw_batch_off(pixwin);
}
