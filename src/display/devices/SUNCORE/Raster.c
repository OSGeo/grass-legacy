#include "sun.h"
#include <stdio.h>

extern int SCREEN_BOTTOM ;

static struct
{
	int width, height, depth ;
	short *bits ;
} raster ;
static short alloc = 0 ;

Raster_int(num, nrows, array, withzeros, color_type)
	int num ;
	int nrows ;
	int *array ;
	int withzeros ;
	int color_type ;
{
	float beg_x_pos ;
	float beg_y_pos ;
	int cur_x ;
	int cur_y ;
	int *arr ;
	unsigned char *bit_ptr ;
	char *calloc() ;
	char *realloc() ;
	int c ;
	int i ;

	int (*assign_color)() ;
	int _get_color_index() ;
	int do_nothing() ;
	
	if(color_type)
		assign_color = _get_color_index ;
	else
		assign_color = do_nothing ;
	
/* Check to see if current x,y position is correct */
	Get_current_xy(&cur_x, &cur_y) ;
	if (sun_x != cur_x || sun_y != cur_y)
		move_abs_2((float)cur_x, (float)(SCREEN_BOTTOM - cur_y)) ;
	sun_x = 99999999, sun_y = 99999999 ; /* Note that we lose current x,y */

/* Allocate space for raster */
	if (alloc < num)
	{
		if (alloc == 0 )
			raster.bits = (short *)calloc(num/2+1, sizeof(short)) ;
		else
			raster.bits = (short *)realloc((char *)raster.bits, sizeof(short) * num/2+1) ;
		if (raster.bits == NULL)
			return(-1) ;
		alloc = num ;
	}

	raster.width  = num ;
	raster.height = 1 ;
	raster.depth  = 8 ;

	inquire_current_position_2(&beg_x_pos, &beg_y_pos) ;

/* If zeros are to be included, an entire raster row can be constructed */
	if (withzeros)
	{
		bit_ptr = (unsigned char *)raster.bits ;
		arr = array ;
		for(i=0;i<num;i++)
			*bit_ptr++ = (unsigned char)(*assign_color)(*arr++) ;

		for(i=0; i<nrows; i++)
		{
			move_abs_2(beg_x_pos, beg_y_pos - (float)i) ;
			put_raster(&raster) ;
		}
	}
/* If zeros are not include need to potentially draw many rasters */
	else
	{
		int start_col ;

		bit_ptr = (unsigned char *)raster.bits ;
		arr = array ;
		start_col = 0 ;
		raster.width  = 0 ;

		for(c=0;c<num;c++)
		{
			if (*arr == 0)
			{
				if(raster.width > 0)
				{
					for(i=0; i<nrows; i++)
					{
						move_abs_2(beg_x_pos + start_col, beg_y_pos - (float)i) ;
						put_raster(&raster) ;
					}
					bit_ptr = (unsigned char *)raster.bits ;
					raster.width  = 0 ;
					start_col = c ;
				}
				else
				{
					start_col++ ;
				}
			}
			else
			{
				*bit_ptr++ = (unsigned char)(*assign_color)(*arr) ;
				raster.width++ ;
			}
			arr++ ;
		}
	/* Print out any remaining data */
		if(raster.width > 0)
		{
			for(i=0; i<nrows; i++)
			{
				move_abs_2(beg_x_pos + start_col, beg_y_pos - (float)i) ;
				put_raster(&raster) ;
			}
		}
	}
}

static do_nothing(n)
{
	return(n) ;
}
