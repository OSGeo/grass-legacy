

#include	<stdio.h>
#include	"grid_structs.h"
#include	"dig_defines.h"


write_grid (fp_digit, grid_info)
	FILE  *fp_digit ;
	struct  grid_description  *grid_info ;
{

	int  i, k ;
	int  rows, cols ;
	int  num_v_rows, num_v_cols ;
	double  x,  y ;
	double  width,  length ;
	double  next_x,  next_y ;
	char  buffer[128] ;

	num_v_rows = grid_info->num_vect_rows ;
	num_v_cols = grid_info->num_vect_cols ;

	rows = grid_info->num_rows ;
	cols = grid_info->num_cols ;

	width = grid_info->width ;
	length = grid_info->length ;

/*  write out all the vector lengths (x vectors) of the entire grid  */

	printf("\n Writing out vector rows...") ;

	y = grid_info->origin_y ;
	for ( i = 0; i < num_v_rows; ++i)
	{
		x = grid_info->origin_x ;
		for (k = 0; k < cols; ++k)
		{
			next_x = x + length ;
			write_vect( fp_digit, x, y, next_x, y ) ;
			x = next_x ;
		}

		y += width ;
	}



/*  write out all the vector widths (y vectors) of the entire grid  */

	printf("\n Writing out vector columns...") ;

	x = grid_info->origin_x ;
	for ( k = 0; k < num_v_cols; ++k)
	{
		y = grid_info->origin_y ;
		for (i = 0; i < rows; ++i)
		{
			next_y = y + width ;
			write_vect( fp_digit, x, y, x, next_y ) ;
			y = next_y ;
		}

		x += length ;
	}

	return (0) ;


}


static  double  xarray[10] ; 
static  double  yarray[10] ; 

#define  NUM_POINTS  2

write_vect( fp_digit, x1, y1, x2, y2 )
	FILE  *fp_digit ;
	double  x1, y1, x2, y2 ;
{

	xarray[0] = x1 ;
	xarray[1] = x2 ;
	yarray[0] = y1 ;
	yarray[1] = y2 ;

	dig_Write_line( fp_digit, AREA, xarray, yarray, NUM_POINTS) ;
}

