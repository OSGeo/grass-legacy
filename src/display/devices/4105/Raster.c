#define ESC 033
#include "colors.h"
#include "graphics.h"

Raster(num, nrows, array)
	int num ;
	int nrows ;
	unsigned char *array ;
{
	extern int current_x_pos ;
	extern int current_y_pos ;
	register unsigned char cur_color ;
	register unsigned char *arr ;
	register int npixles ;
	int xarr[5], yarr[5] ;
	int orig_y ;
	int orig_x ;
	
/* Modify array to contain actual color call numbers */
	adjust_raster(num, array) ;

	arr = array ;
	R_color((int)(cur_color = *array)) ;
	npixles = 0 ;
	orig_y = current_y_pos ;
	orig_x = current_x_pos ;

	while(--num)
	{
		if (*(++arr) != cur_color)
		{
			xarr[0] = orig_x ;
			yarr[0] = orig_y ;
			xarr[1] = orig_x+npixles+1 ;
			yarr[1] = orig_y ;
			xarr[2] = orig_x+npixles+1 ;
			yarr[2] = orig_y+nrows ;
			xarr[3] = orig_x ;
			yarr[3] = orig_y+nrows ;
			Polygon_abs(xarr, yarr, 4) ;

			orig_x += npixles + 1 ;
			R_color((int)(cur_color = *arr)) ;
			npixles = 0 ;
		}
		else
		{
			npixles++ ;
		}
	}

	xarr[0] = orig_x ;
	yarr[0] = orig_y ;
	xarr[1] = orig_x+npixles ;
	yarr[1] = orig_y ;
	xarr[2] = orig_x+npixles ;
	yarr[2] = orig_y+nrows ;
	xarr[3] = orig_x ;
	yarr[3] = orig_y+nrows ;
	Polygon_abs(xarr, yarr, 4) ;
}

static
adjust_raster(num, array)
	int num ;
	unsigned char *array ;
{
	int n ;
	unsigned char *ptr ;
	extern int color_lookup[] ;

	for(n=0, ptr=array; n<num; n++, ptr++)
		*ptr = color_lookup[*ptr] ;
}

static
R_color(code)
	int code ;
{
	extern int CUR_COL ;
	char *encode() ;
	char *s ;

	if (code > 174)
		code = 174 ;
	if (code < 50)
		code = 50 ;
	s = encode(code) ;

/* Select pattern color */
	printf("%cMP%c%c%c", ESC, s[0],s[1],s[2]) ;
	CUR_COL = code ;
}
