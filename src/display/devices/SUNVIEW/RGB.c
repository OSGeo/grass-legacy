/******************************************************************************
 * These routines support the drawing of multi-band images on the graphics
 * device.  A color lookup table with equal divisions in the red, green, and
 * blue dimensions is created for this purpose.
 *
 * The user sends a red, green, and blue intensity ramp for 256 levels each
 * using the Set_RGB_color() routine.  Subsequent calls to RGB_raster uses
 * this information to convert RGB intensity rasters to a color raster.
 * This is then sent to the routine Raster_int().
 *
 * All intensity values are represented in unsigned (8-byte) values.  That is
 * with values between and including 0 and 255.
 *
 ******************************************************************************
 * Set_RGB_color(r,g,b)
 *     unsigned char r[256], g[256], b[256] ;
 * This contains the desired intensity functions for red, green, and blue.
 * Using the known number of available levels static arrays are filled with
 * which provide easy determination of which real color is associated with
 * any given RGB color intensity cmbination.
 *
 ******************************************************************************
 * RGB_raster(n, nrows, r, g, b, withzeros)
 *     int n ;
 *     int nrows ;
 *     unsigned char *r, *g, *b ;
 *     int withzeros ;
 * Generates a color Raster_int() call based on the red, grn, and blu array
 * information and the intensity function provided in the last Set_RGB_color()
 * call.
 ******************************************************************************
 */

#include <stdio.h>

static unsigned char red[256], grn[256], blu[256] ;

Set_RGB_color(r,g,b)
	unsigned char r[256], g[256], b[256] ;
{
	unsigned char *R, *G, *B ;
	unsigned char *RED, *GRN, *BLU ;
	int i ;

	R = r; G = g; B = b ;
	RED = red; GRN = grn; BLU = blu ;
	for(i=0; i<256; i++)
	{
		*RED++ = *R++ ;
		*GRN++ = *G++ ;
		*BLU++ = *B++ ;
	}
}

RGB_raster(n, nrows, r, g, b, withzeros)
	int n ;
	int nrows ;
	register unsigned char *r, *g, *b ;
	int withzeros ;
{
	static unsigned char *left_over_red = NULL ;
	static unsigned char *left_over_grn = NULL ;
	static unsigned char *left_over_blu = NULL ;
	static int *array = NULL ;
	static int array_alloc = 0 ;
	int to_alloc ;

	if (n > array_alloc)
	{
	/*  Make sure there is enough space  */
		while (n > array_alloc)
			array_alloc += 512 ;
		
	/* Make sure sufficient space is allocated */
		if (array == NULL)
		{
			array = (int *)malloc(array_alloc * sizeof(int)) ;
			left_over_red = (unsigned char *)malloc(array_alloc) ;
			left_over_grn = (unsigned char *)malloc(array_alloc) ;
			left_over_blu = (unsigned char *)malloc(array_alloc) ;
		}
		else
		{
			array = (int *)realloc((char *)array, array_alloc * sizeof(int)) ;
			left_over_red = (unsigned char *)realloc((char *)left_over_red, array_alloc) ;
			left_over_grn = (unsigned char *)realloc((char *)left_over_grn, array_alloc) ;
			left_over_blu = (unsigned char *)realloc((char *)left_over_blu, array_alloc) ;
		}
		if( (array == NULL) ||
			(left_over_red == NULL) ||
			(left_over_grn == NULL) ||
			(left_over_blu == NULL) )
		{
			fprintf(stderr,"ERROR: insufficient memory in RGB_raster\n") ;
			exit(-1) ;
		}
		else
		{
			register unsigned char *i ;
			int j ;
			for(j=0,i=left_over_red;j<array_alloc;j++,i++)
				*i = NULL ;
			for(j=0,i=left_over_grn;j<array_alloc;j++,i++)
				*i = NULL ;
			for(j=0,i=left_over_blu;j<array_alloc;j++,i++)
				*i = NULL ;
		}
	}

/* Convert RGB to color number */
	{
		register int i ;
		register int *a ;

		a = array ;

		for(i=0; i<n; i++)
		{
			if(i)
			{
				left_over_red[i] += left_over_red[i-1] ;
				left_over_grn[i] += left_over_grn[i-1] ;
				left_over_blu[i] += left_over_blu[i-1] ;
			}
			*a++ = _get_lookup_for_color_with_leftover (
				red[*r++], grn[*g++], blu[*b++],
				&left_over_red[i],
				&left_over_grn[i],
				&left_over_blu[i]) ;
		}
	}

	Raster_int(n, nrows, array, withzeros, 0) ;
}
