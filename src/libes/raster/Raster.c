#include "raster.h"
#include "graph.h"
#include <stdio.h>


/*!
 * \brief draw a raster
 *
 * Starting at the current position, the <b>num</b> colors represented
 * in the <b>raster</b> array are drawn for <b>nrows</b> consecutive pixel
 * rows.  The <b>withzero</b> flag is used to indicate whether 0 values are to
 * be treated as a color (1) or should be ignored (0). If ignored, those screen
 * pixels in these locations are not modified. This option is useful for graphic
 * overlays.
 *
 *  \param num
 *  \param nrows
 *  \param withzero
 *  \param ras raster
 *  \return int
 */

int R_raster(
	int num ,
	int nrows ,
	int withzero ,
	int *ras )
{
	register int z ;
	register int *ptr ;
	char *malloc(), *realloc();
	static int nalloc = 0 ;
	static unsigned char *chararray ;

	z=num ;
	ptr = ras ;

    /* Check to see if char buffer can hold the int values */
	while(z--)
	{
		register int xc;
		register unsigned char cc;

		xc = *ptr++;
		cc = xc;
		if (cc != xc)
		{
			R_raster_int(num, nrows, withzero, ras) ;
			return 0;
		}
	}

    /* If all one byte values, copy to char raster and call raster_char */
	if (! nalloc)
	{
		chararray = (unsigned char *)malloc(num) ;
		nalloc = num ;
	}
	else
	{
		if (num > nalloc)
		{
			chararray = (unsigned char *)realloc(chararray, num) ;
			nalloc = num ;
		}
	}
	if (chararray == NULL)
	{
	    fprintf (stderr, "out of memory");
	    return 0;
	}

	{
		register unsigned char *cptr ;
		cptr = chararray ;
		ptr = ras ;
		z = num ;
		while (z--)
			*(cptr++) = *(ptr++) ;
	}

	R_raster_char(num, nrows, withzero, chararray) ;

	return 0;
}

/*!
 * \brief Send arguments to the driver
 *
 * Sends arguments to the driver, preceded by the RASTER_CHAR opcode; 
 * the actual work is done by the driver. A raster drawing operation is
 * performed. The result is that a rectangular area of width <b>num</b> 
 * and height <b>nrows</b>, with its top-left corner at the current position,
 * is filled with <b>nrows</b> copies of the data pointed to by <b>ras</b>.
 *
 * \param num is the number of columns.
 * \param nrows is the number of rows to be drawn, all of which are identical
 *        (this is used for vertical scaling).
 * \param withzero should be true (non-zero) if zero pixels are to be drawn in
 *        color zero, false (zero) if they are to be transparent (i.e.
 *        not drawn).
 * \param ras should point to <b>num</b> bytes of data, which constitute the 
 *        pixels for a single row of a raster image.
 *
 * Example: to draw a byte-per-pixel image:
  \code
   unsigned char image[HEIGHT][WIDTH];

   for (y = 0; y < HEIGHT; y++)
   {
       R_move_abs(x_left, y_top + y);
       R_raster_char(WIDTH, 1, 1, image[y]);
   }
  \endcode
 *
 */

int R_raster_char(
	int num ,
	int nrows ,
	int withzero ,
	unsigned char  *ras )
{
	int z ;
	_send_ident(RASTER_CHAR) ;
	z = num ;
	_send_int(&z) ;
	z = nrows ;
	_send_int(&z) ;
	z = withzero ;
	_send_int(&z) ;
	_send_char_array(num,ras) ;

	return 0;
}

int R_raster_int(
	int num ,
	int nrows ,
	int withzero ,
	int  *ras )
{
	int z ;
	_send_ident(RASTER_INT) ;
	z = num ;
	_send_int(&z) ;
	z = nrows ;
	_send_int(&z) ;
	z = withzero ;
	_send_int(&z) ;
	_send_int_array(num,ras) ;

	return 0;
}
