#include "raster.h"
#include "graph.h"
#include <stdio.h>

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
