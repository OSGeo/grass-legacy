/****************************************************************/
/* NAME: 	put_image					*/
/*								*/
/* FUNCTION:	organize the band file row by row		*/
/*								*/
/* USAGE:	put_image(row,band)				*/
/*								*/
/* INPUT:	row -- current row number			*/
/*		band -- current band				*/
/*								*/
/* OUTPUT:	0 -- error					*/
/*		1 -- success					*/
/****************************************************************/
#include "tape.h"

put_image (row,band,ok)
{
    static int too_short = 0;

    if (tape.n < 0)
    {
	fprintf(stderr,"\n");
	fprintf(stderr,"ERROR: bad read tape(row %d band %d)\n",tape.firstrow+row,band+1);
	fprintf(stderr,"      raster files are incomplete\n");
	return 0;
    }

    if (tape.n == 0)
    {
	fprintf(stderr,"\n");
	fprintf(stderr,"ERROR: only found %d rows on tape\n", row);
	fprintf(stderr,"      raster files will be incomplete\n");
	return 0;
    }

    if (tape.n < tape.lastcol)
    {
	if (!too_short)
	{
	    too_short = 1;
	    fprintf(stderr,"\n");
	    fprintf (stderr, "WARNING: data records shorter than expected\n");
	    fprintf (stderr, "        only found %d columns on the tape\n", tape.n);
	}
    }

    if (tape.wantband[band])
    {
	if (ok)
	{
 	  put_row (tape.band[band].fd, tape.tapebuf+IMAGE_DATA_START+tape.firstcol-1, row);
	}
	else
	{
  	  G_zero(tape.tapebuf, tape.tapebufsize);
  	  put_row (tape.band[band].fd, tape.tapebuf, row);
	}
    }
    return 1;

}
