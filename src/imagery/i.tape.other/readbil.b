#include "tape.h"

readbil (row,band)
{
    int n;
    static int too_short = 0;

    G_zero (tapebuf, lastcol);
    n = read (tapefd, tapebuf, lastcol);
    if (n < 0)
    {
	fprintf(stderr,"\n");
	fprintf(stderr,"ERROR bad tape read (row %d band %d)\n",firstrow+row,band+1);
	fprintf(stderr,"      raster files will be incomplete\n");
	return 0;
    }

    if (n == 0)
    {
	fprintf(stderr,"\n");
	fprintf(stderr,"ERROR only found %d rows on tape\n", firstrow+row);
	fprintf(stderr,"      raster files will be incomplete\n");
	return 0;
    }

    if (n < lastcol)
    {
	if (!too_short)
	{
	    too_short = 1;
	    fprintf(stderr,"\n");
	    fprintf (stderr, "WARNING data records shorter than expected\n");
	    fprintf (stderr, "        only found %d columns on the tape\n", n);
	}
    }
    if (wantband[band])
	put_row (bandfd[band], tapebuf+firstcol-1, row);
    return 1;
}
