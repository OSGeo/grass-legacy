#include "tape.h"

readbil (row,band)
    int row, band;
{
    int n;
    int r_skip;
    register int j;
    unsigned char *m;
    static int too_short = 0;

    G_zero (tapebuf, tapebufsize);
    n = read (tapefd, tapebuf, tapebufsize);

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
	    fprintf (stderr, "WARNING: data records shorter than expected\n");
	    fprintf (stderr, "        only found %d columns on the tape\n", n);
	}
    }
    if (wantband[band]) {
      if (first[band]) {
        r_skip = (row - 1) % blocking_factor;
        first[band] = 0;
      }
      else
	r_skip = 0;
      for (j=0; j<blocking_factor-r_skip; j++) {
	m = tapebuf + r_skip*ncols + j*ncols;
	put_row (bandfd[band], m+skipbytes+firstcol-1);
      }
    }
    return 1;
}
