#include <stdio.h>
#include "tape.h"

readbsq (band)
{
    int row;
    int n;
    int fd ;
    static int too_short = 0;
    int stat ;
    int rec;
    int r_skip;
    register int j;
    unsigned char *p;

    stat = 0;
    fd = I_open_band_new (band);

    fprintf (stderr, "\nbandfile %s: skipping to row %d ...",I_bandname(band), 
		firstrow);
    fflush (stdout);

    rec = (firstrow-1)/blocking_factor;
    I_tape_advance (tapefd, rec);

    fprintf (stderr, "\nextracting ... "); fflush (stdout);
    for (row = 0; row < nrows; row+=blocking_factor)
    {
      G_percent (row, nrows, 2);
      n = read (tapefd, tapebuf, tapebufsize);
      if (n < 0)
      {
        fprintf(stderr, "\n");
        fprintf(stderr,"ERROR: bad tape read (row %d band %d)\n",
		firstrow+row,band);
	goto DONE;
      }

      if (n == 0)
      {
        fprintf(stderr, "\n");
        fprintf(stderr,"ERROR: only found %d rows on tape\n", firstrow+row-1);
	goto DONE;
      }

      if (n < lastcol)
      {
        if (!too_short)
        {
          too_short = 1;
	  fprintf(stderr, "\n");
	  fprintf (stderr, "WARNING: data records shorter than expected\n");
	  fprintf (stderr, "        only found %d columns on the tape\n",n);
	}
      }
      if (blocking_factor == 1)
        put_row (fd, tapebuf+firstcol-1);
      else {
	if (first[band]) {
	  r_skip = (firstrow-1) % blocking_factor;
	  first[band] = 0;
	}
	else
	  r_skip = 0;
	for (j=0; j<blocking_factor-r_skip; j++) {
	  p = tapebuf + (r_skip+j)*tapebufsize/blocking_factor;
	  put_row (fd, p+skipbytes+firstcol-1);
	}
      }
    }
    G_percent (nrows, nrows, 2);
    stat = 1;
DONE:
    fprintf (stderr, "\n");
    I_close_band (fd, &tape_info, band);

    return stat;
}
