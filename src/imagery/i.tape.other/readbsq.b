#include <stdio.h>
#include "tape.h"

readbsq (band)
{
    int row;
    int n;
    int fd ;
    static int too_short = 0;
    int stat ;

    stat = 0;
    fd = I_open_band_new (band);

    printf ("\nbandfile %s: skipping to row %d ...",I_bandname(band), firstrow);
    fflush (stdout);

    I_tape_advance (tapefd, firstrow-1);

    printf ("\nextracting ... "); fflush (stdout);
    for (row = 0; row < nrows; row++)
    {
	G_percent (row, nrows, 10);
	n = read (tapefd, tapebuf, lastcol);
	if (n < 0)
	{
	    printf("\n");
	    fprintf(stderr,"ERROR bad tape read (row %d band %d)\n",firstrow+row,band);
	    goto DONE;
	}

	if (n == 0)
	{
	    printf("\n");
	    fprintf(stderr,"ERROR only found %d rows on tape\n", firstrow+row-1);
	    goto DONE;
	}

	if (n < lastcol)
	{
	    if (!too_short)
	    {
		too_short = 1;
		printf("\n");
		fprintf (stderr, "WARNING data records shorter than expected\n");
		fprintf (stderr, "        only found %d columns on the tape\n", n);
	    }
	}
	put_row (fd, tapebuf+firstcol-1, row);
    }
    G_percent (row, nrows, 10);
    stat = 1;
DONE:
    printf ("\n");
    I_close_band (fd, &tape_info, band);

    return stat;
}
