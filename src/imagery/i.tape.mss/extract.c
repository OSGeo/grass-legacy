/*******************************************************************
NAME:		extract()

FUNCTION:	skips the specified number of rows in a LANDSAT tape
		and then transplants the requested raw data to the
		requested files.

USAGE:		extract(tapefd, buf, tape_startrow, tape_endrow, 
			startrow, endrow, startcol, endcol)
			     
		unsigned char buf[]
*******************************************************************/

#include "tape.h"

extract(tapefd, buf, tape_startrow, tape_endrow, startrow, endrow, startcol, endcol) 

    unsigned char buf[] ;
{
    int nrows ;      /* number of rows for this tape */
    int row;         /* current gis window row */
    int count ;      /* current row this tape */
    int band ;
    int skip;


    if (startrow < tape_startrow)
	startrow = tape_startrow;

    if (endrow > tape_endrow)
	endrow = tape_endrow;

    startcol--;
    count = 0;
    nrows = endrow - startrow + 1;
    row = startrow - firstrow;

/* skip to requested beginning row */

    printf("extract rows %d-%d\n", startrow, endrow);

    if ((skip = (startrow - tape_startrow)) > 0)
    {
	printf("Skipping to row %d...", startrow) ;
	fflush (stdout);

	I_tape_advance (tapefd, skip * nbands - 1);
	if (read(tapefd, buf, record_size)==-1) 
	{
	    printf("\nERROR: Reading row %d", startrow) ;
	    return 0;
	}

	printf("\n");
    }

/* write out first record into band1 */

    printf ("extracting ...");
    G_percent (count, nrows, 10);

    if (bandfd[0] >= 0)
    {
	put_row(bandfd[0], &buf[startcol], row) ; 
    }


/* read in and write out the rest of the row 1 bands */
    for (band=1; band<nbands; band++) 
    {
	if (read(tapefd, buf, record_size)==-1) 
	{
	    printf("\nERROR: Reading row %d", startrow) ;
	    return 0;
	}
	if (bandfd[band] >= 0)
	{
	    put_row(bandfd[band], &buf[startcol], row) ; 
	}
    }

/* read through rest of desired records, extracting desired columns */

    while (++startrow <= endrow)
    {
	G_percent (count, nrows, 10);
	row++;
	count++;

	for (band=0; band<nbands; band++) 
	{
	    if (read(tapefd, buf, record_size)==-1) 
	    {
		printf("\nERROR: Reading row %d", startrow) ;
		return 0;
	    }
	    if (bandfd[band] >= 0)
	    {
		put_row(bandfd[band], &buf[startcol], row) ; 
	    }
	}
    }
    G_percent (nrows, nrows, 10);
}
