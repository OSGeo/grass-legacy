#include "dma.h"

static char *BADFORMAT  = "unrecognized tape format - probably not a DMA tape";

extract ()
{
    int have_data;
    int have_uhl;
    int first_uhl;
    long xnorth, xsouth, xeast, xwest;
    unsigned char *tapeptr;
    int outlen;
    long offset ;
    int row ;

    have_data = have_uhl = 0;
    first_uhl = 1;

/*
 * allocate the tape buffer large enough to start
 * uhl() routine will reallocate this when the size of a data record
 * is determined
 */
    tapebuf = (unsigned char *) malloc (tapebuflen = 4096);
    if (tapebuf == NULL)
    {
	error ("Not Enough Memory",0);
	exit(1);
    }

/*
 * UHL records contain the lat/lon of the tape data window
 * 252 records are the data records
 * other records are ignored
 */
    xnorth = tsouth < south ? tsouth : south ;
    
    while (readtape())
    {
	if (*tapebuf == 0252)
	{
if (verbose > 1 && !have_data) fprintf (stderr, "DATA\n");

	    if (!have_uhl)
	    {
		error (BADFORMAT,0);
		return 0;
	    }
	    have_data = 1;

	    if (outlen > 0 && row >= 0 && row < nrows)
	    {
		/* output data */
		seek_out (row * ncols * 2 + offset);
		write_out (tapeptr, outlen);
	    }
	    row++ ;

/* stop logic - if we have written the NE corner of the
   user window, stop. This assumes that the DMA data is
   in adjacent blocks on the tape from SW to NE
*/
	    if (stopok && row >= nrows && xnorth >= north)
		return 1;	/* all done */
	    continue;
	}

if (verbose > 1)
    fprintf (stderr, "%c%c%c\n", tapebuf[0],tapebuf[1],tapebuf[2]);

	if (have_data)
	    have_data = have_uhl = 0;

	if (strncmp (tapebuf, "VOL", 3) == 0)
	    continue;
	if (strncmp (tapebuf, "EOF", 3) == 0)
	    continue;
	if (strncmp (tapebuf, "HDR", 3) == 0)
	    continue;
	if (strncmp (tapebuf, "UTL", 3) == 0)
	    continue;
	if (strncmp (tapebuf, "UHL", 3) == 0)
	{
	    switch (uhl(first_uhl))
	    {
	    case -1:
		error ("data resolution varies within the tape",0);
		return 0;
	    case 0:
		error (BADFORMAT,0);
		return 0;
	    default:
		break;
	    }
	    if (first_uhl)
	    {
/* ajdust user window to data resolution */
		round_up   (&north, latres);
		round_down (&south, latres);
		round_up   (&west,  lonres);
		round_down (&east,  lonres);
		ncols =  (north-south)/latres + 1;
		nrows =  (west-east)/lonres + 1;

/* write the header file */
		write_hd();

/* write zeros to output file */

if (verbose)
    fprintf (stderr, "initializing %s\n", outname);
		seek_out (0L);
		zero_out ((long) nrows*ncols*2) ;
		seek_out (0L);

		first_uhl = 0;
	    }

	    xnorth = tnorth < north ? tnorth : north ;
	    xsouth = tsouth > south ? tsouth : south ;
	    xeast  = teast  > east  ? teast  : east  ;
	    xwest  = twest  < west  ? twest  : west  ;

	    tapeptr = tapebuf + ((xsouth - tsouth) / latres) * 2 + 8 ;
	    offset = ((xsouth - south) / latres) * 2 ;
	    row = (west - twest) / lonres ;
	    if (tnorth < south || tsouth > north
	    ||  twest  < east  || teast  > west)
		outlen = 0;
	    else
		outlen = ((xnorth - xsouth) / latres + 1) * 2;


if (verbose)
{
 fprintf (stderr, "  SW ");
 printgeo (stderr, tsouth, "SN");
 fprintf (stderr, " ");
 printgeo (stderr, twest, "EW");
}
if (verbose > 1)
{
 fprintf (stderr, " (");
 printgeo (stderr, south, "SN");
 fprintf (stderr, " ");
 printgeo (stderr, west, "EW");
 fprintf (stderr, " = ");
 printgeo (stderr, xsouth, "SN");
 fprintf (stderr, " ");
 printgeo (stderr, xwest, "EW");
 fprintf (stderr, ")");
}

if (verbose)
{
 fprintf (stderr, "\n");

 fprintf (stderr, "  NE ");
 printgeo (stderr, tnorth, "SN");
 fprintf (stderr, " ");
 printgeo (stderr, teast, "EW");
}
if (verbose > 1)
{
 fprintf (stderr, " (");
 printgeo (stderr, north, "SN");
 fprintf (stderr, " ");
 printgeo (stderr, east, "EW");
 fprintf (stderr, " = ");
 printgeo (stderr, xnorth, "SN");
 fprintf (stderr, " ");
 printgeo (stderr, xeast, "EW");
 fprintf (stderr, ")");
}

if (verbose)
 fprintf (stderr, "\n");

if (verbose > 1)
 fprintf (stderr, "%d rows %d cols\n", nrows, ncols);

if (verbose > 2)
{
 fprintf (stderr, "north  %-10ld south  %-10ld east  %-10ld west  %-10ld\n",north,south,east,west);
 fprintf (stderr, "tnorth %-10ld tsouth %-10ld teast %-10ld twest %-10ld\n",tnorth,tsouth,teast,twest);
 fprintf (stderr, "xnorth %-10ld xsouth %-10ld xeast %-10ld xwest %-10ld\n",xnorth,xsouth,xeast,xwest);
}
	    have_uhl = 1;
	    continue;
	}
	error(BADFORMAT,0);
	return 0;
    }

    return 1;
}
