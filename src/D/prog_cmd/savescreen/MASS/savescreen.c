/*  %W%  %G%  */
#include <stdio.h>

savescreen (fd)
    FILE *fd;
{
    char *malloc();

    struct colorbuf
    {
	unsigned char unused;
	unsigned char red;
	unsigned char grn;
	unsigned char blu;
    } colorbuf;

    float red, grn, blu;
    int ncolors;
    int ncols;
    int nrows;
    int i,j,k;
    int row;
    int left;
    int right;
    int top;
    int bottom;
    int junk;
    unsigned char *planebuf;
    int planebufsize;
    int plane;
    int frame;
    int mod;


/* assign the graphic processor */
    mgiasngp (0, 0) ;


/* get the full screen coordinates */
    mgigetvcoor (2, &left, &bottom, &right, &top, &junk);

    ncols = (right - left + 1);
    nrows = (top - bottom + 1);

/* determine the number of color registers needed for NPLANES */
/* ncolors = 2 raised to the NPLANES power			*/

    for (ncolors = 1, i = 0; i < NPLANES; i++)
	ncolors *= 2;

/* allocate plane buffer */

    planebufsize = (right - left) / 8 + 1;
    planebuf = (unsigned char *) malloc (planebufsize);
    if (planebuf == NULL)
    {
	fprintf(stderr, "Out of Memory\n");
	return 0;
    }

/* get current frame number */
    mgigetfb (&frame, &mod);

/* write out the color table: number of colors followed by color % */

    i = ncolors ;
    fwrite (&i, sizeof i, 1, fd);

    for (i = 0; i < ncolors; i++)
    {
	int n;

	mgigetcms (i, 1, &colorbuf);

/* must do conversion from unsigned char to int explicitly */
/* and then from int to float. Compiler complains about    */
/* unsigned char to float with -f compile option	   */

	n = (int)colorbuf.red ;
	red = (float) n / 255.0 ;

	n = (int)colorbuf.grn ;
	grn = (float) n / 255.0 ;

	n = (int)colorbuf.blu ;
	blu = (float) n / 255.0 ;

	fwrite (&red, sizeof red, 1, fd);
	fwrite (&grn, sizeof grn, 1, fd);
	fwrite (&blu, sizeof blu, 1, fd);
    }

/* write number of planes, planebufsize */
    i = NPLANES ;
    fwrite (&i, sizeof i, 1, fd);

    i = planebufsize ;
    fwrite (&i, sizeof i, 1, fd);

/* write number of rows and number of cols */

    i = nrows ;
    fwrite (&i, sizeof i, 1, fd);
    i = ncols ;
    fwrite (&i, sizeof i, 1, fd);

/*
* for each row, get image from each plane
*/

    i = 0;
    k = -1;
    for (row=top; row>=bottom; row--)
    {
	j = i++ * 100 / nrows ;
	if (j%5 == 0 && j != k)
	{
	    printf("%4d%%\b\b\b\b\b", k = j);
	    fflush (stdout);
	}

	for (plane = 0; plane < NPLANES; plane++)
	{
	    mgigetfbdata (frame, 1<<plane, left, row, right, row, planebuf);
	    fwrite (planebuf, sizeof(*planebuf), planebufsize, fd);
	}
    }
    printf (" 100%%\n");
    free (planebuf);

/* deassign graphics processor */
    mgideagp (0, 0);

/* done */
    return 1;
}
