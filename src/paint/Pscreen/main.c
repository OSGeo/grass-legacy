/* %W%   %G% */
/**************************************************************

NAME:		Pscreen

FUNCTION:	send a file (created by Dsavescreen) to
		the color printer.

USAGE:		Pscreen file [-bw] [-w#] [-#[p|i]]

		file is file saved by Psavescreen
		-bw     black/white reversal
		-w#     set color for # to white
		-#[p|i] scales the output
		         floating point number with (optional) units
		         (panels or inches).

screen file format:
		int ncolors
		float colortable[3 * ncolors]
		int cellsize
		int nrows
		int ncols
		char cell[nrows * ncols * cellsize]

		ncolors: number of colors in color table
		colortable: rgb percentages
		cellsize: number of bytes per cell
		nrows:	number of rows of data
		ncols:	number of cells per row

COLOR ALGORITHM: The rbg percentages are converted to color
		numbers and stored in an allocated color table
		index buffer. Each cell is painted by looking
		up the color number in the table.
***************************************************************/

#define MAIN

#include <stdio.h>

#define GET(x,y) if (fread(x, y, 1, fd) != 1) {fprintf (stderr, "%s: %s: read error\n", argv[0], screenfile); exit(1);}


main(argc, argv)	char *argv[];
{
    float red;
    float grn;
    float blu;

    int panel;
    int npanels;

    int nrows;
    int ncols;
    int rowsize;
    int cellsize;
    int ncolors;
    unsigned char *cell;
    unsigned char *colortable;

    FILE *fd;		/* screen file descriptor */
    long offset;

    char *screenfile;
    int pcols, prows ;
    int npix ;
    int xcols;		/* horizontal pixels */
    int xrows;		/* vertical pixels */
    int prev_row;
    int i;
    int n;
    unsigned char white;
    unsigned char black;

    int *pixmap ;
    int *pm ;
    char *malloc();
    double Phres() ;
    double Pvres() ;
    int bwrev ;
    char units;
    double scale;
    double xscale;
    int have_scale ;

    char *me;
    char *slash;

    me = argv[0] ;
    for (slash = me ; *slash; slash++)
	if (*slash == '/')
	    me = slash + 1;

    bwrev = 0;
    screenfile = NULL;
    have_scale = 0;

    for (i = 1; i < argc; i++)
    {
	if (argv[i][0] == '-')
	{
	    if (strcmp (argv[i], "-bw") == 0)
		bwrev = 1;
	    else if (sscanf (argv[i],"-w%d", &n) == 1)
		continue;
	    else
	    {
		char u[2];

		if (have_scale++) usage(me);
		*u = 0;

		switch (sscanf (&argv[i][1],"%lf%1s", &scale, u))
		{
		case 2: break;
		case 1: if (*u != 0)
			    usage(me);
			break;
		default:
			usage(me);
		}
		if (scale < 0) usage(me);
		switch (*u)
		{
		case 0: units = ' ';
			break;
		case 'I':
		case 'i': units = 'i';
			break;
		case 'P':
		case 'p': units = 'p';
			break;
		default:
			usage(me);
		}
	    }
	}
	else if (screenfile == NULL)
	    screenfile = argv[i];
	else
	    usage (me);
    }
    if (screenfile == NULL)
	usage (me);

    if (!have_scale)
    {
	scale = 1.0;
	units = 'p';
    }

/* open the screenfile */

    if ((fd = fopen(screenfile, "r")) == NULL)
    {
	perror (screenfile);
	exit(1);
    }

/* connect to the printer */
    Pconnect();
    Plock();
    Popen();
    white = Pcolornum (1.0, 1.0, 1.0) ;
    black = Pcolornum (0.0, 0.0, 0.0) ;
    Pnpixels(&prows, &pcols) ;
    if (prows)
    {
	printf ("This function NOT available for current PAINT device\n");
	exit(1);
    }

/*
* read number of colors
* allocate colortable
* read colortable
*/

    GET (&ncolors, sizeof ncolors) ;

    colortable = (unsigned char *) malloc ((unsigned) ncolors);
    if (colortable == NULL)
    {
	fprintf(stderr,"Out of Memory\n");
	exit(1);
    }

    for (i = 0; i < ncolors; i++)
    {
	GET (&red, sizeof red);
	GET (&grn, sizeof grn);
	GET (&blu, sizeof blu);

	colortable[i] = Pcolornum (red, grn, blu);
	if (bwrev)
	{
	    if (colortable[i] == white)
		colortable[i] = black;
	    else if (colortable[i] == black)
		colortable[i] = white;
	}
    }
    for (i = 1; i < argc; i++)
	if (sscanf (argv[i], "-w%d", &n) == 1)
	    if (n >= 0 && n < ncolors)
		colortable[n] = white;

/*
* read cell size, number of rows, cols
* allocate cell buffer
*/

    GET (&cellsize, sizeof cellsize) ;

    GET (&nrows, sizeof nrows);

    GET (&ncols, sizeof ncols) ;

    rowsize = cellsize * ncols;

/* convert scaling to number of output cols (xcols) */
    switch (units)
    {
    case ' ':
	xcols = scale * ncols;
	break;
    case 'i':
	xcols = scale * Phres();
	break;
    case 'p':
	xcols = scale * pcols;
	break;
    default:
	fprintf (stderr, "OOPS - shouldn't happen\n");
	xcols = pcols;
    }
    if (xcols <= 0)
    {
	fprintf (stderr, "%s: warning - scale request too small.\n", me);
	exit(1);
    }

    xscale = (double) xcols / ncols;
    xrows = nrows * xscale + .5 ;

    cell = (unsigned char *) malloc ((unsigned) rowsize);
    if (cell == NULL)
    {
	fprintf(stderr,"Out of Memory\n");
	exit(1);
    }

    pixmap = (int *) malloc ((unsigned) pcols * sizeof(int));
    if (pixmap == NULL)
    {
	fprintf(stderr,"Out of Memory\n");
	exit(1);
    }

    npanels = (xcols + pcols - 1) / pcols;
    if (npanels > 1) printf ("%d panels\n", npanels);


    offset = ftell (fd);

    for (panel = 0; panel < npanels; panel++)
    {
	fseek (fd, offset, 0);

	npix = xcols - panel*pcols;
	if (npix > pcols)
	    npix = pcols;
	for (i = 0; i < npix; i++)
	{
	    register int col;

	    col = (panel*pcols+i)/xscale + .5;
	    if (col >= ncols)
		col = ncols - 1;
	    pixmap[i] = col * cellsize ;
	}



    /* setup the map */

	Praster ();
	Ppictsize (xrows+2, npix);

	Prle_begin();
	Prle (black, npix);
	Prle_end();

    /* paint the screen */

	prev_row = -1;
	for (i = 0; i < xrows; i++)
	{
	    register int n;
	    int row;

	    Prle_begin ();

	    row = i/xscale + .5;
	    if (row >= nrows)
		row = nrows - 1 ;
	    while (prev_row < row)
	    {
		prev_row++;
		GET (cell, rowsize);
	    }
	    pm = pixmap ;
	    for (n = 0; n < npix; n++) 
	    {
		Prle (colortable[idx(cell + *pm++,cellsize,ncolors)], 1);
	    }
	    Prle_end ();
	}
	Prle_begin();
	Prle (black, npix);
	Prle_end();
    }

/* close the printer and flush the output */
    Pclose ();
    Pdisconnect();
    exit(0);
}

usage (me)
    char *me;
{
    fprintf (stderr, "usage: %s [-bw] [-w#] -[#[panels|inches]] file\n", me);
    exit(1);
}
