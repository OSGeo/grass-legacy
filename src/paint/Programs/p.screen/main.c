/**************************************************************

NAME:		p.screen

FUNCTION:	send a file (created by Dsavescreen) to
		the color printer.

USAGE:		p.screen input=file [-r] [white=#] [scale=#[p|i]]

		file is file saved by d.savescreen
		-r          black/white reversal
		white=#     set color for # to white
		scale=#[p|i] scales the output
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

#include "gis.h"

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
    struct
    {
	struct Option *input, *scale, *bg;
    } parm;
    struct
    {
	struct Flag *bw;
    } flag;

    G_gisinit (argv[0]);

    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->type = TYPE_STRING;
    parm.input->required = YES;
    parm.input->description = "name of file containing screen image";

    parm.bg = G_define_option();
    parm.bg->key="bg";
    parm.bg->type=TYPE_INTEGER;
    parm.bg->description="data value representing the background";

    parm.scale = G_define_option();
    parm.scale->key = "scale";
    parm.scale->key_desc = "value[p|i]";
    parm.scale->type = TYPE_STRING;
    parm.scale->description = "image scaling factor";

    flag.bw = G_define_flag();
    flag.bw->key = 'r';
    flag.bw->description = "black/white reversal";


    if (G_parser(argc,argv))
	exit(1);

    bwrev = flag.bw->answer;
    screenfile =  parm.input->answer;

    if(!scan_scale(parm.scale->answer, &scale, &units))
    {
	fprintf (stderr, "%s - bad scale request\n", parm.scale->answer);
	G_usage();
	exit(1);
    }

/* open the screenfile */

    if ((fd = fopen(screenfile, "r")) == NULL)
    {
	fprintf (stderr, "%s - ", G_program_name());
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
	fprintf (stderr, "%s - this function NOT available for <%s> painter\n",
	    G_program_name(), Ppainter_name());
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
    if (parm.bg->answer)
    {
	if(sscanf (parm.bg->answer, "%d", &n) == 1 && n >= 0 && n < ncolors)
	    colortable[n] = white;
    }

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
	fprintf (stderr, "%s: ERROR - scale request too small\n", G_program_name());
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
