/* %W% %G% */
#include "gis.h"

struct Colors Colors;

main (argc,argv) 
    char *argv[];
{
    CELL *cell;
    char *colr;
    char *in, *mapset, *outfile;
    FILE *outfp;
    int cf;
    int row,col;
    int nrows, ncols;
    int i;
    int have_cell;
    int n;
    char fmt[20];
    int lut;

    have_cell = 0;
    strcpy (fmt, "%ld ");

    if (argc != 3 && argc != 4)
	fprintf (stderr, "Usage: %s cell_in out.tga\n", argv[0]), exit(1);

    /*
    **  if there is a 4th argument, then use a color lookup table
    **  NOT yet implemented
    */
    if (argc == 4)
	lut = 1;
    else
	lut = 0;

    in      = argv[1];
    outfile = argv[2];

    G_gisinit (argv[0]);

    mapset = G_find_cell (in,"");
    if (mapset == NULL)
    {
	fprintf (stderr, "%s: %s: cellfile not found\n", argv[0], in);
	exit(1);
    }

    cf = G_open_cell_old (in, mapset);
    if (cf < 0)
	exit(1);

    if (0 > G_read_colors (in, mapset, &Colors))
	exit (1);

    if (NULL == (outfp = fopen (outfile, "w")))
	fprintf (stderr, "Cannot open file '%s' for output\n", outfile),exit(1);

    cell = G_allocate_cell_buf();
    colr = (char *) G_allocate_cell_buf();

    nrows = G_window_rows();
    ncols = G_window_cols();


    if (!lut || Colors.max-Colors.min > 1024)
	do_rgb (cf, outfp, cell, colr, nrows, ncols);
    else
	do_lut (cf, outfp, cell, colr, nrows, ncols);

    fclose (outfp);
    G_close_cell (cf);

    exit(0);
}

do_rgb (cf, fp, cell, colr, nrows, ncols)
    FILE *fp;
    char *colr;
    CELL *cell;
{
    int row, col;
    char cbuf[3];
    int r,g,b;

    targahead_rgb (fp, nrows, ncols);

    for (row = 0; row < nrows; row++)
    {
	if (G_get_map_row (cf, cell, row) < 0)
	    exit(1);
	for (col = 0; col < ncols; col++)
	{
	    G_get_color (cell[col], &r, &g, &b, &Colors);
	    cbuf[0] = r; cbuf[1] = g; cbuf[2] = b;
	    fwrite (cbuf, 1, 3, fp);
	}
    }
    targatail_rgb (fp, nrows, ncols);
}

#define FWRITE_ZERO  fwrite (&Zero, 1, 1, fp)

static char Zero = 0;

targahead_rgb (fp, nrows, ncols)
    FILE *fp;
{
    char c;

    /* Field 1  ID Length    1 byte */
    FWRITE_ZERO;

    /* Field 2  Color Map Type    1 byte */
    FWRITE_ZERO;

    /* Field 3  Image Type    1 byte */
    c = 2;		/* Uncompressed True-Color */
    fwrite (&c, 1, 1, fp);

    /* Field 4  Color Map Specification  5 bytes */
    fwrite_short (fp, 0);		/* First Entry Index */
    fwrite_short (fp, 0);		/* Color Map Length */
    FWRITE_ZERO;			/* Color Map Entry Size */

    /* Field 5   Image Specification   10 bytes */
    fwrite_short (fp, 0);		/* X origin */
    fwrite_short (fp, 0);		/* Y origin */
    fwrite_short (fp, ncols);		/* Image Width */
    fwrite_short (fp, nrows);		/* Image Height */
    c = 24;
    fwrite (&c, 1, 1, fp);		/* Pixel Depth */
    c = 0x20;				/* Image Descriptor */
    fwrite (&c, 1, 1, fp);		/*     origin top left */	

    /* Field 6   Image ID  (not used) */

    /* Field 7   Color Map Data  (not used) */

    /* Field 8   Image Data  (filled in in main) */
}

/*
** write targa footer
*/
targatail_rgb (fp)
    FILE *fp;
{
    char *s = "TRUEVISION-XFILE.";

    /* Field 28   Extension Area Offset      4 bytes */
    fwrite_long (fp, 0);			/* not used */
    /* Field 29   Developer Directory Offset 4 bytes */
    fwrite_long (fp, 0);			/* not used */
    /* Field 30-32    Signature, Reserved Char, Terminator */
    fwrite (s, 1, strlen (s)+1, fp);
}


do_lut (cf, fp, cell, colr, nrows, ncols)
    FILE *fp;
    char *colr;
    CELL *cell;
{
    int row, col;
    char cbuf[3];

    targahead_lut (fp, nrows, ncols);

    for (row = 0; row < nrows; row++)
    {
	if (G_get_map_row (cf, cell, row) < 0)
	    exit(1);
	for (col = 0; col < ncols; col++)
	{
	    /*
	    G_get_color (cell[col], &r, &g, &b, &Colors);
	    cbuf[0] = r; cbuf[1] = g; cbuf[2] = b;
	    fwrite (cbuf, 1, 3, fp);
	    */
	}
    }
    targatail_lut (fp, nrows, ncols);
}

targahead_lut (fp, nrows, ncols)
    FILE *fp;
{
    char c;

    /* Field 1  ID Length    1 byte */
    FWRITE_ZERO;

    /* Field 2  Color Map Type    1 byte */
    FWRITE_ZERO;

    /* Field 3  Image Type    1 byte */
    c = 2;		/* Uncompressed True-Color */
    fwrite (&c, 1, 1, fp);

    /* Field 4  Color Map Specification  5 bytes */
    fwrite_short (fp, 0);		/* First Entry Index */
    fwrite_short (fp, 0);		/* Color Map Length */
    FWRITE_ZERO;			/* Color Map Entry Size */

    /* Field 5   Image Specification   10 bytes */
    fwrite_short (fp, 0);		/* X origin */
    fwrite_short (fp, 0);		/* Y origin */
    fwrite_short (fp, ncols);		/* Image Width */
    fwrite_short (fp, nrows);		/* Image Height */
    c = 24;
    fwrite (&c, 1, 1, fp);		/* Pixel Depth */
    c = 0x20;				/* Image Descriptor */
    fwrite (&c, 1, 1, fp);		/*     origin top left */	

    /* Field 6   Image ID  (not used) */

    /* Field 7   Color Map Data  (not used) */

    /* Field 8   Image Data  (filled in in main) */
}

/*
** write targa footer
*/
targatail_lut (fp)
    FILE *fp;
{
    return targatail_rgb (fp);
}

/*
**  force  little-endian write for intell compatible format
*/
fwrite_short (fp, val)
    register FILE *fp;
    register short val;
{
    unsigned char c;

    c = val & 0xff;
    fwrite (&c, 1, 1, fp);
    c = (val>>8) & 0xff;
    fwrite (&c, 1, 1, fp);
}

fwrite_long (fp, val)
    register FILE *fp;
    register short val;
{
    unsigned char c;
    register int i;

    for (i = 0 ; i < 4 ; i++)
    {
	c = (val >> 8 * i) & 0xff;
	fwrite (&c, 1, 1, fp);
    }
}
