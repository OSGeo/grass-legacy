/* Written by Dave Gerdes USA-CERL  Fall 1990
*/
#include "gis.h"

struct Colors Colors;

long Ext_offset;

main (argc,argv) 
    char *argv[];
{
    CELL *cell;
    struct Cell_head Window;
    char *colr;
    char *in, *mapset, *outfile;
    FILE *fp;
    int cf;
    int row,col;
    int nrows, ncols;
    int i;
    int n;
    int lut;

    G_gisinit (argv[0]);

    parse_command_line (argc, argv, &in, &outfile);

    /*
    **  Color LUT,  NOT yet implemented
    */
    lut = 0;


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

    if (NULL == (fp = fopen (outfile, "w")))
	fprintf (stderr, "Cannot open file '%s' for output\n", outfile),exit(1);



    cell = G_allocate_cell_buf();
    colr = (char *) G_allocate_cell_buf();

    G_get_window (&Window);
    nrows = G_window_rows();
    ncols = G_window_cols();


/*    if (!lut || Colors.max-Colors.min > 1024)  Colors struct changed */
    if (!lut)
    {
	targahead_rgb (fp, nrows, ncols);
	do_rgb (cf, fp, cell, colr, nrows, ncols);
	targa_extension (fp, &Window);
	targatail_rgb (fp, nrows, ncols);
    }
    else
    {
	targahead_lut (fp, nrows, ncols);
	do_lut (cf, fp, cell, colr, nrows, ncols);
	targa_extension (fp, &Window);
	targatail_lut (fp, nrows, ncols);
    }

    fclose (fp);
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


    /*for (row = 0; row < nrows; row++)*/
    for (row = nrows-1; row >= 0; row--)
    {
	if (G_get_map_row (cf, cell, row) < 0)
	    exit(1);
	for (col = 0; col < ncols; col++)
	{
	    G_get_color (cell[col], &r, &g, &b, &Colors);
	    /*cbuf[0] = r; cbuf[1] = g; cbuf[2] = b;*/
	    cbuf[0] = b; cbuf[1] = g; cbuf[2] = r;
	    fwrite (cbuf, 1, 3, fp);
	}
    }
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
    fwrite_short (fp, 0);		/* X origin           5.1 */
    fwrite_short (fp, 0);		/* Y origin           5.2 */
#ifdef FOO
    fwrite_short (fp, nrows-1);		/* Y origin           5.2 */
#endif
    fwrite_short (fp, ncols);		/* Image Width        5.3*/
    fwrite_short (fp, nrows);		/* Image Height       5.4*/
    c = 24;
    fwrite (&c, 1, 1, fp);		/* Pixel Depth        5.5*/
    /* c = 0x20; */			/* Image Descriptor   5.6 */
    /*c = 0x30;*/			/* Image Descriptor   5.6 */
    c = 0x00;				/* Image Descriptor   5.6 */
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
    fwrite_long (fp, Ext_offset);	/* set in targa_extension() */
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

targa_extension (fp, Window)
    FILE *fp;
    struct Cell_head *Window;
{
    char buffer[500];
    int i;

    Ext_offset = ftell (fp);

    /* Field 10  Extension Size   2 bytes */
    fwrite_short (fp, 494);		/* 494 bytes for version 2.0 tga */

    /* Field 11  Image Author Name */
    sprintf (buffer, "%41s", G_whoami());
    fwrite (buffer, 1, 42, fp);		/* 41 chars plus NULL */

    /* Field 12  Author Comments  324 Bytes */
    /* Note later fields depend on buffer being full of zeros */
    for (i = 0 ; i < 324 ; i++)
	buffer[i] = 0;
/**/fwrite (buffer, 1, 324, fp);		/* 4 lines 80 chars + nuls */

    /* Field 13  Date/Time        12 bytes */
/**/fwrite (buffer, 1, 12, fp);		       /* 6 shorts  (not used here) */

    /* Field 14   Job Name/ID     41 bytes */
/**/fwrite (buffer, 1, 41, fp);		       /* (not used here) */

    /* Field 15   Job Time         6 bytes */
/**/fwrite (buffer, 1, 6, fp);		       /* (not used here) */
    
    /* Field 16   Software ID     41 Bytes */
    fwrite ("Cell2tga  USA-CERL  GRASS               ", 1, 41, fp);

    /* Field 17   Software Version  3 bytes */
    fwrite_short (fp, 100);			/* Version 1.00 */
    fwrite (" ", 1, 1, fp);

    /* Field 18   Key Color 	   4 bytes */
/**/fwrite (buffer, 1, 4, fp);			/* Black */

    /* Field 19  Pixel Aspect Ratio 4 bytes */
    fwrite_short (fp, (short) Window->ew_res);	/* this is the only reason I */
    fwrite_short (fp, (short) Window->ns_res);    /* went through all this crap */

    /* Field 20   Gamma Value     4 bytes */
/**/fwrite (buffer, 1, 4, fp);			/* not used */

    /* Field 21   Color Corr. Offset  4 bytes */
/**/fwrite (buffer, 1, 4, fp);			/* not used */

    /* Field 22   Postage Stamp Offset 4 bytes */
/**/fwrite (buffer, 1, 4, fp);			/* not used */

    /* Field 23   Scan Line Offset    4 bytes */
/**/fwrite (buffer, 1, 4, fp);			/* not used */

    /* Field 24   Attributes Type     1 byte */
/**/fwrite (buffer, 1, 1, fp);			/* not used */

}

/* Use defines cuz current standards keep chaning */

/*  see what I mean?
#define KEY1 "raster"
#define KEY2 "tga"
*/
#define KEY1 "input"
#define KEY2 "output"


parse_command_line (argc, argv, in, outfile)
    int argc;
    char **argv;
    char **in, **outfile;
{
    struct Option *rast, *tga;
    static char strbuf[500];

    rast = G_define_option ();
    rast->key 		= KEY1;
    rast->type 		= TYPE_STRING;
    rast->required	= YES;
    rast->multiple	= NO;
    rast->gisprompt	= "old,cell,raster";
    rast->description	= "Raster file to output to TGA";

    tga = G_define_option ();
    tga->key 		= KEY2;
    tga->type 		= TYPE_STRING;
    tga->required	= NO;
    tga->multiple	= NO;
    tga->description	= "Name of TGA (Targa) output file";

    if (G_parser (argc, argv))
	exit (-1);

    *in = rast->answer;
    *outfile = tga->answer;

    if (!(*outfile))
    {
	sprintf (strbuf, "%s.tga", *in);
	*outfile = strbuf;
    }

    return (0);
}
