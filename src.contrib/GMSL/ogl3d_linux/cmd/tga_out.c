/* Written by Dave Gerdes USA-CERL  Fall 1990
*/
#include "gis.h"
#include "interface.h"
/*struct Colors Colors;*/

long Ext_offset;

#define KEY1 "input"
#define KEY2 "output"

static unsigned long *pixbuf;
     
int 
targa_out (char *filename)

{
    int row,col;
    int nrows, ncols;
    int i;
    int n;
    FILE *fp;
    int success;

    if (NULL == (fp = fopen (filename, "w"))) 
	fprintf (stderr, "Cannot open file '%s' for output\n", filename),exit(1);

	success = gsd_getimage(&pixbuf, &ncols, &nrows);
	targahead_rgb (fp, nrows, ncols);
	do_rgb (fp, nrows, ncols);
	targa_extension (fp);
	targatail_rgb (fp, nrows, ncols);
    
    fclose (fp);
  
}
	
int 
do_rgb (FILE *fp, int nrows, int ncols)

{
    int row, col;
    char cbuf[3];

     
    for (row = 0; row < nrows; row++)
/*    for (row = nrows-1; row >= 0; row--) */
    {
	if (!pixbuf)
	    exit(1);
	for (col = 0; col < ncols; col++)
	{
	    
	     cbuf[2] = (pixbuf[row*ncols + col] & 0x000000FF);
	     cbuf[1] = (pixbuf[row*ncols + col] & 0x0000FF00)>>8;
	     cbuf[0] = (pixbuf[row*ncols + col] & 0x00FF0000)>>16;
	    
	     fwrite (cbuf, 1, 3, fp);
	}
    }
    free(pixbuf);
}

#define FWRITE_ZERO  fwrite (&Zero, 1, 1, fp)

static char Zero = 0;

int 
targahead_rgb (FILE *fp, int nrows, int ncols)
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
int 
targatail_rgb (FILE *fp)
{
    char *s = "TRUEVISION-XFILE.";
    Ext_offset = ftell (fp);
    /* Field 28   Extension Area Offset      4 bytes */
    fwrite_long (fp, Ext_offset);	/* set in targa_extension() */
    /* Field 29   Developer Directory Offset 4 bytes */
    fwrite_long (fp, 0);			/* not used */
    /* Field 30-32    Signature, Reserved Char, Terminator */
    fwrite (s, 1, strlen (s)+1, fp);
}




/*
**  force  little-endian write for intell compatible format
*/
int 
fwrite_short (register FILE *fp, int val)
{
    unsigned char c;

    c = val & 0xff;
    fwrite (&c, 1, 1, fp);
    c = (val>>8) & 0xff;
    fwrite (&c, 1, 1, fp);
}

int 
fwrite_long (register FILE *fp, int val)
{
    unsigned char c;
    register int i;

    for (i = 0 ; i < 4 ; i++)
    {
	c = (val >> 8 * i) & 0xff;
	fwrite (&c, 1, 1, fp);
    }
}



int 
targa_extension (FILE *fp)
{
    char buffer[500];
    int i;
    
    short ew_res = 50;
    short nw_res = 50;
    
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

    /* Field 19  Pixel Aspect Ratio 4 bytes 
    fwrite_short (fp, ew_res);	
    fwrite_short (fp, ns_res);  */ 

    /* Field 20   Gamma Value     4 bytes */
/**/fwrite (buffer, 2, 4, fp);			/* not used */

    /* Field 21   Color Corr. Offset  4 bytes */
/**/fwrite (buffer, 1, 4, fp);			/* not used */

    /* Field 22   Postage Stamp Offset 4 bytes */
/**/fwrite (buffer, 1, 4, fp);			/* not used */

    /* Field 23   Scan Line Offset    4 bytes */
/**/fwrite (buffer, 1, 4, fp);			/* not used */

    /* Field 24   Attributes Type     1 byte */
/**/fwrite (buffer, 1, 1, fp);			/* not used */

}
