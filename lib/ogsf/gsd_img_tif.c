/*
 * $Id$
 *
 * endian added Markus
 * changed 10/99 Jaro
 * Created new function GS_write_tif based
 * on RGB dump 
 */

#include <stdio.h>
#include <sys/types.h>
#include "image.h"
#include "tiffio.h"
#include "gstypes.h"

u_short config = PLANARCONFIG_CONTIG;
u_short compression = -1;
u_short rowsperstrip = 0;

unsigned short rbuf[8192];
unsigned short gbuf[8192];
unsigned short bbuf[8192];

int GS_write_tif(char *name)
{   
    TIFF *out;
    int y, x;
    unsigned int xsize, ysize;
    FILE *fp;
    int mapsize, linebytes, I;
    u_char *buf, *tmpptr;
    unsigned long *pixbuf;
    char all_buf[3];

    int swapFlag;

    /* endian test */
    swapFlag = G_is_little_endian();    

    gsd_getimage(&pixbuf, &xsize, &ysize);

    out = TIFFOpen(name, "w");
    if (out == NULL)
    {
	fprintf (stderr, "Cannot open file for output\n"),exit(1);
    }

    /* Write out TIFF Tags */
    /* Assuming 24 bit RGB Tif */
    TIFFSetField(out, TIFFTAG_IMAGEWIDTH, xsize);
    TIFFSetField(out, TIFFTAG_IMAGELENGTH, ysize);
    TIFFSetField(out, TIFFTAG_ORIENTATION, ORIENTATION_BOTLEFT);
    TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, 24 > 8 ? 3 : 1);
    TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, 24 > 1 ? 8 : 1);
    TIFFSetField(out, TIFFTAG_PLANARCONFIG, config);
    mapsize = 1<<24;

    TIFFSetField(out, TIFFTAG_PHOTOMETRIC, 24 > 8 ?
                PHOTOMETRIC_RGB : PHOTOMETRIC_MINISBLACK);

    linebytes = ((xsize*ysize+15) >> 3) &~ 1;
    
    if (TIFFScanlineSize(out) > linebytes)
    {
     	buf = (u_char *)malloc(linebytes);
    }
    else
    {
    	buf = (u_char *)malloc(TIFFScanlineSize(out));
    }
    
    if (rowsperstrip != (u_short)-1)
    {
    	rowsperstrip = (u_short)(8*1024/linebytes);
    }
    
    TIFFSetField(out, TIFFTAG_ROWSPERSTRIP, rowsperstrip == 0 ? 1 : rowsperstrip);

    /* Done with Header Info*/
    for (y=0; y<ysize; y++)
    {
	tmpptr = buf;
	
	for (x=0; x<(xsize); x++)
	{
	    if (!swapFlag)
	    {
	    	/* big endian: SUN et al. */
	    	*tmpptr++ = (pixbuf[y*xsize + x] & 0xFF000000)>>24;
	    	*tmpptr++ = (pixbuf[y*xsize + x] & 0x00FF0000)>>16;
	    	*tmpptr++ = (pixbuf[y*xsize + x] & 0x0000FF00)>>8;
	    }
	    else
	    {
	    	/* little endian: Linux et al. */
	    	*tmpptr++ = (pixbuf[y*xsize + x] & 0x000000FF);
	    	*tmpptr++ = (pixbuf[y*xsize + x] & 0x0000FF00)>>8;
	    	*tmpptr++ = (pixbuf[y*xsize + x] & 0x00FF0000)>>16;
 	    }
    	}
	
	if (TIFFWriteScanline(out, buf, y, 0) < 0)
    	{
	    break;
	}
    }
    
    (void) TIFFClose(out);
    
    return(0);
}
