/* - added little/big endian test Markus Neteler
 * -modified to PPM by Bob Covill <bcovill@tekmap.ns.ca>
 *
 * $Id$ 
 *
 * changed 10/99 Jaro
 * Created new function GS_write_ppm based
 * on RGB dump 
 */

#include "image.h"
#include "gstypes.h"

unsigned short rbuf[8192];
unsigned short gbuf[8192];
unsigned short bbuf[8192];

int GS_write_ppm(char *name)
{
    int y, x;
    unsigned int xsize, ysize;
    FILE *fp;
    unsigned long *pixbuf;
    char all_buf[3];

    int swapFlag;

    /* endian test */
    swapFlag = G_is_little_endian();    

    gsd_getimage(&pixbuf, &xsize, &ysize);

    if (NULL == (fp = fopen (name, "w")))
    {
	fprintf (stderr, "Cannot open file for output\n"),exit(1);
    }

    fprintf(fp, "P6 %d %d 255\n", xsize, ysize);

    for (y=ysize-1; y>=0; y--)
    {
	for(x=0; x<xsize; x++)
	{
	    if (!swapFlag)
	    {
		/* big endian: SUN et al. */
		rbuf[x] = (pixbuf[y*xsize + x] & 0xFF000000)>>24;
		gbuf[x] = (pixbuf[y*xsize + x] & 0x00FF0000)>>16;
		bbuf[x] = (pixbuf[y*xsize + x] & 0x0000FF00)>>8;
	    }
	    else
	    {
		/* little endian: Linux et al. */
	        rbuf[x] = (pixbuf[y*xsize + x] & 0x000000FF);
        	gbuf[x] = (pixbuf[y*xsize + x] & 0x0000FF00)>>8;
                bbuf[x] = (pixbuf[y*xsize + x] & 0x00FF0000)>>16;
	    }

	    fputc((int)rbuf[x], fp);
	    fputc((int)gbuf[x], fp);
	    fputc((int)bbuf[x], fp);	
	}

    }
    free(pixbuf);
    fclose (fp);

    return(0);
}
