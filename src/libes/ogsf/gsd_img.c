/* 
 * $Id$
 * added little/big endian test Markus Neteler 9/2000 
 * changed 10/99 Jaro*/

#include "image.h"
#include "gstypes.h"

static void ierrfunc(char *);

unsigned short rbuf[8192];
unsigned short gbuf[8192];
unsigned short bbuf[8192];

static void ierrfunc(char *ebuf)
{
    fprintf(stderr, "%s\n",ebuf);
    
    return;
}

int GS_write_rgb(char *name)
{
    int y, x;
    unsigned int xsize, ysize;
    IMAGE *image;
    unsigned long *pixbuf;
    int swapFlag;
    
    /* endian test */
    swapFlag = G_is_little_endian();

    gsd_getimage(&pixbuf, &xsize, &ysize);

    if (pixbuf)
    {
	i_seterror(ierrfunc);
	
	if (NULL == (image = iopen(name,"w",VERBATIM(1),3,xsize,ysize,3)))
	{
	    fprintf(stderr,"Unable to open %s for writing.\n", name);
	   
	    return (-1);
	}

	for (y=0; y<ysize; y++)
	{
	    for (x=0; x<xsize; x++)
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

		putrow(image,rbuf,y,0);		/* red row */
		putrow(image,gbuf,y,1);		/* green row */
		putrow(image,bbuf,y,2);		/* blue row */
	    }
	}

	free(pixbuf);
	iclose(image);

	return(0);
    }

    return(-1);
}
