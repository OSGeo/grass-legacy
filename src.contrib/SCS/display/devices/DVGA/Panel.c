/* Functions: Panel_save, Panel_restore, Panel_delete	P.W. Carlson 12/88 */

#include <stdio.h>
#include "vio_driver.h"

Panel_save(name, top, bottom, left, right)
int top, bottom, left, right;
char *name;
{
    register int y, n;
    int fd, numpixels;
    int i, paintfile;
    float red, green, blue;

    numpixels = right - left + 1;

    /* is this file to be painted? */
    paintfile = (top | bottom | left | right) ? 0 : 1;

    /* open the file */
    fd = creat(name, 0644);

    /* if not a paint file, write size info */
    if (!paintfile)
    {	write(fd, &top, sizeof(top));
    	write(fd, &bottom, sizeof(bottom));
    	write(fd, &left, sizeof(left));
    	write(fd, &numpixels, sizeof(numpixels));
    }

    /* if a paint file, write color and size info */
    else
    {	i = NCOLORS;
	write(fd, &i, sizeof(i));
	for (n = 0; n < i; n++)
    	{   args.arg1 = n;
            ioctl(viofd, VIO_GETPAL, &args);
	    red   = (float)args.arg2 / 63.0;
	    green = (float)args.arg3 / 63.0;
	    blue  = (float)args.arg4 / 63.0;
    	    write(fd, &red,   sizeof(red));
    	    write(fd, &green, sizeof(green));
    	    write(fd, &blue,  sizeof(blue));
	}
	i = 1;
	write(fd, &i, sizeof(i));
	i = SCREEN_BOTTOM + 1;
	write(fd, &i, sizeof(i));
	i = SCREEN_RIGHT + 1;
	write(fd, &i, sizeof(i));
	bottom = SCREEN_BOTTOM;
	right = SCREEN_RIGHT;
	numpixels = right + 1;
    }

    /* get the raster and write it to the file */
    for (y = top; y <= bottom; y++)
    {	args.arg1 = left;
        args.arg2 = right;
        args.arg3 = y;
        args.ptr  = raster_buff;
        ioctl(viofd, VIO_GETRAST, &args);
	write(fd, raster_buff, numpixels);
    }
    close(fd);
}


Panel_restore(name)
char *name;
{
    int fd, top, bottom, left, right, numpixels;
    register int y;

    /* open the file and read the size info */
    fd = open(name, 0);
    read(fd, &top, sizeof(top));
    read(fd, &bottom, sizeof(bottom));
    read(fd, &left, sizeof(left));
    read(fd, &numpixels, sizeof(numpixels));

    /* read the raster from the file and display it */
    right = left + numpixels - 1;
    for (y = top; y <= bottom; y++)
    {	read(fd, raster_buff, numpixels);
    	args.arg1 = left;
    	args.arg2 = y;
    	args.arg3 = right;
    	args.arg4 = y;
   	args.ptr  = raster_buff;
    	ioctl(viofd, VIO_PUTRAST, &args);
    }
    close(fd);
}


Panel_delete(name)
char *name;
{
    unlink(name);
}
