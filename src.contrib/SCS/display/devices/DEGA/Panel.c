/* Functions: Panel_save, Panel_restore, Panel_delete	P.W. Carlson 5/89 */

#include <stdio.h>
#include "ega_io.h"

Panel_save(name, top, bottom, left, right)
int top, bottom, left, right;
char *name;
{
    register int y;
    int fd, num_bytes;

    /* compute the number of bytes */
    num_bytes = (right >> 3) - (left >> 3) + 1;

    /* open the file */
    fd = creat(name, 0644);

    /* write size information */
    write(fd, &top, sizeof(top));
    write(fd, &bottom, sizeof(bottom));
    write(fd, &left, sizeof(left));
    write(fd, &right, sizeof(right));


    /* get the raster and write it to the file */
    args.arg1 = left;
    args.arg2 = right;
    args.ptr1 = plane_0;
    args.ptr2 = plane_1;
    args.ptr3 = plane_2;
    for (y = top; y <= bottom; y++)
    {	args.arg3 = y;
        ioctl(egafd, EGA_GETPANEL, &args);
	write(fd, plane_0, num_bytes);
	write(fd, plane_1, num_bytes);
	write(fd, plane_2, num_bytes);
    }
    close(fd);
}


Panel_restore(name)
char *name;
{
    int fd, top, bottom, left, right, num_bytes;
    register int y;

    /* open the file and read the size info */
    fd = open(name, 0);
    read(fd, &top, sizeof(top));
    read(fd, &bottom, sizeof(bottom));
    read(fd, &left, sizeof(left));
    read(fd, &right, sizeof(right));

    /* compute the number of bytes */
    num_bytes = (right >> 3) - (left >> 3) + 1;

    /* read the raster from the file and display it */
    args.arg1 = left;
    args.arg2 = right;
    args.ptr1 = plane_0;
    args.ptr2 = plane_1;
    args.ptr3 = plane_2;
    for (y = top; y <= bottom; y++)
    {	read(fd, plane_0, num_bytes);
    	read(fd, plane_1, num_bytes);
    	read(fd, plane_2, num_bytes);
	args.arg3 = y;
        ioctl(egafd, EGA_PUTPANEL, &args);
    }
    close(fd);
}


Panel_delete(name)
char *name;
{
    unlink(name);
}
