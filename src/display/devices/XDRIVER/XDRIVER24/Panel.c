#include "gis.h"
#include "includes.h"

#include <unistd.h>
#include <fcntl.h>

int Panel_save(char *name, int top, int bottom, int left, int right)
{
    int fd;
    int width, height;
    XImage *im;
    
    /* Adjust panel edges if outside window necessary */
    if (top < screen_top)
        top = screen_top;
    if (bottom > screen_bottom)
        bottom = screen_bottom;
    if (left < screen_left)
        left = screen_left;
    if (right > screen_right)
        right = screen_right;

    height = bottom - top;
    width = right - left;

    /* Get the image off the pixmap */
    im = XGetImage(dpy, bkupmap, left, top, width, height,
		   AllPlanes, ZPixmap);

    /* open the file */
    fd = creat(name, 0644);
    if (fd < 0)
    {
	perror("unable to create panel file");
	return -1;
    }

    /* write the lower coordinates and size of image */
    write(fd, &left,   sizeof(left));
    write(fd, &top,    sizeof(top));
    write(fd, &width,  sizeof(width));
    write(fd, &height, sizeof(height));
    write(fd, &im->bytes_per_line, sizeof(im->bytes_per_line));
    write(fd, &im->xoffset, sizeof(im->xoffset));
    write(fd, &im->depth, sizeof(im->depth));

    /* write the data */
    write(fd, im->data, height * im->bytes_per_line);

    close(fd);

    XDestroyImage(im);

    return 0;
}

int Panel_restore(char *name)
{
    int fd;
    int top, left, width, height, bytes_per_line, xoffset, depth;
    char *data;
    XImage *newimage;
    XWindowAttributes xwa;

    /* open the file */
    fd = open(name, O_RDONLY);
    if (fd < 0)
    {
	perror("unable to open panel file");
	return -1;
    }

    read(fd, &left,   sizeof(left));
    read(fd, &top,    sizeof(top));
    read(fd, &width,  sizeof(width));
    read(fd, &height, sizeof(height));
    read(fd, &bytes_per_line, sizeof(bytes_per_line));
    read(fd, &xoffset, sizeof(xoffset));
    read(fd, &depth, sizeof(depth));

    data = G_malloc(bytes_per_line * height);

    /* read the data */
    read(fd, data, bytes_per_line * height);

    close(fd);

    /* now that data is in memory, get the window's attributes and turn
     * it into an image, then draw it. */
    if (XGetWindowAttributes(dpy, grwin, &xwa) == 0)
        return -1;

    newimage = XCreateImage(dpy, xwa.visual, depth, ZPixmap, xoffset,
			    data, width, height, 8, bytes_per_line);
    XPutImage(dpy, bkupmap, gc, newimage, 0, 0, left, top, width, height);

    /* free the XImage structure; also frees the data */
    XDestroyImage(newimage);

    needs_flush = 1;
    return 0;
}

int Panel_delete(char *name)
{
    unlink(name);
    return 0;
}

