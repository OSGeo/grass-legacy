#include "gis.h"
#include <stdio.h>
#include "includes.h"

extern int screen_top;
extern int screen_bottom;
extern int screen_left;
extern int screen_right;

extern Display *dpy;
extern Window grwin;
extern GC gc;
extern Pixmap bkupmap;

/* Saves all bit plane information for the screen area described by
 * top, bottom, left, and right borders.  Associates the saved
 * information with the string "name".  This name is a local system
 * file name which is actually be used to store the image. */
int Panel_save (char *name, int top, int bottom, int left, int right)
{
    int fd;
    int width, height, i;
    XImage *impanel;
    char *dpoint;
    
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
    impanel = XGetImage(dpy, bkupmap, left, top, width, height,
			AllPlanes, ZPixmap);

    /* open the file */
    fd = creat(name, 0644);
    /* write the lower coordinates and size of image */
    write(fd, (char *) &left, sizeof(left));
    write(fd, (char *) &top, sizeof(top));
    write(fd, (char *) &width, sizeof(width));
    write(fd, (char *) &height, sizeof(height));
    write(fd, (char *) &(impanel->bytes_per_line),
			  sizeof(impanel->bytes_per_line));
    write(fd, (char *) &(impanel->xoffset),
			  sizeof(impanel->xoffset));
    write(fd, (char *) &(impanel->depth),
			  sizeof(impanel->depth));
    /* write the rasters, one line atta time */
    dpoint = impanel->data;
    for (i = 0; i < height; i++) {
        write(fd, dpoint, width);
        write(fd, dpoint, impanel->bytes_per_line);
        dpoint += impanel->bytes_per_line;
    }
/*   another way of writing data
        write(fd, impanel->data, impanel->bytes_per_line*height);
*/

    close(fd);
    XDestroyImage(impanel);

    return 0;
}

/* The saved panel associated with "name" is restored. */
int Panel_restore (char *name)
{
    int fd, i;
    int top, left, width, height, bytes_per_line, xoffset, depth;
    char *data, *tdata; /* , *G_malloc(); */
    XImage *newimage;
    XWindowAttributes xwa;

    /* open file, read the dimensions and location */
    if ((fd = open(name, 0)) == 0) {
        fprintf(stderr, "Cannot open panel %s\n", name);
        return (-1);
    }
    read(fd, (char *) &left, sizeof(left));
    read(fd, (char *) &top, sizeof(top));
    read(fd, (char *) &width, sizeof(width));
    read(fd, (char *) &height, sizeof(height));
    read(fd, (char *) &bytes_per_line, sizeof(bytes_per_line));
    read(fd, (char *) &xoffset, sizeof(xoffset));
    read(fd, (char *) &depth, sizeof(depth));

    /* allocate space and read the data points */
    /*
    data = (char *) G_malloc((unsigned) width * height);
    */
    data = (char *) G_malloc((size_t) (bytes_per_line * height));
    /*   another way of reading data
    read(fd, (char *) &data, bytes_per_line * height);
    */
    tdata = data;
    for (i = 0; i < height; i++) {
        read(fd, tdata, width);
        read(fd, tdata, bytes_per_line);
        tdata += bytes_per_line;
    }
    close(fd);

    /* now that data is in memory, get the window's attributes and turn
     * it into an image, then draw it. */
    if (XGetWindowAttributes(dpy, grwin, &xwa) == 0)
        return (-1);
/*
    newimage = XCreateImage(dpy, xwa.visual, 8, ZPixmap, 0,
            data, width, height, 8, width);
*/
    newimage = XCreateImage(dpy, xwa.visual, depth, ZPixmap, xoffset,
            data, width, height, 8, bytes_per_line);
    XPutImage(dpy, bkupmap, gc, newimage, 0, 0, left, top, width, height);
    XDestroyImage(newimage);
    needs_flush = 1;
    return 1;
}

/* The saved panel associated with "name" is deleted. */
int Panel_delete (char *name)
{
    unlink(name);
    return 0;
}
