#include <stdio.h>
#include "includes.h"

extern int SCREEN_TOP;
extern int SCREEN_BOTTOM;
extern int SCREEN_LEFT;
extern int SCREEN_RIGHT;

extern Display *dpy;
extern Window grwin;
extern GC gc;
extern Pixmap bkupmap;
extern int backing_store;

/* Saves all bit plane information for the screen area described by
 * top, bottom, left, and right borders.  Associates the saved
 * information with the string "name".  This name is a local system
 * file name which is actually be used to store the image. */
Panel_save(name, top, bottom, left, right)
char *name;
int top, bottom, left, right;
{
    int fd;
    int width, height, i;
    XImage *impanel;
    char *dpoint;

    /* Adjust panel edges if outside window necessary */
    if (top < SCREEN_TOP)
        top = SCREEN_TOP;
    if (bottom > SCREEN_BOTTOM)
        bottom = SCREEN_BOTTOM;
    if (left < SCREEN_LEFT)
        left = SCREEN_LEFT;
    if (right > SCREEN_RIGHT)
        right = SCREEN_RIGHT;

    height = bottom - top + 1;
    width = right - left+1;

    /* Get the image off the window */
    if (!backing_store)
        impanel = XGetImage(dpy, bkupmap, left, top, width, height,
                AllPlanes, ZPixmap);
    else
        impanel = XGetImage(dpy, grwin, left, top, width, height,
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
}

/* The saved panel associated with "name" is restored. */
Panel_restore(name)
char *name;
{
    int fd, i;
    int top, left, width, height, bytes_per_line, xoffset, depth;
    char *data, *tdata, *malloc();
    XImage *newimage;
    XWindowAttributes xwa;

    /* open file, read the dimensions and location */
    if ((fd = open(name, 0)) == NULL) {
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
    data = malloc((unsigned) width * height);
    */
    data = malloc((unsigned) bytes_per_line * height);
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
    XPutImage(dpy, grwin, gc, newimage, 0, 0, left, top, width, height);
    if (!backing_store)
        XPutImage(dpy, bkupmap, gc, newimage, 0, 0, left, top,
                width, height);
    XDestroyImage(newimage);
    return 1;
}

/* The saved panel associated with "name" is deleted. */
Panel_delete(name)
char *name;
{
    unlink(name);
}
