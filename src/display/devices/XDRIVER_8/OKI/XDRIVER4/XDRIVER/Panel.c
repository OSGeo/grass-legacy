#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

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
    right += 2;                 /* Grab just a little more for luck */

    /* Adjust width to an even number of pixels (whole shorts), */
    /* but remain within edges of display space */
    height = bottom - top + 1;
    width = right - left;
    if (!(width % 2))
        if (right < SCREEN_RIGHT)
            right++;
        else if (left > SCREEN_LEFT)
            left--;
        else
            right--;

    /* Get the image off the window */
    if (backing_store != Always)
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
    /* write the rasters, one line atta time */
    dpoint = impanel->data;
    for (i = 0; i < height; i++) {
        write(fd, dpoint, width);
        dpoint += impanel->bytes_per_line;
    }

    close(fd);
    XDestroyImage(impanel);
}

/* The saved panel associated with "name" is restored. */
Panel_restore(name)
char *name;
{
    int fd, i;
    int top, left, width, height;
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

    /* allocate space and read the data points */
    data = malloc((unsigned) width * height);
    tdata = data;
    for (i = 0; i < height; i++) {
        read(fd, tdata, width);
        tdata += width;
    }
    close(fd);

    /* now that data is in memory, get the window's attributes and turn
     * it into an image, then draw it. */
    if (XGetWindowAttributes(dpy, grwin, &xwa) == 0)
        return (-1);
    newimage = XCreateImage(dpy, xwa.visual, 8, ZPixmap, 0,
            data, width, height, 8, width);
    XPutImage(dpy, grwin, gc, newimage, 0, 0, left, top, width, height);
    if (backing_store != Always)
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
