/*
 *
 * Panel.c
 *
 * functios for saving and restoring rectangular areas (panel) of
 * the graphics display. 
 *
 * DMJ - GRASS expects an 8 bit-plane graphics display.  The OKI 
 * has a 16 bit-plane display.  I made modifications to the panel.c
 * functions to account for the extra bit-planes.  All modifications
 * are marked with a "DMJ"
 * 
 */
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
    int width, height, i;
    XImage *impanel;
    char *dpoint;
    int fd;

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
        /*
         * DMJ - modified the write statement to write twice
         * as much data to account for the 16-bit display 
         */ 
        write(fd, dpoint, width * 2);
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

    /* DMJ - multiplied the space allocated by 2 to account
     * for the 16 bit display
     */ 
    data = malloc((unsigned) width * height * 2 );
    tdata = data;
    for (i = 0; i < height; i++) 
        {
        /* DMJ - and multiplied the bytes written by 2 */ 
        read(fd, tdata, width*2);
        tdata += (width*2);
        }
    close(fd);

    /* now that data is in memory, get the window's attributes and turn
     * it into an image, then draw it. */
    if (XGetWindowAttributes(dpy, grwin, &xwa) == 0)
        return (-1);

    /* DMJ - modified the XCreateImage call to account for 16 bit planes */ 
    newimage = XCreateImage(dpy, xwa.visual, 16, ZPixmap, 0,
            data,width,height,32,0);

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
