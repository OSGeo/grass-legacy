#include "gis.h"
#include "includes.h"

#include <stdio.h>

int Panel_save(char *name, int top, int bottom, int left, int right)
{
    Pixmap pix;
    Window root;
    int dummy;
    unsigned int depth;
    FILE *fp;
    int width, height;
    
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

    if (!XGetGeometry(dpy, bkupmap, &root, &dummy, &dummy, &dummy, &dummy, &dummy, &depth))
    {
	perror("Panel_Save: cannot get depth");
	return -1;
    }

    pix = XCreatePixmap(dpy, bkupmap, width, height, depth);
    XCopyArea(dpy, bkupmap, pix, gc, left, top, width, height, 0, 0);

    /* open the file */
    fp = fopen(name, "w");
    if (!fp)
    {
	perror("unable to create panel file");
	return -1;
    }

    fprintf(fp, "%lx %d %d %d %d\n", (unsigned long) pix, left, top, width, height);
    fclose(fp);

    return 0;
}

int Panel_restore(char *name)
{
    FILE *fp;
    unsigned long pix;
    int top, left, width, height;

    fp = fopen(name, "r");
    if (!fp)
    {
	perror("unable to open panel file");
	return -1;
    }

    if (fscanf(fp, "%lx %d %d %d %d", &pix, &left, &top, &width, &height) != 5)
    {
	fprintf(stderr, "error reading panel file\n");
	fclose(fp);
	return -1;
    }

    fclose(fp);

    XCopyArea(dpy, (Pixmap) pix, bkupmap, gc, 0, 0, width, height, left, top);

    needs_flush = 1;
    return 0;
}

int Panel_delete(char *name)
{
    FILE *fp = NULL;
    unsigned long pix = 0;

    fp = fopen(name, "r");
    if (!fp)
    {
	perror("unable to open panel file");
	goto done;
    }

    if (fscanf(fp, "%lx", &pix) != 1)
    {
	fprintf(stderr, "error reading panel file\n");
	goto done;
    }

done:
    if (fp)
	fclose(fp);

    if (pix)
	XFreePixmap(dpy, (Pixmap) pix);

    remove(name);
    return 0;
}

