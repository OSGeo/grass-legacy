/* Saves all bit plane information for the screen area 
 * described by top, bottom, left, and right borders.  Associates
 * the saved information with the string "name".  This name is a
 * local system file name which may actually be used to store the
 * image.   The last part of the name can be parsed off and used as
 * a pointer name to the saved image.
 */

#include <unistd.h>
#include <fcntl.h>

#include "png.h"

int Panel_save(char *name, int top, int bottom, int left, int right)
{
    int width, height;
    int bytes, xoffset, depth;
    int fd;
    int i;

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

    bytes = true_color ? width * 4 : width;
    xoffset = 0;
    depth = true_color ? 32 : 8;

    /* open the file */
    fd = creat(name, 0644);
    if (fd < 0)
    {
	perror("unable to create panel file");
	return -1;
    }

    /* write the header */
    write(fd, &left,    sizeof(left));
    write(fd, &top,     sizeof(top));
    write(fd, &width,   sizeof(width));
    write(fd, &height,  sizeof(height));
    write(fd, &bytes,   sizeof(bytes));
    write(fd, &xoffset, sizeof(xoffset));
    write(fd, &depth,   sizeof(depth));

    /* write the data */
    for (i = 0; i < height; i++)
    {
	void *ptr = true_color
	    ? (void *) im->tpixels[i]
	    : (void *) im->pixels[i];
	write(fd, ptr, bytes);
    }

    close(fd);

    return 0;
}

int Panel_restore(char *name)
{
    int left, top, width, height;
    int bytes, xoffset, depth;
    int fd;
    int i;

    /* open the file */
    fd = open(name, O_RDONLY);
    if (fd < 0)
    {
	perror("unable to open panel file");
	return -1;
    }

    /* read the header */
    read(fd, &left,    sizeof(left));
    read(fd, &top,     sizeof(top));
    read(fd, &width,   sizeof(width));
    read(fd, &height,  sizeof(height));
    read(fd, &bytes,   sizeof(bytes));
    read(fd, &xoffset, sizeof(xoffset));
    read(fd, &depth,   sizeof(depth));

    /* read the data */
    for (i = 0; i < height; i++)
    {
	void *ptr = true_color
	    ? (void *) im->tpixels[i]
	    : (void *) im->pixels[i];
	read(fd, ptr, bytes);
    }

    close(fd);

    return 0;
}

int Panel_delete(char *name)
{
    unlink(name);
    return 0;
}

