/* #ifndef NOT_SUN
 * # include <rasterfile.h>
 * #else
 */
# define RAS_MAGIC 0x59a66a95
# define RT_STANDARD 1
# define RT_BYTE_ENCODED 2
# define RMT_EQUAL_RGB 1
/* #endif */

#include <stdio.h>

static long width_offset, height_offset, length_offset, data_offset;

long lseek();
#define where(fd) lseek(fd, 0L, 1)

initialize_rasterfile(fd)
{
    unsigned char red[256], grn[256], blu[256];
    float r,g,b;
    int n, ncolors;

/* build STANDARD rasterfile header */

    put_int (fd, RAS_MAGIC);       /* rasterfile magic number */

    width_offset = where(fd);
    put_int (fd, 0);               /* ras_width */

    height_offset = where(fd);
    put_int (fd, 0);               /* ras_height */

    put_int (fd, 8);               /* ras_depth */

    length_offset = where(fd);
    put_int(fd, 0);                /* ras_length */

    put_int(fd,RT_STANDARD);       /* ras_type */


/* create the color table */

    ncolors = Pncolors();
    for (n = 0; n < ncolors; n++)
    {
	Pcolorvalue (n, &r, &g, &b);
	red[n] = (int) (r * 255);
	grn[n] = (int) (g * 255);
	blu[n] = (int) (b * 255);
/*fprintf (stderr, "%d: %d %d %d\n", n, red[n], grn[n], blu[n]);*/
    }
    if (ncolors < 256)
    {
	ncolors = 256;
	while (n < ncolors)
	{
	    red[n] = grn[n] = blu[n] = 255;
	    n++;
	}
    }

/* write the colortable */

    put_int (fd, RMT_EQUAL_RGB);   /* ras_maptype */
    put_int (fd, 3*ncolors);       /* ras_maplength */
    write (fd, red, ncolors);
    write (fd, grn, ncolors);
    write (fd, blu, ncolors);

    data_offset = where(fd);
}
put_int (fd, n)
{
    int x;
    x = n; /* compiler paranoia */
    write (fd, &x, sizeof x);
}

set_rasterfile_size (fd, rows, cols)
{
    lseek (fd, width_offset, 0);
    put_int (fd, cols);
    lseek (fd, height_offset, 0);
    put_int (fd, rows);
    lseek (fd, length_offset, 0);
    put_int (fd, rows*cols);
    lseek (fd, data_offset, 0);
}
