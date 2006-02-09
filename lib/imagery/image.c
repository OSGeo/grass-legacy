#include <stdlib.h>
#include <grass/imagery.h>


static CELL *cell;
static int ncols;
static int maskfd;
static int build_color(CELL,CELL,int,
                       unsigned char *,unsigned char *,unsigned char *);

/*******************************************************************
 *
 * I_image_colors (Ref, red_colors, grn_colors, blu_colors)
 *
 *    struct Ref *Ref                       group reference (with colors)
 *    unsigned char red_colors[256]
 *    unsigned char grn_colors[256]
 *    unsigned char blu_colors[256]
 *
 *    Fills in color GRASS graphics color tables for the red/grn/blu
 *    color tables in the Ref structure.
 *    Note: if the data range is 0-255, this operation reduces to
 *        copying the Ref color tables to the arrays, Otherwise
 *        they are translated by the index (in Ref) associated with the colors.
 *
 * I_open_image (Ref)
 *    struct Ref *Ref                       group reference (with colors)
 *
 *   Opens the color files for the group, allocates i/o buffers
 *
 * Note: Ref must already have colors, ie I_read_group_colors() must be
 *       called prior to calling these routines
 *******************************************************************/


int I_image_colors (
    struct Ref *Ref,
    unsigned char *red_colors,
    unsigned char *grn_colors,
    unsigned char *blu_colors)
{
    build_color (Ref->red.min, Ref->red.max, Ref->red.n,
		 Ref->red.table, Ref->red.index, red_colors);
    build_color (Ref->grn.min, Ref->grn.max, Ref->grn.n,
		 Ref->grn.table, Ref->grn.index, grn_colors);
    build_color (Ref->blu.min, Ref->blu.max, Ref->blu.n,
		 Ref->blu.table, Ref->blu.index, blu_colors);
    return 0;
}

int I_open_image (struct Ref *Ref)
{
    int n;

    Ref->red.fd = Ref->grn.fd = Ref->blu.fd = -1;

    if ((n = Ref->red.n) >= 0)
    {
	Ref->red.fd = G_open_cell_old (Ref->file[n].name, Ref->file[n].mapset);
	if (Ref->red.fd < 0)
	    return 0;
    }

    if ((n = Ref->grn.n) >= 0)
    {
	Ref->grn.fd = G_open_cell_old (Ref->file[n].name, Ref->file[n].mapset);
	if (Ref->grn.fd < 0)
	{
	    if (Ref->red.fd >= 0)
		G_close_cell (Ref->red.fd);
	    return 0;
	}
    }

    if ((n = Ref->blu.n) >= 0)
    {
	Ref->blu.fd = G_open_cell_old (Ref->file[n].name, Ref->file[n].mapset);
	if (Ref->blu.fd < 0)
	{
	    if (Ref->red.fd >= 0)
		G_close_cell (Ref->red.fd);
	    if (Ref->grn.fd >= 0)
		G_close_cell (Ref->grn.fd);
	    return 0;
	}
    }

/* allocate 1 CELL buffer, and the 3 data buffers */
    ncols = G_window_cols();
    cell  = G_allocate_cell_buf();

    Ref->red.buf = (unsigned char *) G_malloc (ncols);
    Ref->grn.buf = (unsigned char *) G_malloc (ncols);
    Ref->blu.buf = (unsigned char *) G_malloc (ncols);

    if (Ref->red.fd < 0)
	G_zero (Ref->red.buf, ncols);
    if (Ref->grn.fd < 0)
	G_zero (Ref->grn.buf, ncols);
    if (Ref->blu.fd < 0)
	G_zero (Ref->blu.buf, ncols);

    maskfd = G_maskfd();

    return 1;
}

int I_get_image_row (struct Ref *Ref,int row)
{
    int repeat;
    int n;

    repeat = maskfd >= 0 ? G_row_repeat_nomask (maskfd, row) : 0;

    if (Ref->red.fd >= 0)
    {
	if (G_get_map_row (Ref->red.fd, cell, row) < 0)
	    return 0;
	I_translate_image_data (cell, Ref->red.buf, Ref->red.min, Ref->red.max, Ref->red.index, ncols);
	n = G_row_repeat_nomask (Ref->red.fd, row);
	if (repeat == 0 || n < repeat)
	    repeat = n;
    }
    if (Ref->grn.fd >= 0)
    {
	if (G_get_map_row (Ref->grn.fd, cell, row) < 0)
	    return 0;
	I_translate_image_data (cell, Ref->grn.buf, Ref->grn.min, Ref->grn.max, Ref->grn.index, ncols);
	n = G_row_repeat_nomask (Ref->grn.fd, row);
	if (repeat == 0 || n < repeat)
	    repeat = n;
    }
    if (Ref->blu.fd >= 0)
    {
	if (G_get_map_row (Ref->blu.fd, cell, row) < 0)
	    return 0;
	I_translate_image_data (cell, Ref->blu.buf, Ref->blu.min, Ref->blu.max, Ref->blu.index, ncols);
	n = G_row_repeat_nomask (Ref->blu.fd, row);
	if (repeat == 0 || n < repeat)
	    repeat = n;
    }

    return repeat?repeat:1;
}

int I_translate_image_data(
    CELL *c, unsigned char *buf,
    register CELL min,register CELL max,unsigned char *index,
    int ncols)
{
    register CELL v;

    if (index != NULL)
    {
	while (ncols-- > 0)
	{
	    v = *c++;
	    if (v < min || v > max)
		*buf++ = 0;
	    else
		*buf++ = index[v - min];
	}
    }
    else
    {
	while (ncols-- > 0)
	{
	    v = *c++;
	    if (v < min || v > max)
		*buf++ = 0;
	    else
		*buf++ = v;
	}
    }
    return 1;
}

static int build_color(CELL min,CELL max, int flag,
    unsigned char *table,unsigned char *index,unsigned char *colors)
{
    CELL i;

    G_zero (colors, 256);
    if (flag < 0) return 1;

    if (min >= 0 && max <= 255)
	for (i = min; i <= max; i++)
	    colors[i] = table[i-min];
    else
	for (i = min; i <= max; i++)
	    colors[index[i-min]] = table[i-min];
    return 0;
}

int I_close_image (struct Ref *Ref)
{
    free (Ref->red.buf);
    free (Ref->grn.buf);
    free (Ref->blu.buf);
    free (cell);

    if (Ref->red.fd >= 0)
	G_close_cell (Ref->red.fd);

    if (Ref->grn.fd >= 0)
	G_close_cell (Ref->grn.fd);

    if (Ref->blu.fd >= 0)
	G_close_cell (Ref->blu.fd);
    return 0;
}
