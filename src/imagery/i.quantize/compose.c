#include "globals.h"

static int out_fd;
static int red_fd, grn_fd, blu_fd;

int compose (void)
{
    char *name, *mapset;
    struct Range Range;

    red_fd = grn_fd = blu_fd = -1;

    G_init_range (&Range);


    if (ref.red.n >= 0)
    {
	name = ref.file[ref.red.n].name;
	mapset = ref.file[ref.red.n].mapset;

	if (0 > G_read_range (name, mapset, &Range))
	    G_fatal_error ("You must create Histogram and Range files first");
	valid_range (&Range);
	

	red_fd = G_open_cell_old (name, mapset);
	if (red_fd < 0)
	    G_fatal_error ("Failed to open Red band file");
    }

    if (ref.grn.n >= 0)
    {
	name = ref.file[ref.grn.n].name;
	mapset = ref.file[ref.grn.n].mapset;

	if (0 > G_read_range (name, mapset, &Range))
	    G_fatal_error ("You must create Histogram and Range files first");
	valid_range (&Range);

	grn_fd = G_open_cell_old (name, mapset);
	if (grn_fd < 0)
	    G_fatal_error ("Failed to open Green band file");
    }

    if (ref.blu.n >= 0)
    {
	name = ref.file[ref.blu.n].name;
	mapset = ref.file[ref.blu.n].mapset;

	if (0 > G_read_range (name, mapset, &Range))
	    G_fatal_error ("You must create Histogram and Range files first");
	valid_range (&Range);

	blu_fd = G_open_cell_old (name, mapset);
	if (blu_fd < 0)
	    G_fatal_error ("Failed to open Blue band file");
    }

    median (0);

    /*
    fprintf (stderr, "percent complete: ");
	G_percent (row, nrows, 10);
    fprintf (stderr, "creating support files for %s\n", result);
    */

    return 0;
}

int valid_range (struct Range *range)
{
    if (range->max > 255 || range->min < 0)
    {
	G_fatal_error ("Data values must be 0-255 for images");
	return  0;
    }

    return 1;
}


/* 
** glue routines to tie w/ median
*/
static int cur_row = 0;

int readrgbline (CELL *red, CELL *green, CELL *blue)
{
    G_get_map_row (red_fd, red, cur_row);
    G_get_map_row (grn_fd, green, cur_row);
    G_get_map_row (blu_fd, blue, cur_row);
    cur_row++;

    if (cur_row < imageheight())
	return 1;
    else
	return 0;	/* EOF */
}

int writemappedline (CELL *scanline)
{
    G_put_raster_row (out_fd, scanline, CELL_TYPE);
#ifdef FOO
    { /*DEBUG*/ 
	int i;
	for (i = 0 ; i < imagewidth () ; i++)
	    fprintf (stderr, "%d ", scanline[i]);
	fprintf (stderr, "\n");
    } /*DEBUG*/ 
#endif
    return 0;
}

int rewind_input (void)
{
    cur_row = 0;
    return 0;
}

int setcmap (int num_colors,
    unsigned char *red, unsigned char *green, unsigned char *blue)
{
    register int i;
    struct Colors color;

    G_init_colors (&color);

    for (i = 0 ; i < num_colors ; i++)
	G_set_color (i, (int)red[i], (int)green[i], (int)blue[i], &color);
    G_write_colors (result, G_mapset(), &color);
    G_free_colors (&color);

    return 0;
}

int open_output_file (void)
{
    out_fd = G_open_cell_new (result);
    if (out_fd < 0)
	G_fatal_error ("Can't open cell file for write");

    return 0;
}

int close_output_file (void)
{
    G_close_cell (out_fd);

    return 0;
}

int imagewidth (void)
{
    static int width = 0;
    if (!width)
	return width = G_window_cols();
    else 
	return width;
}

int imageheight (void)
{
    static int height = 0;

    if (!height)
	return height = G_window_rows();
    else 
	return height;
}
