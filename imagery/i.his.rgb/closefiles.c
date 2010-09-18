#include <stdlib.h>
#include <grass/gis.h>
#include "globals.h"

/* This routine closes up the cell maps, frees up the row buffers and
   use a less than perfect way of setting the color maps for the output
   to grey scale.  */

int closefiles(char *r_name, char *g_name, char *b_name,
	       int fd_output[3], CELL * rowbuf[3])
{
    int i;
    struct Colors colors;
    struct Range range;
    struct History history;
    CELL min, max;
    char *mapset;

    for (i = 0; i < 3; i++) {
	G_close_cell(fd_output[i]);
	G_free(rowbuf[i]);
    }

    mapset = G_mapset();

    /* write colors */
    /*   set to 0,max_level instead of min,max ?? */
    G_read_range(r_name, mapset, &range);
    G_get_range_min_max(&range, &min, &max);
    G_make_grey_scale_colors(&colors, min, max);
    G_write_colors(r_name, mapset, &colors);

    G_read_range(g_name, mapset, &range);
    G_get_range_min_max(&range, &min, &max);
    G_make_grey_scale_colors(&colors, min, max);
    G_write_colors(g_name, mapset, &colors);

    G_read_range(b_name, mapset, &range);
    G_get_range_min_max(&range, &min, &max);
    G_make_grey_scale_colors(&colors, min, max);
    G_write_colors(b_name, mapset, &colors);

    /* write metadata */
    G_short_history(r_name, "raster", &history);
    G_command_history(&history);
    G_write_history(r_name, &history);
    G_put_cell_title(r_name, "Image red");

    G_short_history(g_name, "raster", &history);
    G_command_history(&history);
    G_write_history(g_name, &history);
    G_put_cell_title(g_name, "Image green");

    G_short_history(b_name, "raster", &history);
    G_command_history(&history);
    G_write_history(b_name, &history);
    G_put_cell_title(b_name, "Image blue");

    return 0;
}
