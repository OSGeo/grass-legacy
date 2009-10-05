#include <stdlib.h>
#include <grass/gis.h>
#include "globals.h"

/* This routine closes up the cell maps, frees up the row buffers and
   use a less than perfect way of setting the color maps for the output
   to grey scale.  */

int closefiles(char *h_name, char *i_name, char *s_name,
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
    G_read_range(h_name, mapset, &range);
    G_get_range_min_max(&range, &min, &max);
    G_make_grey_scale_colors(&colors, min, max);
    G_write_colors(h_name, mapset, &colors);

    G_read_range(i_name, mapset, &range);
    G_get_range_min_max(&range, &min, &max);
    G_make_grey_scale_colors(&colors, min, max);
    G_write_colors(i_name, mapset, &colors);

    G_read_range(s_name, mapset, &range);
    G_get_range_min_max(&range, &min, &max);
    G_make_grey_scale_colors(&colors, min, max);
    G_write_colors(s_name, mapset, &colors);

    /* write metadata */
    G_short_history(h_name, "raster", &history);
    G_command_history(&history);
    G_write_history(h_name, &history);
    G_put_cell_title(h_name, "Image hue");

    G_short_history(i_name, "raster", &history);
    G_command_history(&history);
    G_write_history(i_name, &history);
    G_put_cell_title(i_name, "Image intensity");

    G_short_history(s_name, "raster", &history);
    G_command_history(&history);
    G_write_history(s_name, &history);
    G_put_cell_title(s_name, "Image saturation");

    return 0;
}

