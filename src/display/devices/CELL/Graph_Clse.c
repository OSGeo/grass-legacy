/*
 * Close down the graphics processing.  This gets called only at driver
 * termination time.
 */
#include <stdlib.h>

#include "config.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "cell.h"
#include "gis.h"


extern int screen_top;
extern int screen_left;
extern int screen_bottom;
extern int screen_right;

int Graph_Close (void)
{
    struct Cell_head Window;
    CELL *Cellbuf;
    struct Colors Color;
    int newmap;
    int row, i;

    /* now copy temp file into cell file */
    /* and write color table */

    fseek (Temp_fp, 0L, 0);
    Window.north=screen_bottom;
    Window.east=screen_right;
    Window.south=screen_top;
    Window.west=screen_left;
    Window.proj=0;
    Window.zone=0;
    Window.ew_res=1;
    Window.ns_res=1;

    G_set_window (&Window);

    Cellbuf = G_allocate_cell_buf ();
    if ((newmap = G_open_cell_new (FILE_NAME)) == -1)
	fprintf (stderr, "Error creating cell file '%s'\n", Filename), exit(-1);
    

    for (row = screen_top; row < screen_bottom; row++)
    {
	if (0 >= fread (Row_buf, 1, screen_right - screen_left, Temp_fp))
	{
	    fprintf (stderr, "Error reading tmp file in CELL driver\n");
	    break;
	}
	for (i = 0 ; i < screen_right - screen_left ; i++)
	    Cellbuf[i] = Row_buf[i];
	G_put_raster_row (newmap, Cellbuf, CELL_TYPE);
    }

    G_close_cell (newmap);

    /* and now color table */
    G_init_colors (&Color);
    for (i = 0 ; i < 256 ; i++)
    {
	G_set_color (i, Color_table[i][0], Color_table[i][1], Color_table[i][2],
	    &Color);
    }
    G_write_colors (FILE_NAME, G_mapset(), &Color);

    unlink (Filename);
    fclose (Temp_fp);

    return 0;
}
