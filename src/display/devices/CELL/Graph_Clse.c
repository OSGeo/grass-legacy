/*
 * Close down the graphics processing.  This gets called only at driver
 * termination time.
 */
#include "config.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "cell.h"
#include "gis.h"


extern int SCREEN_BOTTOM;
extern int SCREEN_RIGHT;

int Graph_Close (void)
{
    struct Cell_head Window;
    CELL *Cellbuf;
    struct Colors Color;
    int newmap;
    int row, i;

    /* now copy temp file into cell file */
    /* and write color table */


#define NEWCODE
#ifdef NEWCODE
    fseek (Temp_fp, 0L, 0);
    Window.north=SCREEN_BOTTOM;
    Window.east=SCREEN_RIGHT;
    Window.south=0;
    Window.west=0;
    Window.proj=0;
    Window.zone=0;
    Window.ew_res=1;
    Window.ns_res=1;

    G_set_window (&Window);

    Cellbuf = G_allocate_cell_buf ();
    if ((newmap = G_open_cell_new (FILE_NAME)) == -1)
	fprintf (stderr, "Error creating cell file '%s'\n", Filename), exit(-1);
    

    for (row = 0 ; row < SCREEN_BOTTOM ; row++)
    {
	if (0 >= fread (Row_buf, 1, SCREEN_RIGHT, Temp_fp))
	{
	    fprintf (stderr, "Error reading tmp file in CELL driver\n");
	    break;
	}
	for (i = 0 ; i < SCREEN_RIGHT ; i++)
	    Cellbuf[i] = Row_buf[i];
	G_put_map_row (newmap, Cellbuf);
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

#endif
    unlink (Filename);
    fclose (Temp_fp);

    return 0;
}
