/*
 * Close down the graphics processing.  This gets called only at driver
 * termination time.
 */
#include "cell.h"
#include "gis.h"
Graph_Close()
{
    struct Cell_head Window;
    CELL *Cellbuf;
    int newmap;

    /* now copy temp file into cell file */
    /* and write color table */


#ifdef NEWCODE
    fseek (Temp_fp, 0L, 0);
    Window.north=SCREEN_NORTH;
    Window.east=SCREEN_EAST;
    Window.south=0;
    Window.west=0;
    Window.ew_res=1;
    Window.ns_res=1;

    G_set_window (&Window);

    Cellbuf = G_alloc_cell_buf ();
    if ((newmap = G_open_cell_new (Filename)) == -1)
	fprintf (stderr, "Error creating cell file '%s'\n", Filename), exit(-1);
    

    for (row = 0 ; row < SCREEN_NORTH ; row++)
    {
	if (0 >= fread (Row_buf, 1, SCREEN_EAST, Temp_fp))
	{
	    fprintf (stderr, "Error reading tmp file in CELL driver\n");
	    break;
	}
	for (i = 0 ; i < SCREEN_EAST ; i++)
	    Cellbuf[i] = Row_buf[i];
	G_put_map_row (newmap, Cellbuf);
    }

#endif
    fclose (Temp_fp);
}
