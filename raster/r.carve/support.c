#include <string.h>
#include <stdio.h>
#include <grass/gis.h>
#include "enforce.h"


/*
 * update_history - Update a history file.  Some of the digit file 
 * information is placed in the history file.
 * returns  0  -  successful creation of history file
 *         -1  -  error
 */
int update_history(char *raster_name)
{
    struct History hist;

    if (raster_name == NULL)
        return -1;

    if (G_read_history(raster_name, G_mapset(), &hist) < 0)
	return -1;

    /* write command line to history */
    G_short_history(raster_name, "raster", &hist);
    sprintf(hist.edhist[0], "%s version %.2f", G_program_name(), APP_VERSION);
    sprintf(hist.edhist[1], "%s", G_recreate_command());
    sprintf(hist.datsrc_1, "raster elevation file: %s", raster_name);
    hist.edlinecnt = 2;

    return G_write_history(raster_name, &hist);
}
