#include <string.h>
#include <stdio.h>
#include <grass/gis.h>
#include <grass/glocale.h>
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
    {
        G_warning(_("%s[%d]: No raster file specified."), __FILE__, __LINE__);

        return -1;
    }

    if (G_read_history(raster_name, G_mapset(), &hist) < 0)
	return -1;

    /* write command line to history */
    sprintf(hist.edhist[0], "%s version %.2f", G_program_name(), APP_VERSION);
    sprintf(hist.datsrc_1, "raster elevation file: %s", raster_name);
    hist.edlinecnt = 1;
    G_command_history(&hist);

    return G_write_history(raster_name, &hist);
}
