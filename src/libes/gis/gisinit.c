/**********************************************************************
 *
 *   G_gisinit(pgm)
 *      char *pgm        Name to be associated with current program
 *
 *  Does some program initialization.  Read comments in this file
 *  for details.
 **********************************************************************/

#include <stdio.h>
#include "G.h"
#include "version.h"

struct G__ G__ ;
static int initialized = 0;
static gisinit();

CELL CELL_NODATA; /* defined in gis.h */

G_gisinit(pgm)
    char *pgm;
{
    char *G_store() ;
    char *mapset;
    char msg[100];

    G_set_program_name (pgm);

    CELL_NODATA = 0;	/* 0 for the moment */

/* Make sure location and mapset are set */
    G_location_path();
    switch (G__mapset_permissions (mapset = G_mapset()))
    {
    case 1:
	    break;
    case 0:
	    sprintf(msg,"MAPSET %s - permission denied", mapset);
	    G_fatal_error (msg);
	    exit(-1);
	    break;
    default:
	    sprintf(msg,"MAPSET %s not found", mapset);
	    G_fatal_error (msg);
	    exit(-1);
	    break;
    }

    gisinit();
}

G_no_gisinit()
{
    gisinit();
}

G__check_gisinit()
{
    if (initialized) return 1;
    fprintf (stderr, "\7ERROR: System not initialized. Programmer forgot to call G_gisinit()\n");
    sleep(3);
    exit(-1);
}

static
gisinit()
{
    int i ;

/* Mark window as not set */
    G__.window_set = 0 ;

/* no histograms */
    G__.want_histogram = 0;

/* Mark all cell files as closed */
    for (i = 0; i < MAXFILES; i++)
    {
	G__.fileinfo[i].open_mode = -1;
    }

/* Set compressed data buffer size to zero */
    G__.compressed_buf_size = 0;
    G__.work_buf_size = 0;

/* Set masking flag unknown */
    G__.auto_mask = -1 ;

    initialized = 1;
    umask(022);
}
