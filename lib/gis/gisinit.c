/**********************************************************************
 *
 *   G_gisinit(pgm)
 *      char *pgm        Name to be associated with current program
 *
 *  Does some program initialization.  Read comments in this file
 *  for details.
 **********************************************************************/

#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>
#include "gis.h"
#include "G.h"
#include "glocale.h"

struct G__ G__ ;
static int initialized = 0;
static int gisinit();

int G_gisinit( char *pgm)
{
    char *mapset;
    char msg[100];

    if ( initialized )
	return 0;

    G_set_program_name (pgm);

   /* Make sure location and mapset are set */
    G_location_path();
    switch (G__mapset_permissions (mapset = G_mapset()))
    {
    case 1:
	    break;
    case 0:
	    sprintf(msg,_("MAPSET %s - permission denied"), mapset);
	    G_fatal_error (msg);
	    exit(-1);
	    break;
    default:
	    sprintf(msg,_("MAPSET %s not found"), mapset);
	    G_fatal_error (msg);
	    exit(-1);
	    break;
    }

    gisinit();

    return 0;
}

int G_no_gisinit(void)
{
    if ( initialized )
	return 0;

    gisinit();

    return 0;
}

int G__check_gisinit()
{
    if (initialized) return 1;
    fprintf (stderr, _("\7ERROR: System not initialized. Programmer forgot to call G_gisinit()\n"));
    sleep(3);
    exit(-1);
}

static int gisinit()
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
    G__.null_buf_size = 0;
    G__.mask_buf_size = 0;
    G__.temp_buf_size = 0;
    /* mask buf we always want to keep allocated */
    G__reallocate_mask_buf();

/* set the write type for floating maps */
    G__.fp_type = FCELL_TYPE;
    G__.fp_nbytes = XDR_FLOAT_NBYTES;

/* Set masking flag unknown */
    G__.auto_mask = -1 ;

/* set architecture dependant bit patterns for embeded null vals */
    G__init_null_patterns();

    initialized = 1;
    umask(022);

    return 0;
}
