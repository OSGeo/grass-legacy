#include "gis.h"
#include "cseg.h"

dseg_close(dseg)
	DSEG	*dseg;
{
	segment_release (&(dseg->seg));
	close (dseg->fd);
	unlink (dseg->filename);
	if (dseg->name)
	{
	    free (dseg->name);
	    dseg->name = NULL;
	}
	if (dseg->mapset)
	{
	    free (dseg->mapset);
	    dseg->mapset = NULL;
	}
	return 0;
}
