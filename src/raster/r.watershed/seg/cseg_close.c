#include "gis.h"
#include "cseg.h"

cseg_close(cseg)
	CSEG	*cseg;
{
	segment_release (&(cseg->seg));
	close (cseg->fd);
	unlink (cseg->filename);
	if (cseg->name)
	{
	    free (cseg->name);
	    cseg->name = NULL;
	}
	if (cseg->mapset)
	{
	    free (cseg->mapset);
	    cseg->mapset = NULL;
	}
	return 0;
}
