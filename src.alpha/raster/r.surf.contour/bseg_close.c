#include "gis.h"
#include "cseg.h"

bseg_close(bseg)
	BSEG	*bseg;
{
	segment_release (&(bseg->seg));
	close (bseg->fd);
	unlink (bseg->filename);
	if (bseg->name)
	{
	    free (bseg->name);
	    bseg->name = NULL;
	}
	if (bseg->mapset)
	{
	    free (bseg->mapset);
	    bseg->mapset = NULL;
	}
	return 0;
}
