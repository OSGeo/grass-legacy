#include "gis.h"
#include "cseg.h"

seg_close(sseg)
	SSEG	*sseg;
{
	segment_release (&(sseg->seg));
	close (sseg->fd);
	unlink (sseg->filename);
	return 0;
}
