#include "gis.h"
#include "glob.h"
/***************************************************************
these routines manage a pool of buffers which fit the current
window. they are large enough for both CELL and double data
****************************************************************/

static char **bufs = 0;
static int nbufs = 0;
static char *used = 0;

char *
get_buffer_from_pool (n)
    int *n;
{
    int i;
    int size;

    for (i = 0; i < nbufs; i++)
	if (!used[i])
	{
	    used[i] = 1;
	    return bufs[*n = i];
	}

    i = nbufs++;
    bufs = (char **) G3d_realloc (bufs, nbufs * sizeof (*bufs));
    used = (char *) G3d_realloc (used, nbufs * sizeof (*used));

    size = sizeof(double) ;

    bufs[i] = G3d_malloc (size * current_region.cols);
    used[i] = 1;

    return bufs[*n = i];
}

return_buffer_to_pool (n)
{
    if (n < nbufs)
	used[n] = 0;
}
