#include <stdio.h>
#include "rowio.h"

int rowio_setup (ROWIO *R,int fd, int nrows, int len,
                 int (*getrow)(), int (*putrow)())
{
    int i;
    char *malloc();

    R->getrow = getrow;
    R->putrow = putrow;
    R->nrows = nrows;
    R->len = len;
    R->cur = -1;
    R->buf = NULL;
    R->fd = fd;

    R->rcb = (struct ROWIO_RCB *) malloc (nrows * sizeof(struct ROWIO_RCB));
    if (R->rcb == NULL)
    {
	fprintf (stderr, "rowio_setup: out of memory\n");
	return -1;
    }
    for (i = 0; i < nrows; i++)
    {
	R->rcb[i].buf = malloc (len);
	if (R->rcb[i].buf == NULL)
	{
	    fprintf (stderr, "rowio_setup: out of memory\n");
	    return -1;
	}
	R->rcb[i].row = -1;	/* mark not used */
    }
    return 1;
}
