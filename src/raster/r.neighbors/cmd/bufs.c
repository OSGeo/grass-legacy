#include "gis.h"
#include "ncb.h"

/*
   allocate the i/o bufs

   the i/o bufs will be rotated by the read operation so that the
   last row read will be in the last i/o buf

 */

allocate_bufs()
{
    int i;
    int bufsize;

    bufsize = (G_window_cols() + 2 * ncb.nsize) * sizeof (CELL);

    ncb.buf   = (CELL **) G_malloc (ncb.nsize * sizeof(CELL *));
    for (i = 0; i < ncb.nsize; i++)
    {
	ncb.buf[i] = (CELL *) G_malloc (bufsize) ;
	G_zero (ncb.buf[i], bufsize);
    }
}

rotate_bufs()
{
    CELL *temp;
    int i;

    temp = ncb.buf[0];

    for (i = 1; i < ncb.nsize; i++)
	ncb.buf[i-1] = ncb.buf[i];
    
    ncb.buf[ncb.nsize-1] = temp;
}
