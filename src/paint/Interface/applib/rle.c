#include "interface.h"
#include <stdlib.h>

static unsigned char value;
static int count ;
static unsigned char *dbuf, *rbuf ;
static int ncols = 0 ;
static int nd,nr ;
static int as_data ;
static int rle_flush(void);

int Prle_begin (void)
{
    count = 0;
    value = 0;
    nr = nd = 0;
    as_data = 0 ;

    return 0;
}

int Prle (unsigned char v, int n)
{
    if (value != v)
    {
	rle_flush();
	value = v;
	count = n;
    }
    else
	count += n;

    return 0;
}

int Prle_end (void)
{

    rle_flush();

    if (as_data)
    {
	P__opcode (DATA);
	P__send ((char *) dbuf, nd);
    }
    else
    {
	P__opcode (RLE);
	P__send ((char *) rbuf, nr);
    }

    return 0;
}

int Prle_set_cols (int nc)	/* allocate rle buffer and data buffer */
{
    if (nc > ncols)
    {
	if (ncols)
	    free (dbuf);
	dbuf = (unsigned char *) malloc (nc + nc + 1);
	rbuf = dbuf + nc;
	ncols = nc;
    }

    return 0;
}

static int 
rle_flush (void)
{

    while (count > 0)
    {
	if (as_data || nr >= ncols)
	{
	    if (!as_data)	/* convert current rle to data */
	    {
		register int i;
		register unsigned char repeat, v ;

		i = 0;
		while (i < nr)
		{
		    repeat = rbuf[i++];
		    v  = rbuf[i++];
		    while (repeat-- > 0)
			dbuf[nd++] = v;
		}
		as_data = 1;
	    }
	    while (count-- > 0)
		dbuf[nd++] = value;
	}
	else
	{
	    register unsigned char repeat ;

	    repeat = count > 255 ? 255 : count;
	    count -= repeat ;
	    rbuf[nr++] = repeat;
	    rbuf[nr++] = value;
	}
    }

    return 0;
}
