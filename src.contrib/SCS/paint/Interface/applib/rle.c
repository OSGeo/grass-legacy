#include "interface.h"

static unsigned char value;
static int count ;
static unsigned char *dbuf, *rbuf ;
static int ncols = 0 ;
static int nd,nr ;
static int as_data ;

Prle_begin()
{
    count = 0;
    value = 0;
    nr = nd = 0;
    as_data = 0 ;
}

Prle (v, n)
    unsigned char v;
{
    if (value != v)
    {
	rle_flush();
	value = v;
	count = n;
    }
    else
	count += n;
}

Prle_end()
{

    rle_flush();

    if (as_data)
    {
	P__opcode (DATA);
	P__send (dbuf, nd);
    }
    else
    {
	P__opcode (RLE);
	P__send (rbuf, nr);
    }
}

Prle_set_cols (nc)		/* allocate rle buffer and data buffer */
{
    char *malloc();
    if (nc > ncols)
    {
	if (ncols)
	    free (dbuf);
	dbuf = (unsigned char *) malloc (nc + nc + 1);
	rbuf = dbuf + nc;
	ncols = nc;
    }
}

static
rle_flush()
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
}
