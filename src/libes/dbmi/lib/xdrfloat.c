#include "xdr.h"

db__send_float(d)
    float d;
{
    XDR xdrs;
    int stat;

    stat = DB_OK;

    xdr_begin_send (&xdrs);
    if(!xdr_float (&xdrs, &d))
	stat = DB_PROTOCOL_ERR;
    xdr_end_send (&xdrs);

    if (stat == DB_PROTOCOL_ERR)
	db_protocol_error();
    return stat;
}

db__recv_float (d)
    float *d;
{
    XDR xdrs;
    int stat;

    stat = DB_OK;
    xdr_begin_recv (&xdrs);
    if(!xdr_float (&xdrs, d))
	stat = DB_PROTOCOL_ERR;
    xdr_end_recv (&xdrs);

    if (stat == DB_PROTOCOL_ERR)
	db_protocol_error();
    return stat;
}

db__send_float_array (x, n)
    float *x;
    int n;
{
    XDR xdrs;
    int i;
    int stat;

    stat = DB_OK;

    xdr_begin_send (&xdrs);

    if(!xdr_int (&xdrs, &n))
	stat = DB_PROTOCOL_ERR;
    for (i = 0; stat == DB_OK && i < n; i++)
    {
	if(!xdr_float (&xdrs, x))
	    stat = DB_PROTOCOL_ERR;
	x++;
    }

    xdr_end_send (&xdrs);

    if (stat == DB_PROTOCOL_ERR)
	db_protocol_error();
    return stat;
}

/* returns an allocated array of floats */
/* caller is responsible for free() */

db__recv_float_array (x, n)
    float **x;
    int *n;
{
    XDR xdrs;
    int i, count, stat;
    float y, *a;

    *x = NULL;
    *n = 0;

    stat = DB_OK;
    xdr_begin_recv (&xdrs);
    if (xdr_int (&xdrs, &count))
    {
	if (count <= 0)
	    stat = DB_PROTOCOL_ERR;
	a = (float *)db_calloc (count, sizeof (float));
	if (a == NULL && stat == DB_OK)
	    stat = DB_MEMORY_ERR;

	for (i = 0; i < count; i++)
	{
	    if (!xdr_float (&xdrs, &y))
	    {
		stat = DB_PROTOCOL_ERR;
		break;
	    }
	    if (a) a[i] = y;
	}
	if (stat != DB_OK)
	{
	    if (a != NULL) free(a);
	    a = NULL;
	}
    }
    else
	stat = DB_PROTOCOL_ERR;

    if (stat == DB_OK)
    {
	*x = a;
	*n = count;
    }
    else if (stat == DB_PROTOCOL_ERR)
	db_protocol_error();

    xdr_end_recv (&xdrs);
    return stat;
}
