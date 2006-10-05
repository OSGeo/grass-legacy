#include "xdr.h"


int
db__send_char(int d)
{
    XDR xdrs;
    int stat;
    char c;

    stat = DB_OK;
    c = d;

    xdr_begin_send (&xdrs);
    if(!xdr_char (&xdrs, &c))
	stat = DB_PROTOCOL_ERR;
    xdr_end_send (&xdrs);

    if (stat == DB_PROTOCOL_ERR)
	db_protocol_error();

    return stat;
}


int
db__recv_char (char *d)
{
    XDR xdrs;
    int stat;

    stat = DB_OK;
    xdr_begin_recv (&xdrs);
    if(!xdr_char (&xdrs, d))
	stat = DB_PROTOCOL_ERR;
    xdr_end_recv (&xdrs);

    if (stat == DB_PROTOCOL_ERR)
	db_protocol_error();

    return stat;
}
