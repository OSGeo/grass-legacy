#include "xdr.h"

static FILE *_send, *_recv;

void
db__set_protocol_fds (send, recv)
    FILE *send, *recv;
{
    _send = send;
    _recv = recv;
}

xdr_begin_send(xdrs)
    XDR *xdrs;
{
    xdrstdio_create (xdrs, _send, XDR_ENCODE);
}

xdr_begin_recv(xdrs)
    XDR *xdrs;
{
    xdrstdio_create (xdrs, _recv, XDR_DECODE);
}

xdr_end_send(xdrs)
    XDR *xdrs;
{
    fflush(_send);
    xdr_destroy (xdrs);
}

xdr_end_recv(xdrs)
    XDR *xdrs;
{
    xdr_destroy (xdrs);
}
