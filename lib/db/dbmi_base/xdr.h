#include <rpc/rpc.h>
#include "dbmi.h"

int xdr_begin_send(XDR *xdrs);
int xdr_begin_recv( XDR *xdrs);
int xdr_end_send( XDR *xdrs);
int xdr_end_recv(XDR *xdrs);
