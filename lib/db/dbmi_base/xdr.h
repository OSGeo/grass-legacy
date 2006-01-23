#ifdef __MINGW32__
#include <rpc/types.h>
#include <rpc/xdr.h>
#else
#include <rpc/rpc.h>
#endif

#include "dbmi.h"

int xdr_begin_send(XDR *xdrs);
int xdr_begin_recv(XDR *xdrs);
int xdr_end_send(XDR *xdrs);
int xdr_end_recv(XDR *xdrs);
