#ifdef __MINGW32__
#include <rpc/types.h>
#include <rpc/xdr.h>
#else
#include <rpc/rpc.h>
#endif

#include "dbmi.h"

xdr_begin_send(XDR *);
xdr_begin_recv(XDR *);
xdr_end_send(XDR *);
xdr_end_recv(XDR *);
