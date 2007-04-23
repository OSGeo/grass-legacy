/****************************************************************************
 *
 * MODULE:       dbmi_base
 * AUTHOR(S):    Radim Blazek <radim.blazek gmail.com> (original contributor)
 *               Brad Douglas <rez touchofmadness.com>, 
 *               Markus Neteler <neteler itc.it>
 * PURPOSE:      database management functions for modules and drivers
 * COPYRIGHT:    (C) 2003-2006 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/
#include "xdr.h"

static FILE *_send, *_recv;

void
db__set_protocol_fds (FILE *send, FILE *recv)
{
    _send = send;
    _recv = recv;
}

int
xdr_begin_send(XDR *xdrs)
{
    xdrstdio_create (xdrs, _send, XDR_ENCODE);

    return 0;
}

int
xdr_begin_recv(XDR *xdrs)
{
    xdrstdio_create (xdrs, _recv, XDR_DECODE);

    return 0;
}

int
xdr_end_send(XDR *xdrs)
{
    fflush(_send);
    xdr_destroy (xdrs);

    return 0;
}

int
xdr_end_recv(XDR *xdrs)
{
    xdr_destroy (xdrs);

    return 0;
}



