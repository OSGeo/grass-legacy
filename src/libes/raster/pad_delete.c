#include "raster.h"
#include "graph.h"

int R_pad_delete ()
{
    char result;

    _hold_signals(1);

    _send_ident (PAD_DELETE);
    _get_char (&result);

    _hold_signals(0);

    return result ;
}
