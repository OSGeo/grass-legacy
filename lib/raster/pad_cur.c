#include "raster.h"
#include "graph.h"

int R_pad_current ( char *name)
{
    char result;

    _hold_signals(1);

    _send_ident (PAD_CURRENT);
    _get_char (&result);
    _get_text (name);

    _hold_signals(0);

    return result;
}
