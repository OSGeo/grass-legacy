#include "raster.h"
#include "graph.h"

int R_pad_select (char *pad)
{
    char result;

    _hold_signals(1);

    _send_ident (PAD_SELECT);
    _send_text (pad);
    _get_char (&result);

    _hold_signals(0);

    return result;
}
