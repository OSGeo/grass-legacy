
#include "graph.h"

R_pad_invent (pad)
    char *pad;
{
    _hold_signals(1);

    _send_ident (PAD_INVENT);
    _get_text (pad);

    _hold_signals(0);
}
