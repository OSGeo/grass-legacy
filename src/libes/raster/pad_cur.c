
#include "graph.h"

R_pad_current (name)
    char *name;
{
    char result;

    _hold_signals(1);

    _send_ident (PAD_CURRENT);
    _get_char (&result);
    _get_text (name);

    _hold_signals(0);

    return result;
}
