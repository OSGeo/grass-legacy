#include "raster.h"
#include "graph.h"

int R_pad_append_item ( char *item, char *value, int replace)
{
    char result;

    _hold_signals(1);

    _send_ident (PAD_APPEND_ITEM);
    _send_text (item);
    _send_text (value);
    _send_int (&replace);
    _get_char (&result);

    _hold_signals(0);

    return result;
}
