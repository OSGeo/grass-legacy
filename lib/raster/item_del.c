#include "raster.h"
#include "graph.h"

int R_pad_delete_item ( char *name)
{
    char result;

    _hold_signals(1);

    _send_ident (PAD_DELETE_ITEM);
    _send_text (name);
    _get_char (&result);

    _hold_signals(0);

    return result ;
}
