
#include "graph.h"

R_pad_list (list, count)
    char ***list;
    int *count;
{
    _hold_signals(1);

    _send_ident (PAD_LIST);
    _get_list (list, count);

    _hold_signals(0);
}
