
#include "graph.h"

R_pad_list_items (list, count)
    char ***list;
    int *count;
{
    char result;

    _hold_signals(1);

    _send_ident (PAD_LIST_ITEMS);
    _get_char (&result);
    if (result == OK)
	_get_list (list, count);

    _hold_signals(0);

    return result ;
}
