#include "raster.h"
#include "graph.h"

int R_pad_list ( char ***list, int *count)
{
    _hold_signals(1);

    _send_ident (PAD_LIST);
    _get_list (list, count);

    _hold_signals(0);

	return 0;
}
