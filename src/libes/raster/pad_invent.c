#include "raster.h"
#include "graph.h"

int R_pad_invent (char *pad)
{
    _hold_signals(1);

    _send_ident (PAD_INVENT);
    _get_text (pad);

    _hold_signals(0);

	return 0;
}
