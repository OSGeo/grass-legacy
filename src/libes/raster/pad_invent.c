#include "raster.h"
#include "graph.h"

/* PAD FUNCTIONS
       The monitor has a very simple database management capabil­
       ity  which supports the windowing.  There are scratch pads
       to be written on. Each scratch pad can contain items,  and
       each  item can have a list of values.  These are NOT to be
       used by the programmer.  They are used indirectly  through
       the displaylib library calls.
 */

int R_pad_invent (char *pad)
{
    _hold_signals(1);

    _send_ident (PAD_INVENT);
    _get_text (pad);

    _hold_signals(0);

	return 0;
}
