#include "P.h"

Praster ()
{
    ras_row = 0;
    flush_raster();
}

end_raster ()
{
    flush_raster();
}

flush_raster()
{
    if (ras_row)
	esc ("A"); /* output the micro line */
    ras_row = 0;
}
