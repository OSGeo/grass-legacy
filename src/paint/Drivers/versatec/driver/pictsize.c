#include "P.h"
Ppictsize (r, c)
{
    char *malloc();

    print_rasterfile();
    ncols = c;
    if (ncols < TEXT_COLS)
	ncols = TEXT_COLS;
    if (ncols%2) ncols++;	/* for sun loadscreen compatibility */
    if (data_buf) free (data_buf);
    data_buf = (unsigned char *) malloc (ncols);
}
