#include <stdio.h>
#include "ramseg.h"

int
size_array (ram_seg, nrows, ncols)
	int *ram_seg, nrows, ncols;
{
	int size, segs_in_col;

	segs_in_col = ((nrows - 1) >> RAMSEGBITS) + 1;
	*ram_seg = ((ncols - 1) >> RAMSEGBITS) + 1;
	size = ((((nrows - 1) >> RAMSEGBITS) + 1) << RAMSEGBITS) *
	       ((((ncols - 1) >> RAMSEGBITS) + 1) << RAMSEGBITS);
	size -= ((segs_in_col << RAMSEGBITS) - nrows) << RAMSEGBITS;
	size -= (*ram_seg << RAMSEGBITS) - ncols;
	return (size);
}
