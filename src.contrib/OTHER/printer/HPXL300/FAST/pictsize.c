#include "P.h"
Ppictsize (nrows, ncols)
{
	/* Set global variable for width resolution to be used by Praster */
	ncolumns = ncols;
	if (ncols > maxcolumns) {
		printf("%d exceeds width of %d",ncols,maxcolumns);
	}
}
