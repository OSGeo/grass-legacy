#
/********************************************************

NAME:		Ppictsize ()

FUNCTION:	set picture size

USAGE:		Ppictsize (rows, cols)
**********************************************************/

#include "P.h"

Ppictsize (nr, nc)
{
    int i;
    unsigned char n ;

    Praster();

    ncols = nc;
    nrows = nr + 3;

/* must have at least 3 rows */
    if (nrows < 3)
	nrows = 3;

/* number of pixels must be a multiple of 8 */
    if (nc % 8) ncols += 8 - nc % 8; /* round up to multiple of 8 */
    padding = ncols - nc ;

    Poutc (SIZE);

    Poutc ((unsigned char) (ncols % 256));
    Poutc ((unsigned char) (ncols / 256));

    Poutc ((unsigned char) (nrows % 256));
    Poutc ((unsigned char) (nrows / 256));
}
