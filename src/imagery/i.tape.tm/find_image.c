#include "tape.h"
find_image (band, row)
{
    int b;
    int r;

    do
    {
	if (tape.record_type == IMAGE_DATA)
	{
	    b = number (17,20);	/* get band number */
	    r = number (13,16);	/* get row */

	    if (b == band && r == row)
		    return (1);
	    if ((b > band) || (b == band && r > row))
	    {
		printf("** WARNING: band %d row %d missing from tape\n", band, row);
		return 0;
	    }
	}
    }
    while( read_tape(1) );
    return 0;
}
