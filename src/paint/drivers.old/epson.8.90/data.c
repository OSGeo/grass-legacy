#include "P.h"
Pdata (buf, n)
    unsigned char *buf;
{
    int col;
    int c, y, m ;
    unsigned char value;
    unsigned char rmask;
    unsigned char *cmask;
    unsigned char bitmask;

    extern unsigned char col_mask[2][];
    extern unsigned char cyan_lookup[];
    extern unsigned char yellow_lookup[];
    extern unsigned char magenta_lookup[];


/*
    rmask = 1 << (7 - ((ras_row & 3) << 1));
*/
    rmask = 1 << (7 - ((ras_row & 3)));
    cmask = col_mask[ras_row & 1];

    for (col = 0; n-- > 0; col++)
    {
	if((value = *buf++) > 124)
	    value = 0;

	bitmask = 1 << (col & 7);
	y = yellow_lookup[value];
	c = cyan_lookup[value];
	m = magenta_lookup[value];

	if (cmask[y] & bitmask)
	    YELLOW[col] |= rmask ;
	if (cmask[m] & bitmask)
	    MAGENTA[col] |= rmask ;
	if (cmask[c] & bitmask)
	    CYAN[col] |= rmask ;
    }
    ras_row++;
    if (ras_row == ras_nrows)
	flush_raster();
}
