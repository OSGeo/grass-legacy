#include "P.h"

static unsigned char rowmask[8] = {0200,0100,0040,0020,0010,0004,0002,0001};
static unsigned char colmask[2][5] = {0000,0042,0146,0335,0377,
				       0000,0210,0231,0167,0377};

dither (col, cyan, yellow, magenta)
{
    unsigned char rmask, *cmask;
    unsigned char bitmask;

    rmask = rowmask[ras_row_idx];
    cmask = colmask[ras_row_parity];
    bitmask = 1 << (col&7);

    if (cmask[yellow] & bitmask)
	YELLOW[col][ras_row_grp]   |= rmask ;
    if (cmask[cyan] & bitmask)
	CYAN[col][ras_row_grp]     |= rmask ;
    if (cmask[magenta] & bitmask)
	MAGENTA[col][ras_row_grp]  |= rmask ;
}
