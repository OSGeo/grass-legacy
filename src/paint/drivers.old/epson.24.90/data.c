#include "P.h"
Pdata (buf, n)
    unsigned char *buf;
{
    int c, y, m ;
    int shift;
    int group;
    unsigned char mask1, mask2;
    unsigned char value, repeat;
    extern unsigned char cyan_lookup[];
    extern unsigned char yellow_lookup[];
    extern unsigned char magenta_lookup[];
    unsigned char *cyan0, *yellow0, *magenta0;
    unsigned char *cyan1, *yellow1, *magenta1;

    shift = 6 - ((ras_row&3) << 1);
    group = ras_row >> 2;
    mask1 = 1 << shift;
    mask2 = 2 << shift;

    cyan0    = CYAN[0][group];
    cyan1    = CYAN[1][group];
    yellow0  = YELLOW[0][group];
    yellow1  = YELLOW[1][group];
    magenta0 = MAGENTA[0][group];
    magenta1 = MAGENTA[1][group];

    while (n-- > 0)
    {
	if((value = *buf++) > 124)
	    value = 0;

	c = cyan_lookup[value];
	y = yellow_lookup[value];
	m = magenta_lookup[value];

	switch (c)
	{
	case 4:  *cyan1 |= mask2 ; /*FALLTHROUGH*/
	case 3:  *cyan1 |= mask1 ;
	case 2:  *cyan0 |= mask2 ;
	case 1:  *cyan0 |= mask1 ;
	}
	cyan0++;
	cyan1++;

	switch (y)
	{
	case 4:  *yellow1 |= mask2 ; /*FALLTHROUGH*/
	case 3:  *yellow1 |= mask1 ;
	case 2:  *yellow0 |= mask2 ;
	case 1:  *yellow0 |= mask1 ;
	}
	yellow0++;
	yellow1++;

	switch (m)
	{
	case 4:  *magenta1 |= mask2 ; /*FALLTHROUGH*/
	case 3:  *magenta1 |= mask1 ;
	case 2:  *magenta0 |= mask2 ;
	case 1:  *magenta0 |= mask1 ;
	}
	magenta0++;
	magenta1++;
    }

    ras_row++;
    if (ras_row == ras_nrows)
	flush_raster();
}


