#include "P.h"
Prle (buf, n)
    unsigned char *buf;
{
    int col;
    int c, y, m ;
    unsigned char value, repeat;
    unsigned char rmask;
    unsigned char *cmask;
    unsigned char mask_c;
    unsigned char mask_y;
    unsigned char mask_m;
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

/*
fprintf (stderr,"row(%d) rmask(%03o)\n",ras_row, rmask);
fprintf (stderr,"cmask( ");
for(col=0; col < 5; col++) fprintf (stderr,"%03o ",cmask[col]);
fprintf (stderr,")\n");
*/

    col = 0;
    while (n-- > 0)
    {
	repeat = *buf++;
	if((value = *buf++) > 124)
	    value = 0;
	y = yellow_lookup[value];
	c = cyan_lookup[value];
	m = magenta_lookup[value];


	mask_c = cmask[c];
	mask_y = cmask[y];
	mask_m = cmask[m];

/*
fprintf(stderr,"c(%d=%03o) y(%d=%03o) m(%d=%03o)\n",c,mask_c,y,mask_y,m,mask_m);
*/

	while (repeat-- > 0)
	{
	    bitmask = 1 << (col & 7);
/*
fprintf (stderr,"bitmask(%03o) y(%d) c(%d) m(%d)\n",bitmask, mask_y & bitmask, mask_c & bitmask, mask_m & bitmask);
*/

	    if (mask_y & bitmask)
		YELLOW[col] |= rmask ;
	    if (mask_m & bitmask)
		MAGENTA[col] |= rmask ;
	    if (mask_c & bitmask)
		CYAN[col] |= rmask ;
	    col++;
	}
    }
    ras_row++;
    if (ras_row == ras_nrows)
	flush_raster();
}
