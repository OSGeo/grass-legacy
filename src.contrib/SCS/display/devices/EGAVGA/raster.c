/* Function: rastrer		Paul W. Carlson		April 1990  */

#include <stdio.h>

raster(left_x, upper_y, numrows, numpix, colors)
int left_x, upper_y, numrows, numpix;
unsigned char *colors;
{
    int x, y, xpos, ypos, right_x, lower_y, bit;
    int dither, red_index, grn_index, blu_index;
    unsigned char redbyte, grnbyte, blubyte;
    static unsigned char bit_vals[] = { 128, 64, 32, 16, 8, 4, 2, 1 };
    static unsigned char dither_bits[2][5] = {{ 0,  34, 102, 221, 255 },
					      { 0, 136, 153, 119, 255 }};

    /* Compute the lower right coordinates. */
    right_x = left_x + numpix - 1;
    lower_y = upper_y + numrows - 1;

    /* Send code and coordinates */
    put_chr('R');
    put_int(left_x);
    put_int(upper_y);
    put_int(right_x);
    put_int(lower_y);

    /* Compute and send bytes for the three bitplanes. */
    redbyte = grnbyte = blubyte = 0;
    for (y = upper_y; y <= lower_y; y++)
    {	ypos = y & 1;
        for (x = left_x; x <= right_x; x++)
	{   dither = *(colors + x - left_x);
	    blu_index = dither % 5;
	    red_index = dither / 25;
	    grn_index = (dither - blu_index - 25 * red_index) / 5;
	    xpos = x & 7;
	    bit = bit_vals[xpos];
            redbyte |= (dither_bits[ypos][red_index] & bit);
            grnbyte |= (dither_bits[ypos][grn_index] & bit);
            blubyte |= (dither_bits[ypos][blu_index] & bit);
	    if (xpos == 7 || x == right_x)
            {   putc(redbyte, stdout);
                putc(grnbyte, stdout);
                putc(blubyte, stdout);
		redbyte = grnbyte = blubyte = 0;
            }
	}
    }
    fflush(stdout);
}
