

/*---------- Function: raster ----------*/
#include <stdio.h>
extern int color_pats[];
extern unsigned char bit_vals[2][5][8];
raster_int(left_x, upper_y, numrows, numpix, colors, withzeros)

/* This function is passed the upper left coordinates of the rectangular 
   area to be filled with cells, the number of pixels vertically, the
   number of pixels horizontally, and a pointer to an array that contains 
   the color pattern numbers for each cell. 
                                            Paul W. Carlson  August, 1987
*/

int left_x; 			/* upper left x coordinate            */
int upper_y; 			/* upper left y coordinate            */
int numrows;
int numpix;			/* number of pixels horizontally      */
unsigned int *colors;		/* array of cell color pattern numbers*/
int withzeros;			/* overlay flag			      */
{
    register int x, y;		/* screen coordinates                 */
    register int pattern;	/* the current color pattern number   */
    register int xpos;          /* x position in byte (0 = high bit)  */
    register int ypos;		/* 0 if even line, 1 if odd line      */
    int right_x;		/* right x coordinate                 */
    int lower_y;		/* lower y coordinate                 */
    unsigned char redbyte;	/* byte sent to red bitplane          */
    unsigned char grnbyte;	/* byte sent to green bitplane        */
    unsigned char blubyte;	/* byte sent to blue bitplane         */

    /* Compute the lower right coordinates. */

    right_x = left_x + numpix - 1;
    lower_y = upper_y + numrows - 1;

    /* Send code for rectangular array of cells,  the upper left 
       coordinates, and the lower right coordinates. */

    if (withzeros) put_chr('R');
    else put_chr('X');
    put_int(left_x);
    put_int(upper_y);
    put_int(right_x);
    put_int(lower_y);

    /* Compute and send bytes to the three bitplanes. */

    redbyte = grnbyte = blubyte = 0;
    for (y = upper_y; y <= lower_y; y++)
    {	ypos = y & 1;
        for (x = left_x; x <= right_x; x++)
	{   pattern = color_pats[*(colors + x - left_x)];
	    xpos = x & 7;
            redbyte |= bit_vals[ypos][pattern & 7][xpos];
            grnbyte |= bit_vals[ypos][(pattern >> 3) & 7][xpos];
            blubyte |= bit_vals[ypos][(pattern >> 6) & 7][xpos];
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
