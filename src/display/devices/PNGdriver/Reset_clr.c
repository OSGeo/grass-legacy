/*
 * The systems color represented by "number" is set using the color component
 * intensities found in the "red", "grn", and "blu" variables.  A value of
 * 0 represents 0 intensity; a value of 255 represents 100% intensity.
 */

#include "png.h"

extern int colorTable[];

int
reset_color(number, red, grn, blu)
	int number ;
	unsigned char red, grn, blu ;
{
	int ret;

	/* gdImageColorAllocate(gdImagePtr im, int r, int g, int b) */
	ret = gdImageColorAllocate(im, (int) red, (int) grn, (int) blu);
	if(ret == -1) {
		ret = gdImageColorClosest(im, (int) red, (int) grn, (int) blu);
		if(ret == -1) {
			return(1);
		}
	}
	/*fprintf(stderr,"reset_color: setter farge til %d(%d) (%d,%d,%d)\n",ret,number,(int)red,(int)grn,(int)blu);*/
	colorTable[number] = ret;
	currentColor = ret;
	return(0);
}
