/***************************************************************

NAME:		image_fmt()

FUNCTION:	compute left fill, pixels, right fill for fully
		corrected data image

USAGE:		image_fmt (buf, lfill, pixels, rfill)

		unsigned char buf[];
		int *lfill
		int *pixels
		int *rfill
*****************************************************************/

image_fmt (buf, lfill, pixels, rfill)
    unsigned char buf[];
    int *lfill;
    int *pixels;
    int *rfill;
{
    *lfill = ((int)buf[9] * 16) + ((int)buf[10] >> 4);
    *rfill = ((int)(buf[10] & 017) * 256) + ((int)buf[11]);

    *pixels = 3560 - 12 - *lfill - *rfill;
}
