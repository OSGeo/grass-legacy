/*
 * The systems color represented by "number" is set using the color component
 * intensities found in the "red", "grn", and "blu" variables.  A value of
 * 0 represents 0 intensity; a value of 255 represents 100% intensity.
 */

extern struct vwsurf *suncolor ;
extern int NCOLORS ;

reset_color(number, red, grn, blu)
	int number ;
	int red, grn, blu ;
{
	float r, g, b ;

#define MIN .1

	r=(float)red / 256. ;
	g=(float)grn / 256. ;
	b=(float)blu / 256. ;

	r=r*(1.0-MIN) + MIN;
	g=g*(1.0-MIN) + MIN;
	b=b*(1.0-MIN) + MIN;

	if (number >= NCOLORS)
		return ;

	define_color_indices (suncolor, number, number, &r, &g, &b);
}
