/*
 * The systems color represented by "number" is set using the color component
 * intensities found in the "red", "grn", and "blu" variables.  A value of
 * 0 represents 0 intensity; a value of 255 represents 100% intensity.
 */

#include "iris.h"

reset_color(number, red, grn, blu)
	int number ;
	unsigned char red, grn, blu ;
{
    mapcolor (number+COLOR_OFFSET, red, grn, blu);
}
