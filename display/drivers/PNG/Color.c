/*
 * Identify a color that has been set in the reset_color() (found in Reset_clr.c
 * file in this directory).  Subsequent graphics calls will use this color.
 *
 * Called by:
 *      Color() in ../lib/Color.c
 */

#include "gis.h"
#include "colors.h"
#include "pngdriver.h"

void PNG_color(int number)
{
	if (number >= NCOLORS || number < 0)
	{
		G_warning("Color: can't set color %d\n", number);
		return;
	}

	if (PNG_get_table_type() == FLOAT)
		currentColor = number;
	else if (true_color)
		currentColor = number;
	else
		currentColor = xpixels[number];
}

