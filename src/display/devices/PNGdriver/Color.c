/*
 * Identify a color that has been set in the reset_color() (found in Reset_clr.c
 * file in this directory).  Subsequent graphics calls will use this color.
 *
 * Called by:
 *      Color() in ../lib/Color.c
 */

#include "png.h"

#include "colors.h"
#include "driverlib.h"
#include "gis.h"

int color(int number)
{
    if (number >= NCOLORS || number < 0)
    {
        G_warning("Color: can't set color %d\n", number);
        return 0;
    }

    if (get_table_type() == FLOAT)
        currentColor = number;
    else if (true_color)
        currentColor = number;
    else
        currentColor = xpixels[number];

    return 0;
}

