/*
 * The systems color represented by "number" is set using the color component
 * intensities found in the "red", "grn", and "blu" variables.  A value of
 * 0 represents 0 intensity; a value of 255 represents 100% intensity.
 */

#include "graphics.h"

reset_color(number, red, grn, blu)
        int number ;
        int red, grn, blu ;
{
        unsigned char r, g, b ;
        r = red ;
        g = grn ;
        b = blu ;

		number++ ;
        if (number>CTMAX)
                return ;

        pw_setcmsname(pixwin, "grass");
        pw_putcolormap(pixwin, number, 1, &r, &g, &b);
}
