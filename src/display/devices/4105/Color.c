/*
 * Identify a color that has been set in the reset_color() (found in Reset_clr.c
 * file in this directory).  Subsequent graphics calls will use this color.
 *
 * Called by:
 *      Color() in ../lib/Color.c
 */

#include "graphics.h"
extern int N_COLORS ;

color(number)
        int number ;
{
        if (number >= N_COLORS)
                return ;
        COLOR   = number;

        Op = PIX_COLOR(number) | PIX_SRC;

        if (!has_color) {
                if (number == 0)
                        Op = PIX_SET;
                else
                        Op = PIX_CLR;
        }
}
