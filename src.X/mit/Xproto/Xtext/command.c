/* @(#)command.c    2.1   6/26/87 */

#include "options.h"

set_default_options()
{
    strcpy(color, "white");
    size = 10.0;
    start_line = 1;
}


stash_away(pos, option)
int pos;
char *option;
{
    switch (pos) {
    case SIZE:
        if (!sscanf(option, "%f", &size))
            return (-1);
        break;
    case COLOR:
        /* new_colr = D_translate_color(option) ; if (new_colr == 0)
         * { printf("Don't know the color %s\n", option) ;
         * printf("Available colors:\n") ; printf("  red      orange
         * yellow      green\n") ; printf("  blue     indigo
         * violet      gray\n") ; printf("  white    black\n") ;
         * return(-1) ; } else */
        strcpy(color, option);
        break;
    case LINE:
        if (!sscanf(option, "%d", &start_line))
            return (-1);
        break;
    default:
        printf("Unknown option\n");
        return (-1);
    }
    return (0);
}

