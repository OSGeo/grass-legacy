/* @(#)command.c    2.2   7/24/87 */

#include "options.h"

set_default_options()
{
    strcpy(map_name, "");
    strcpy(color, "white");
}

stash_away(pos, option)
int pos;
char *option;
{
    switch (pos) {
    case NAME:
        strcpy(map_name, option);
        G_strip(map_name);
        if (*map_name == 0)
            return -1;
        break;
    case COLOR:
        strcpy(color, option);
        break;
    default:
        printf("Unknown option\n");
        return (-1);
    }
    return (0);
}
