/* @(#)command.c    2.1   6/26/87 */

#include "options.h"

set_default_options()
{
    mode = UNSET;
}

stash_away(pos, option)
int pos;
char *option;
{
    switch (pos) {
    case MODE:
        if (!strcmp(option, "float"))
            mode = FLOAT;
        else if (!strcmp(option, "fixed"))
            mode = FIXED;
        else
            return (-1);
        break;
    default:
        printf("Unknown option\n");
        return (-1);
    }
    return (0);
}
