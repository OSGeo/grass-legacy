/*  %W%  %G%  */

#include "options.h"

set_default_options()
{
    strcpy(color, "white");
    hsize = 17.0;
    vsize = 17.0;
    infile = stdin;
}


stash_away(pos, option)
int pos;
char *option;
{
    FILE *G_fopen_old();

    switch (pos) {
    case INPUT:
        infile = G_fopen_old("graph", option, "");
        if (infile == NULL) {
            printf("Graph file <%s> not found\n", option);
            return (-1);
        }
        break;
    case VSIZE:
        if (!sscanf(option, "%f", &vsize))
            return (-1);
        break;
    case HSIZE:
        if (!sscanf(option, "%f", &hsize))
            return (-1);
        break;
    case COLOR:
/*
        new_colr = D_translate_color(option) ;

        if (new_colr == 0)
        {
            printf("Don't know the color %s\n", option) ;
            printf("Available colors:\n") ;
            printf("  red      orange      yellow      green\n") ;
            printf("  blue     indigo      violet      gray\n") ;
            printf("  white    black\n") ;
            return(-1) ;
        }
        else
*/
        strcpy(color, option);
        break;
    default:
        printf("Unknown option\n");
        return (-1);
    }
    return (0);
}
