#include "options.h"

set_default_options()
{
    color = D_translate_color("white") ;
    hsize = 5.0 ;
    vsize = 5.0 ;
    infile = stdin ;
}

stash_away(pos, option)
    int pos ;
    char *option ;
{
    int new_colr ;
    FILE *G_fopen_old() ;
    char *mapset ;
    char *G_find_file() ;

    switch(pos)
    {
    case INPUT:
        mapset = G_find_file ("mapgraph", option, "");
        if (mapset == NULL)
        {
            printf("Mapgraph file [%s] not available", option);
            return(-1) ;
        }
        infile = G_fopen_old ("mapgraph", option, mapset);
        if (infile == NULL)
        {
            printf("Graph file <%s> not available\n", option) ;
            return(-1) ;
        }
        break ;
    case VSIZE:
        if (sscanf(option,"%lf",&vsize) != 1 )
            return(-1) ;
        break ;
    case HSIZE:
        if (sscanf(option,"%lf",&hsize) != 1 )
            return(-1) ;
        break ;
    case COLOR:
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
            color = new_colr ;
        break ;
    default:
        printf("Unknown option\n") ;
        return(-1) ;
    }
    return(0) ;
}
