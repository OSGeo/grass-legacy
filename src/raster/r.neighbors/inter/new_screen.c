#include "gis.h"

int 
new_screen (void)
{
    G_clear_screen();

    fprintf (stdout,"         NEIGHBORHOOD UTILITY\n\n");
    fprintf (stdout,"LOCATION: %s    ", G_location());
    fprintf (stdout,"MAPSET:   %s\n\n", G_mapset());

    return 0;
}
