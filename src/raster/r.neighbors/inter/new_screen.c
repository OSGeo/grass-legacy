#include "gis.h"

new_screen()
{
    G_clear_screen();

    printf("         NEIGHBORHOOD UTILITY\n\n");
    printf("LOCATION: %s    ", G_location());
    printf("MAPSET:   %s\n\n", G_mapset());
}
