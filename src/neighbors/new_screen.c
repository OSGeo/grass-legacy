/* %W% %G% */
#include "gis.h"

new_screen()
{
    G_clear_screen();

    printf("%s: neighborhood UTILITY\n\n", G_program_name());
    printf("LOCATION: %s    ", G_location());
    printf("MAPSET:   %s\n\n", G_mapset());
}
