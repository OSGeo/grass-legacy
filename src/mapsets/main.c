/* %W% %G% */

/*  mapsets	Allow user to select new mapset path then call Gmapsets
 *		to set them.
 */

#define MAIN
#include "externs.h"
main()
{
    G_gisinit ("MAPSETS");
    G_clear_screen () ;
    printf("MAPSET SEARCH LIST UTILITY");
    printf("     LOCATION: %s    MAPSET: %s\n\n", G_location(), G_mapset());

    get_available_mapsets ();

    do
    {
	display_mapset_path();
	display_available_mapsets ();
    }
    while ( get_mapset_path() < 0);

    set_mapset_path ();
}
