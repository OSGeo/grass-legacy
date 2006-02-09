/**********************************************************************
 *
 *   G_clear_screen()
 *
 *   clears the terminal screen
 *
 **********************************************************************/
#include <stdlib.h>
#include <grass/gis.h>

int G_clear_screen()
{
    system ("clear");

    return 0;
}
