/**********************************************************************
 *
 *   char *
 *   G_gisdbase()
 *
 *   returns:    pointer to string containing the base directory of
 *               GRASS-GRID data.
 **********************************************************************/

#include "gis.h"

char *
G_gisdbase()
{
    return G_getenv ("GISDBASE");
}
