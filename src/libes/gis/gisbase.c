/**********************************************************************
 *
 *   char *
 *   G_gisbase()
 *
 *   returns:    pointer to string containing the base directory of
 *               GRASS-GRID
 **********************************************************************/

#include "gis.h"

char *
G_gisbase()
{
    return G_getenv ("GISBASE");
}
