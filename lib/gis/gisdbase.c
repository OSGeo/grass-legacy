/**********************************************************************
 *
 *   char *
 *   G_gisdbase()
 *
 *   returns:    pointer to string containing the base directory of
 *               GRASS-GRID data.
 **********************************************************************/

#include "gis.h"


/*!
 * \brief top level database directory
 *
 * Returns the
 * full UNIX path name of the directory which holds the database locations. See
 * GISDBASE for a full explanation of this directory.
 *
 *  \param void
 *  \return char * 
 */

char *
G_gisdbase()
{
    return G_getenv ("GISDBASE");
}
