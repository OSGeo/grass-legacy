
/****************************************************************************
 *
 * MODULE:       r.surf.fractal
 * AUTHOR(S):    Jo Wood, 19th October, 1994
 * PURPOSE:      GRASS module to manipulate a raster map layer.
 * COPYRIGHT:    (C) 2005-2008 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#define MAIN

#include <grass/glocale.h>

#include "frac.h"

int main(int argc, char *argv[])
{

    /*----------------------------------------------------------------------*/
    /*                     GET INPUT FROM USER                              */

    /*----------------------------------------------------------------------*/

    interface(argc, argv);


    /*----------------------------------------------------------------------*/
    /*                    PROCESS RASTER FILES                              */

    /*----------------------------------------------------------------------*/

    process();

    G_done_msg(_("Raster map <%s> created."), rast_out_name);
    
    return EXIT_SUCCESS;
}
