/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       coorcnv library
 * AUTHOR(S):    Andreas Lange - andreas.lange@rhein-main.de
 * PURPOSE: 	 provide functions for reading datum parameters from the
 *               location database.     
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include "gis.h"
#include "CC.h"

/* this functionality has been moved into libgis */

int 
CC_get_datum_by_name(const char *name)
{
    return G_get_datum_by_name(name);
}

char *
CC_get_datum_by_nbr(int n) 
{
    return G_datum_name(n);
}

/* this sets the datum shift parameters for datum pointed to by name */
int 
CC_datum_shift (const char *name, double *dx, double *dy, double *dz)
{
    return G_datum_shift(G_get_datum_by_name(name), dx, dy, dz);
}

/* set the ellipsoid name and parameters for datum */
int 
CC_get_datum_parameters (const char *name, char *ellps, double *dx, double *dy, double *dz)
{
    return G_datum_parameters(G_get_datum_by_name(name), ellps, dx, dy, dz);
}

char *
CC_datum_name (int n)
{
    return G_datum_name(n);
}

char *
CC_datum_description (int n)
{ 
    return G_datum_description(n);
}

char *
CC_datum_ellipsoid (int n)
{
    return G_datum_ellipsoid(n);
}

