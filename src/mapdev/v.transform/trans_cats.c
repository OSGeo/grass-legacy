/*
****************************************************************************
*
* MODULE:       v.transform
* AUTHOR(S):    See other files as well...
*               Eric G. Miller <egm2@jps.net>
* PURPOSE:      To transform a vector layer's coordinates via a set of tie
*               points.
* COPYRIGHT:    (C) 2002 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include "gis.h"

#define DIG_CATS "dig_cats"

void trans_dig_cats (const char *from, const char *from_mapset, const char *to)
{
    char buff[512];
    FILE *ifp, *ofp;

    if ((ifp = G_fopen_old (DIG_CATS, from, from_mapset)))
    {
        if ((ofp = G_fopen_new (DIG_CATS, to)) == NULL)
        {
            G_warning ("failed to transfer category labels");
            fclose (ifp);
            return;
        }
        while (fgets (buff, 512, ifp))
            fprintf (ofp, "%s", buff);
        fclose (ifp);
        fclose (ofp);
    }
}
        

