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
        

