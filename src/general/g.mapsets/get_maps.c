#include <stdio.h>
#include "externs.h"
#include "gis.h"

get_available_mapsets ()
{
    FILE *fp, *popen();
    char buf[1024];
    int n;

    nmapsets = 0;
    sprintf(buf,"ls %s", G_location_path());
    fp = popen (buf,"r");
    if (!fp)
        return;

    while (fscanf (fp, "%s", buf) == 1)
        mapset_name[nmapsets++] = G_store (buf);
    pclose (fp);
}
