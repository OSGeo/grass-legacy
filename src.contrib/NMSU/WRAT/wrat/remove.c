#include "gis.h"
#include "wrat.h"

remove()
{

    char *mapset;
    char name[30];
    int ret;
            printf("\n    REMOVING PROJECT FILES...\n\n\n");
            while(1)
            {
                mapset = G_ask_old("enter project file name",name,
                  "wrat/project","parameter storing");
                if (!mapset)
                    break;

                ret = G_remove("wrat/project",name);
                if (!ret)
                {
                    fprintf(stderr,"Unable to remove project file [%s]\n\n",
                        name);
                    continue;
                }
            }

}
