#include "gis.h"
#include "wrat.h"

/* Changed the name to avoid conflict with the ANSI-C 'remove' function */
/* remove() */
int f_remove(void)
{

    char *mapset;
    char name[30];
    int ret;
            fprintf (stdout,"\n    REMOVING PROJECT FILES...\n\n\n");
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
