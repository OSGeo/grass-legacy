#include <unistd.h>
#include <string.h>
#include "local_proto.h"

int what (double east, double north, int width, int mwidth)
{
    Site *close;
    char *desc;
    char east_buf[40], north_buf[40];
    char temp[512], *str, *mapset, *Gmapset;
    int i;
    
    G_format_easting(east, east_buf, G_projection());
    G_format_northing(north, north_buf, G_projection());
    if(!isatty(fileno(stdout)))
        fprintf(stdout, "\n%s(E) %s(N)\n", east_buf, north_buf);
    fprintf(stderr, "\n%s(E) %s(N)\n", east_buf, north_buf);
    Gmapset = G_mapset();

    for(i=0; i<nsites; i++)
    {
        strcpy(temp, site[i]);
        if((str = strchr(temp, '@'))){
            *str = 0;
            mapset = str+1;
        }else{
            mapset = Gmapset;
        }
        if(!isatty(fileno(stdout)))
            fprintf(stdout, "%*s in %-*s  ", width, temp, mwidth, mapset);
        fprintf(stderr, "%*s in %-*s  ", width, temp, mwidth, mapset);

                if(NULL != (close = closest_site(i, east, north))){
            desc =    G_site_format (close, NULL, 0);
            if (!isatty(fileno(stdout)))
                fprintf(stdout, "%s\n", desc);
            fprintf(stderr, "%s\n", desc);
                }else{
            if(!isatty(fileno(stdout)))
                fprintf(stdout, "Nothing Found.\n");
            fprintf(stderr, "Nothing Found.\n");
        }
    }
    
    return 0;
}
