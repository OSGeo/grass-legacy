#include <stdio.h>
#include "gis.h"
#include "site.h"

int put_site_list (SITE_LIST *site_list,char *file,int clipped,int with_window)
{
    FILE *fd;
    int stat;

    if (!(fd = fopen(file,"w")))
    {
	perror (file);
	return 0;
    }

    stat = write_site_list (site_list, fd, clipped, with_window);

    fclose (fd);

    return stat;
}

int write_site_list (SITE_LIST *site_list,FILE *fd,int clipped,int with_window)
{
    char buff1[50], buff2[50];
    double north;
    double east;
    char *desc;
    int proj = 0;
    struct Cell_head window;
    char *format_east(), *format_north(), *format_res();

    if (clipped || with_window)
    {
	if (G_get_window(&window) == -1)
	{
	    fprintf(stderr,"warning - can't read current region\n");
	    with_window = clipped = 0;
	}
    }

    if (site_list->name[0])
	fprintf(fd,"name|%s\n", site_list->name);
    if (site_list->desc[0])
	fprintf(fd,"desc|%s\n", site_list->desc);
    rewind_site_list (site_list);

    fprintf(fd,"\n");
    fprintf(fd,"# <easting>|<northing>|<description of the site>\n");
    fprintf(fd,"#\n");
    fprintf(fd,"# (note: the above is the format of a point line)\n");
    fprintf(fd,"# (lines that begin with # are comments.        )\n");
    fprintf(fd,"# (actual point lines should not have the #     )\n");
    if (with_window)
    {
	fprintf(fd,"#\n");
        proj = G_projection();
	fprintf(fd,"# (current region: north %s east %s)\n",
	    format_north(window.north, buff1, proj),
            format_east(window.east, buff2, proj));
	fprintf(fd,"# (                south %s west %s)\n",
	    format_north(window.south, buff1, proj),
            format_east(window.west, buff2, proj));
    }
    fprintf(fd,"\n");

    while (next_site (site_list, &north, &east, &desc))
    {
	if (!clipped || within_window (north, east, &window))
	    fprintf(fd,"%s|%s|%s\n", 
                format_east(east, buff1, proj),
                format_north(north, buff2, proj), desc);
    }

    return 1;
}
