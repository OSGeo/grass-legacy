#include <stdio.h>
#include "site.h"
#include "gis.h"

put_site_list (site_list, file, clipped, with_window)
    SITE_LIST *site_list;
    char *file;
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

write_site_list (site_list, fd, clipped, with_window)
    SITE_LIST *site_list;
    FILE *fd;
{
    int north;
    int east;
    char *desc;
    struct Cell_head window;

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
	fprintf(fd,"# (current region: north %.2lf east %.2lf)\n",
	    window.north, window.east);
	fprintf(fd,"# (                south %.2lf west %.2lf)\n",
	    window.south, window.west);
    }
    fprintf(fd,"\n");

    while (next_site (site_list, &north, &east, &desc))
    {
	if (!clipped || within_window (north, east, &window))
	    fprintf(fd,"%d|%d|%s\n", east, north, desc);
    }

    return 1;
}
