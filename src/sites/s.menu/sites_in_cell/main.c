#include <stdlib.h>
#include "gis.h"
#include "site.h"
#include "site_count.h"
#include "local_proto.h"

int 
main (int argc, char *argv[])
{
    struct RCLIST *rc;
    int count;
    SITE_LIST site_list;
    struct Cell_head window;
    char *desc;
    char title[200];
    char layer[40];
    char *mapset;
    int cellfd;
    double north;
    double east;
    int n;

    if (argc != 2)
	exit(0);
    G_gisinit (argv[0]);

    new_screen();

    G_get_window (&window);

    initialize_site_list (&site_list);
    if (!get_site_list (&site_list, argv[1]))
	exit (-1);

    new_screen();

    mapset = G_ask_cell_new ("", layer);
    if (!mapset)
	exit(0);
    do
	fprintf (stdout,"\nenter a title for raster map <%s>: ", layer);
    while (!G_gets(title));
    G_strip (title);

    fprintf (stdout,"creating empty raster file ...\n");
    if ((cellfd = G_open_cell_new_random (layer)) < 0)
    {
	fprintf (stdout,"can't create raster file <%s>\n", layer);
	exit(-1);
    }

    counter_reset ("transferring sites to raster file ...", 0);
    counter (0);

    rc = NULL;
    count = 0;

    rewind_site_list (&site_list);
    for (n = 1; next_site (&site_list, &north, &east, &desc); n++)
    {
        rc = utm_to_rc(&window,north,east,rc,&count);
	counter (n);
    }
    for(n=0;n<count;n++)
      G_put_map_row_random (cellfd,&rc[n].val,(int)rc[n].row,(int)rc[n].col,1);
    fprintf (stdout,"\n");
    fprintf (stdout,"creating support files for %s\n", layer);
    G_close_cell (cellfd);
    if (*title)
	G_put_cell_title (layer, title);

    { char buf[400];
      fprintf (stdout,"compressing raster file ...\n");
      sprintf (buf, "r.compress %s > /dev/null", layer);
      system (buf);
    }

    fprintf (stdout,"\n<%s> raster file complete\n", layer);
    exit (1);
}
