#include "gis.h"
#include "site.h"

main (argc, argv) 	char *argv[];
{
    SITE_LIST site_list;
    struct Cell_head window;
    struct Categories cats;
    char layer[40];
    char *mapset;
    int quad_size;
    int cellfd;
    double north;
    double east;
    char *desc;
    int n;
    char title[200];
    char buf[200];
    int zero_one;
    int some_ok;
    long xcat;

    if (argc != 2)
	exit(0);

    G_gisinit (argv[0]);
    new_screen();

    G_get_window (&window);

    initialize_site_list (&site_list);
    if (!get_site_list (&site_list, argv[1]))
	exit (-1);

    if (!ask_quad (&quad_size))
	exit(0);

    new_screen();

    mapset = G_ask_cell_new ("", layer);
    if (!mapset)
	exit(0);
    do
	printf("\nenter a title for raster map <%s>: ", layer);
    while (!G_gets(title));
    G_strip (title);

    printf ("\n");
    printf ("creating empty raster file ...\n");
    if ((cellfd = G_open_cell_new_random (layer)) < 0)
    {
	printf("can't create raster file <%s>\n", layer);
	exit(-1);
    }

/* if the site descriptions are all of the form: #n <label>
   then assign the site to the #n category with label <label>
   otherwise create a 0/1 cell file
*/

    G_init_cats ((CELL)0, title, &cats);

    zero_one = 0;
    some_ok = 0;
    rewind_site_list (&site_list);
    for (n = 1; next_site (&site_list, &north, &east, &desc); n++)
    {
	*buf = 0;
	if (sscanf (desc,"#%ld%[^\n]", &xcat, buf) < 1)
	{
		zero_one = 1;
		if (some_ok) break;
		continue;
	}
	some_ok = 1;
	G_strip (buf);
	if (*buf)
	    G_set_cat ((CELL)xcat, buf, &cats);
    }
    if (zero_one && some_ok)
    {
	printf ("\n** some site descriptions were not in the #cat format.");
	printf (" creating a 0/1 raster file\n\n");
    }
    if (zero_one)
    {
	G_init_cats ((CELL)1,title,&cats);
	G_set_cat ((CELL)1, "site data", &cats);
    }
    G_set_cat ((CELL)0, "no data", &cats);

    printf ("transferring sites to raster file ...\n");

    rewind_site_list (&site_list);
    for (n = 1; next_site (&site_list, &north, &east, &desc); n++)
    {
	if (zero_one)
	    write_cell (cellfd, &window, north, east, quad_size, (CELL)1);
	else
	{
	    xcat = 0;
	    sscanf (desc,"#%ld", &xcat);
	    write_cell (cellfd, &window, north, east, quad_size, (CELL)xcat);
	}
    }
    printf ("creating support files ...\n");
    G_close_cell (cellfd);
    G_write_cats (layer, &cats);

    printf ("compressing raster file ...\n");

    sprintf (buf, "r.compress %s > /dev/null", layer);
    system (buf);

    printf("\n<%s> raster file complete\n", layer);
    exit (1);
}
