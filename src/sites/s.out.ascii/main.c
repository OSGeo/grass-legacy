#include "gis.h"


main (argc, argv) char *argv[];
{
    char *mapset, *name;
    double east, north;
    char ebuf[128], nbuf[128];
    char *desc;
    char *fs;
    int full, all;
    char msg[200];
    FILE *fd;
    struct Cell_head window;
    struct
    {
	struct Flag *full, *all;
    } flag;
    struct
    {
	struct Option *input, *fs;
    } parm;

    G_gisinit (argv[0]);

    parm.input = G_define_option();
    parm.input->key = "sites";
    parm.input->type = TYPE_STRING;
    parm.input->required = YES;
    parm.input->description = "name of a sites file to be output";
    parm.input->gisprompt = "old,site_lists,sites";

    parm.fs = G_define_option();
    parm.fs->key = "fs";
    parm.fs->key_desc = "character|space|tab";
    parm.fs->type = TYPE_STRING;
    parm.fs->required = NO;
    parm.fs->description = "Output field separator";
    parm.fs->answer = "space";


    flag.all = G_define_flag();
    flag.all->key = 'a';
    flag.all->description = "Output all sites (do not limit to current region)";

    flag.full = G_define_flag();
    flag.full->key = 'd';
    flag.full->description = "Include site descriptions in the output";

    if (G_parser(argc,argv))
	exit(1);
    full = flag.full->answer;
    all  = flag.all->answer;

    name = parm.input->answer;
    mapset = G_find_file ("site_lists", name, "");
    if (mapset == NULL)
    {
	sprintf (msg, "sites file [%s] not found", name);
	G_fatal_error (msg);
    }

    if (fs = parm.fs->answer)
    {
	if(strcmp (fs, "space") == 0)
	    fs = " ";
	else if(strcmp (fs, "tab") == 0)
	    fs = "\t";
    }
    else
	fs = " ";

    if (!all)
	G_get_window (&window);
    fd = G_fopen_sites_old (name, mapset);
    if (fd == NULL)
    {
	sprintf (msg, "can't open sites file [%s]", name);
	G_fatal_error (msg);
    }

    while (G_get_site (fd, &east, &north, &desc) > 0)
    {
	if (all || (east >= window.west && east <= window.east &&
		    north <= window.north && north >= window.south))
	{
	    G_format_easting (east, ebuf, -1);
	    G_format_northing (north, nbuf, -1);
	    printf ("%s%s%s", ebuf, fs, nbuf);
	    if (full)
		printf ("%s%s", fs, desc);
	    printf ("\n");
	}
    }
    fclose (fd);
    exit(0);
}
