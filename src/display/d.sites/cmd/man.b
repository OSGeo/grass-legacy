#include "gis.h"
#include <stdio.h>

main (argc, argv) 
int   argc;
char *argv[];
{
    char *mapset;
    double east, north;
    char *desc;
    int full, all;
    char msg[200];
    FILE *fd;
    struct Cell_head window;
    struct Flag *flag1 ;
    struct Flag *flag2 ;
    struct Option *opt1 ;

/* Define the different flags */

	flag1 = G_define_flag() ;
	flag1->key         = 'f' ;
	flag1->description = "Print a site description";

	flag2 = G_define_flag() ;
	flag2->key         = 'w' ;
	flag2->description = "Ignore the current window and print all the sites" ;

/* Define the different options */

	opt1 = G_define_option() ;
	opt1->key        = "sitefile";
	opt1->type       = TYPE_STRING;
	opt1->required   = YES;
	opt1->gisprompt  = "old,site_lists,sites";
	opt1->description= "Name of a site file" ;

    G_gisinit (argv[0]);

    full = 0;
    all  = 0;

    if (G_parser(argc, argv))
	exit (-1);

    full = flag1->answer;
    all  = flag2->answer;

    mapset = G_find_file ("site_lists", opt1->answer, "");
    if (mapset == NULL)
    {
        sprintf (msg, "sites file [%s] not found", opt1->answer);
        G_fatal_error (msg);
    }

    if (!all)
        G_get_window (&window);
   
     fd = G_fopen_sites_old (opt1->answer, mapset);
    if (fd == NULL)
    {
        sprintf (msg, "can't open sites file [%s]", opt1->answer);
        G_fatal_error (msg);
    }

    while (G_get_site (fd, &east, &north, &desc) > 0)
    {
        if (all || (east >= window.west && east <= window.east &&
	    north <= window.north && north >= window.south))
        {
            printf ("%lf %lf", east, north);
            if (full)
                printf (" %s", desc);
	    printf ("\n");
        }
    }
    fclose (fd);
    exit(0);
}

