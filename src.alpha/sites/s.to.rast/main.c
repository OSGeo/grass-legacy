/*

s.to.rast - this program was created to provided a command-line operability
of what could have been done interactively with the s.menu program. Thus,
much of what you find here is simply a modified version of the sites_to_rast
portion of the s.menu code.

This program reads a site file and creates a raster map representation
of the data. If the site file has category descriptions for all the sites
(and the -s flag is not used), the resulting map will reflect the categories
and their text descriptions (if any). This is why the sites list is read
twice - once to see if all sites have the categories and to put them in a
list, then once again to actually create the map. If the -s flag is used,
the first reading is by-passed.

Chris Rewerts, U.S. Army Construction Engineering Research Laboratory
rewerts@zorro.cecer.army.mil
May 20, 1993

*/

#include "gis.h"

main (argc, argv) 	char *argv[];
{

    FILE *fd;
    struct Cell_head window;
    struct Categories cats;
    char *layer;
    char *name, *mapset;
    char *desc;
    char buf[1024];
    char title_buf[1024];
    char *prev_cat;
    int quad_size;
    int quiet;
    int cellfd;
    int zero_one;
    int said_it;
    long xcat;
    double north;
    double east;
 
    struct Option *input;
    struct Option *output;
    struct Option *size;
    struct Option *given_title;
    struct Flag   *verbose;
    struct Flag   *one_cat;

    G_gisinit (argv[0]);

    input = G_define_option();
    input->key          ="input";
    input->description  ="Name of input site list";
    input->type		=TYPE_STRING;
    input->required     =YES;
    input->gisprompt 	="old,site_lists,site list";
 
    output = G_define_option();
    output->key         ="output";
    output->type        =TYPE_STRING;
    output->required    =YES;
    output->gisprompt   ="new,cell,raster";
    output->description ="Name of new raster file";
 
    size = G_define_option();
    size->key           ="size";
    size->description   ="Number of cells to surround site cell";
    size->type 		=TYPE_INTEGER;
    size->required      =NO;

    given_title = G_define_option ();
    given_title->key		="title";
    given_title->required	= NO;
    given_title->type		= TYPE_STRING;
    given_title->description	= "Title for the resulting raster map";
 
    verbose = G_define_flag() ;
    verbose->key         = 'q' ;
    verbose->description = "Run quietly";

    one_cat = G_define_flag();
    one_cat->key	 = 's' ;
    one_cat->description = "Create a single-valued (0/1) raster map";

    if(G_parser(argc, argv))
        exit(1);
 
    quiet = verbose->answer;
    zero_one = one_cat->answer;
    name = input->answer;
    layer = output->answer;
    if (given_title->answer)
        sprintf (title_buf, "%s", given_title->answer);

    if (size->answer)
    {
        if (sscanf (size->answer, "%ld", &quad_size) != 1 || quad_size < 0)
        {
            fprintf(stderr,
            "\n%s: <%s> is an illegal size option.\n",
            G_program_name(), size->answer);
            G_usage();
            exit(1);
        }
    }
    else
        quad_size = 0;  /* our default - one cell per site */

    if (G_legal_filename(layer) < 0)
    {
        fprintf (stderr, "\n%s: <%s> - illegal name\n", 
        G_program_name (), layer);
        G_usage ();
        exit(1);
    }

    G_get_window (&window);

    if (!quiet)
    {
        fprintf (stdout, "\n\n%s\n", G_program_name());
        fprintf (stdout, "using size option: %d\n", quad_size);
        if (zero_one)
            fprintf (stdout, "forcing single-valued (0/1) raster map\n");
        fprintf (stdout, "\nfinding and opening site list...\n");
    }

    mapset = G_find_sites(name, "");
    if (!mapset)
    {
	fprintf (stderr, "\n%s: site list <%s> not found.\n",
	G_program_name(), name);
	exit(1);
    }

    fd = G_fopen_sites_old (name, mapset);
    if (fd == NULL)
    {
        fprintf (stderr, "\n%s: could not open sites file <%s>",
	G_program_name(), name);
	exit(1);
    }

    if (!quiet)
    {
        fprintf (stdout, "\ninput: <%s> in <%s>\n", name, mapset);
        fprintf (stdout, "\ncreating empty raster file ...\n");
    }

    if ((cellfd = G_open_cell_new_random (layer)) < 0)
    {
        fprintf(stderr, "\ncan't create raster file <%s>\n", layer);
        exit(1);
    }
    if (!quiet)
        fprintf (stdout, "\noutput: <%s> in <%s>\n", layer, G_mapset());


/* 
   if the site descriptions are all of the form: #n <label>
   then assign the site to the #n category with label <label>
   otherwise create a 0/1 cell file
*/
    if (!given_title->answer)
        sprintf (title_buf, "Created by %s from %s", G_program_name (), 
        G_fully_qualified_name (name, mapset) );
    G_init_cats ((CELL)0, title_buf, &cats);

/*
rewind site list to run thru and see if labels are in order
this step is not done if -1 flag was used
*/
    if(!zero_one)
    {
        if (!quiet)
            fprintf (stdout, "\nchecking category values and labels...\n");
        said_it = 0;
        fseek (fd,0L,0);
        while (G_get_site (fd, &east, &north, &desc) == 1)
        {
            *buf = 0;
      	    if (sscanf (desc,"#%ld%[^\n]", &xcat, buf) < 1)
	    {
	        zero_one = 1;
	        break;
	    }

            G_strip (buf);
            if (*buf)
            {
                prev_cat = G_get_cat((CELL)xcat, &cats);
                if (strcmp(prev_cat, "") && strcmp(prev_cat,buf) && !quiet)
                {
                    if (!said_it)
                    {
                        fprintf (stderr,
                        "\nNOTE: Category description mismatch -\n");
                        fprintf (stderr,
                        "more than one label has been found for the same category:\n");
                        said_it = 1;
                    }
                    fprintf (stderr,
                    "    Category Number:      %ld\n", xcat);
                    fprintf (stderr,
                    "    Previous label found: %s\n", prev_cat);
                    fprintf (stderr,
                    "    Current label found:  %s\n", buf);
                }
	    G_set_cat ((CELL)xcat, buf, &cats);
            }
        }

        if (zero_one && !quiet )
        {
	    fprintf (stderr, 
            "\nNOTE: some site(s) did not have category values in the\n");
            fprintf (stderr, 
            "description field, so we can only create a 0/1 raster file.\n");
        }
     }
     else
     {
         G_init_cats ((CELL)1,title_buf,&cats);
         G_set_cat ((CELL)1, "site data", &cats);
     }
    G_set_cat ((CELL)0, "no data", &cats);
    if (!quiet)
        fprintf (stdout, "\ntransferring sites to raster file...\n");

/*
rewind site list
*/
    fseek (fd,0L,0);

    while (G_get_site (fd, &east, &north, &desc) == 1)
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
    if (!quiet)
        fprintf (stdout, "\ncreating support files ...\n");
    G_close_cell (cellfd);
    G_write_cats (layer, &cats);
    if (!quiet)
        fprintf (stdout, "\ncompressing raster file ...\n");
    sprintf (buf, "r.compress %s > /dev/null", layer);
    G_system (buf);
    if (!quiet)
        fprintf(stdout, "\n<%s> raster file complete. Bye.\n\n", layer);
    exit(0);
}
