#include <string.h>
#include "gis.h"
#include "local_proto.h"

int 
main (int argc, char *argv[])
{
    struct Colors colr;
    struct Range range;
    CELL min, max;
    char *mapset;
    int in_fd, out_fd;
    char title[512];
    char name[100];
    char *OUTPUT;
    char *INPUT; 
	struct GModule *module;
    struct Flag *flag1 ;
    struct Option *opt1 ;
    struct Option *opt2 ;
    struct Option *opt3 ;
    static int verbose = 1;


/* Define the different options */

	module = G_define_module();
	module->description =
		"Recategorizes data in a raster map layer by grouping cells " 
		"that form physically discrete areas into unique categories.";
						
    opt1 = G_define_option() ;
    opt1->key        = "input";
    opt1->type       = TYPE_STRING;
    opt1->required   = YES;
    opt1->gisprompt  = "old,cell,raster" ;
    opt1->description= "input layer name" ;


    opt2 = G_define_option() ;
    opt2->key        = "output";
    opt2->type       = TYPE_STRING;
    opt2->required   = YES;
    opt2->gisprompt  = "new,cell,raster" ;
    opt2->description= "output layer name";

    opt3 = G_define_option() ;
    opt3->key        = "title";
    opt3->key_desc   = "\"string\"";
    opt3->type       = TYPE_STRING;
    opt3->required   = NO;
    opt3->description= "Title, in quotes";

/* Define the different flags */

    flag1 = G_define_flag() ;
    flag1->key         = 'q' ;
    flag1->description = "quiet" ;

    G_gisinit (argv[0]);

    if (G_parser(argc, argv) < 0)
	exit(-1);

    verbose = (! flag1->answer);

    INPUT  = opt1->answer;
    OUTPUT = opt2->answer;
 
    strcpy (name, INPUT);
    mapset = G_find_cell2 (name,"");
    if (!mapset)
    {
        char err[100];

        sprintf (err, "%s: <%s> not found", G_program_name(), INPUT);
        G_fatal_error (err);
        exit(1);
    }

    if (G_legal_filename (OUTPUT) < 0)
    {
    	char err[100];
    	sprintf (err, "%s: <%s> illegal name", G_program_name(), OUTPUT);
    	G_fatal_error (err);
	exit(1);
    }


    in_fd = G_open_cell_old (name, mapset);
    if (in_fd < 0)
        exit(1);


    out_fd = G_open_cell_new (OUTPUT);
    if (out_fd < 0)
        exit(1);

    clump (in_fd, out_fd, verbose);

    if (verbose)
    	fprintf (stderr, "CREATING SUPPORT FILES ... "); fflush (stderr);
  
    G_close_cell (in_fd);
    G_close_cell (out_fd);


/* build title */
    if (opt3->answer != NULL)
	strcpy (title, opt3->answer);
    else
        sprintf (title, "clump of %s in %s", name, mapset);

    G_put_cell_title (OUTPUT, title);
    G_read_range (OUTPUT, G_mapset(), &range);
    G_get_range_min_max(&range, &min, &max);
    G_make_random_colors (&colr, min, max);
    G_write_colors (OUTPUT, G_mapset(), &colr);

    if (verbose)
    	fprintf (stderr, "\n");
   
    fprintf (stdout,"%d clumps\n", range.max);
    exit(0);
}
