/*
 *   Dvect
 *
 *   Usage:  Dvect mapname [color]
 *           Dvect name=mapname color=name
 *
 *   Draw the binary vector (dig) file that
 *   the user wants displayed on top of the current image.
 */

#define USAGE   "name=mapname [color=name]"
#include "gis.h"
#include "digit.h"
#define MAIN
#include "options.h"

main(argc, argv)
    int argc ;
    char **argv ;
{
    struct Map_info P_map;
    char *mapset ;
    char buf[128] ;
    int i, stat ;
    extern int stash_away() ;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

/* Check command line */
    set_default_options() ;

    if (D_parse_command(argc, argv, variables, n_variables, stash_away) || 
        *map_name == 0)
    {
        fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
        exit(-1) ;
    }

/* Make sure map is available */
    mapset = G_find_file2 ("dig", map_name, "") ;
    if (mapset == NULL)
    {
        sprintf(buf,"Vector file [%s] not available", map_name);
        G_fatal_error(buf) ;
    }

    R_open_driver();

    D_setup(0);

    R_standard_color(color) ;


    if (use_plot1(map_name, mapset))
	stat = plot1 (map_name, mapset);
    else if (stat = plot2 (map_name, mapset))
    {
	fprintf (stderr, "\n*** Will try another method ***\n\n");
	stat = plot1 (map_name, mapset);
    }
    if(stat == 0)
    {
				/* Add this command to list */
	strcpy(buf, argv[0]) ;
	for(i=1; i<argc; i++)
	{
	    strcat(buf, " ") ;
	    strcat(buf, argv[i]) ;
	}
	D_add_to_list(buf) ;
    };
    R_close_driver();
    exit(stat);
}

/* NULL function to bypass debugf() in dig library */
debugf() { }
