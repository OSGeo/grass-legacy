/*
 *   Dgrid
 *
 *   Usage:  Dgrid size=num color=name
 *
 *   Draw the coordinate grid
 *   the user wants displayed on top of the current image.
 */

#define USAGE	"size=num color=name"
#include "gis.h"
#define MAIN
#include "options.h"

main(argc, argv)
    int argc ;
    char **argv ;
{
    char buff[128] ;
    int i ;
    extern int stash_away() ;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

/* Check command line */
    set_default_options() ;

    if (D_parse_command(argc, argv, variables, n_variables, stash_away))
    {
        fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
        exit(-1) ;
    }

/* Setup driver and check important information */
    R_open_driver();

    D_setup(0);

/* Set color */
    R_standard_color(color) ;

/* Do the plotting */
    plot_grid(size) ;

/* Add this command to list */
    strcpy(buff, argv[0]) ;
    for(i=1; i<argc; i++)
    {
        strcat(buff, " ") ;
        strcat(buff, argv[i]) ;
    }
    D_add_to_list(buff) ;


    R_close_driver();
}
