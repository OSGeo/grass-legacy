/*
 *   Dgraph
 *
 *   Usage:  Dmapgraph [input] [color] [hsize] [vsize]
 *           Dmapgraph [input=name] [color=name] [hsize=num] [vsize=num]
 *
 *   Draw graphics in a map window.
 */

#define MAIN
#include "options.h"
#include "gis.h"
#define USAGE   "[input=name] [color=name] [hsize=num] [vsize=num]"

extern double D_get_d_north();
extern double D_get_d_south();
extern double D_get_d_west();
extern double D_get_d_east();

extern int D_move_abs();
extern int D_cont_abs();

struct Cell_head window ;

main(argc, argv)
    int argc ;
    char **argv ;
{
    char buf[128] ;
    extern int stash_away() ;
    int i ;

/* Initialize the GIS calls */
    G_gisinit("Dmapgraph") ;

/* Check command line */
    set_default_options() ;

    if (D_parse_command(argc, argv, variables, n_variables, stash_away))
    {
        fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
        exit(-1) ;
    }

    R_open_driver();

    D_setup(0);

    G_get_set_window(&window) ;

    R_standard_color(color) ;
    R_move_abs(
       (int)(D_get_d_west() + D_get_d_east() / 2.0),
       (int)(D_get_d_north() + D_get_d_south() / 2.0)) ;
    set_text_size() ;

/* Do the graphics */
    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);

    graphics () ;

/* Add this command to list */
    if(argc > 1)
    {
        strcpy(buf, argv[0]) ;
        for(i=1; i<argc; i++)
        {
            strcat(buf, " ") ;
            strcat(buf, argv[i]) ;
        }
        D_add_to_list(buf) ;
    }

    R_close_driver();
    exit (0);
}
