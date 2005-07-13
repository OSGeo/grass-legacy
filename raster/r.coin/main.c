#define GLOBAL
#include <stdlib.h>
#include <unistd.h>
#include "coin.h"


int 
main (int argc, char *argv[])
{
  double G_area_of_cell_at_row();
fill = "                                                                                                                                       ";
midline = "------------------------------------------------------------------------------------------------------------------------------------";

    G_gisinit (argv[0]);
    G_get_window(&window);
    /* now make a temorary region with the same boundaries only 1 x 1 */
    window.rows = 1;
    window.cols = 1;
    G_adjust_Cell_head (&window, 1, 1);
    G_set_window(&window);

    G_begin_cell_area_calculations();
    window_area = G_area_of_cell_at_row(0);

    /* restore region back to the original */
    G_get_window(&window);
    G_set_window(&window);

    dumpname = G_tempfile () ;
    statname = G_tempfile () ;

    window_cells = G_window_rows() * G_window_cols();


    if (argc > 1)
	command_version(argc, argv);
    else
	interactive_version();

    unlink (dumpname);
    unlink (statname);
    exit(0);
}
