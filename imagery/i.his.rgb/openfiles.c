#include <stdio.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include "globals.h"

 
void openfiles (char *h_name, char *i_name, char *s_name,
                char *r_name, char *g_name, char *b_name,
                int fd_input[3], int fd_output[3],
                CELL *rowbuf[3])
{
    char *mapset;

    /* open input files */
    if ((fd_output[0] = G_open_cell_new (r_name)) < 0)
        G_fatal_error (_("Error in opening output file <%s>"), r_name);
    if ((fd_output[1] = G_open_cell_new (g_name)) < 0)
        G_fatal_error (_("Error in opening output file <%s>"), g_name);
    if ((fd_output[2] = G_open_cell_new (b_name)) < 0)
        G_fatal_error (_("Error in opening output file <%s>"), b_name);

    /* allocate the cell row buffer */
    if ((rowbuf[0] = G_allocate_cell_buf ()) == NULL)
        G_fatal_error (_("Unable to allocate the input row buffer"));
    if ((rowbuf[1] = G_allocate_cell_buf ()) == NULL)
        G_fatal_error (_("Unable to allocate the input row buffer"));
    if ((rowbuf[2] = G_allocate_cell_buf ()) == NULL)
        G_fatal_error (_("Unable to allocate the input row buffer"));

    /* open output files */
    if ((mapset = G_find_cell (h_name, "")) == NULL)
        G_fatal_error (_("Unable to find input cell map <%s>"), h_name);
    if ((mapset = G_find_cell (i_name, "")) == NULL)
        G_fatal_error (_("Unable to find input cell map <%s>"), i_name);
    if ((mapset = G_find_cell (s_name, "")) == NULL)
        G_fatal_error (_("Unable to find input cell map <%s>"), s_name);

    if ((fd_input[0] = G_open_cell_old (h_name, mapset)) < 0)
        G_fatal_error (_("Error in opening input file <%s>"), h_name);
    if ((fd_input[1] = G_open_cell_old (i_name, mapset)) < 0)
        G_fatal_error (_("Error in opening input file <%s>"), i_name);
    if ((fd_input[2] = G_open_cell_old (s_name, mapset)) < 0)
        G_fatal_error (_("Error in opening input file <%s>"), s_name);

    return;
}
