/* =========================================================================
 * this algorithm runs the double for loops at the top level so that the AND
 * and OR operations may short circuit whenever possible to save cpu time
 * inverting this algorithm by placing the double loops in the recursion
 * would eliminate this possible savings from being exploited since the map
 * would have to be constructed at each level before knowledge of an exist-
 * short circuit was available.					
 */

#include "tree.h"
#include "externs.h"

eval_tree(tree) struct Node *tree ;
{
    register CELL *cbufptr ;
    int map_row, map_col ;
    int mod_row, mod_col ;
    int nrows, ncols;
    int next_row_to_plot;
    struct Cell_head *get_cur_win() ;
    int t, b, l, r ;
    char G_intr_char(), *G_unctrl();
    static struct Colors colors;
    static int make_colors = 1;

    /* Prepare to catch interrupts */
    set_signals() ;

    /* set up the boundaries */
    nrows = G_window_rows();
    ncols = G_window_cols();

    /* prepare for writing to monitor if at console */
    if (at_console())
    {
	R_standard_color(D_translate_color("black")) ;
	/*
	D_erase_window() ;
	*/
	D_get_screen_window(&t, &b, &l, &r) ;
	D_cell_draw_setup(t, b, l, r) ;
	R_flush() ;
	if (make_colors)
	{
	    make_16_colors (&colors);
	    make_colors = 0;
	}
	D_set_colors (&colors);
    }

    /* scale the tty map */
    mod_row = nrows/20 + 1 ;
    mod_col = ncols/70 + 1 ;
    if (!mod_row) mod_row = 1 ;
    if (!mod_col) mod_col = 1 ;

    signalflag.interrupt = 0 ;
    printf("     Hit %s to abort\n", G_unctrl(G_intr_char())) ;

    next_row_to_plot = 0;
    for(map_row = 0 ; map_row < nrows ; map_row++)
    {
	rcr_rd_line(tree, map_row) ;

	if (signalflag.interrupt) 
	{
	    reset_signals() ;
	    return(-1) ;
	}

	cbufptr = tree->cbuf ;

	for(map_col = 0 ; map_col < ncols ; map_col++, cbufptr++)
	{
	    *cbufptr = rcr_find_val(tree, map_col) ;

	    /* plot the segment */

	    if(!at_console())
	    {
		if(!(map_row%mod_row) && !(map_col%mod_col))
		    printf("%d", (int)((*cbufptr))%10) ;
	    }
	}

	if(at_console())
	{
	    if (map_row == next_row_to_plot)
		next_row_to_plot = D_draw_cell(map_row, tree->cbuf, &colors) ;
	    R_flush() ;
	}

	rcr_wr_line(tree, map_row) ;
	if(!at_console())
	{
	    if(!(map_row%mod_row))
		printf("\n") ;
	}
    }
    if (at_console())
    {
	R_flush() ;
    }
    reset_signals() ;
    return(0) ;
}

/* ========================================================================= */
