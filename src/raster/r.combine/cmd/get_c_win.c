#include "tree.h"

static struct Cell_head cur_win ;

/* =========================================================================
 * get_cur_win returns the current window of interest
 */

struct Cell_head *
get_cur_win (void)
{
    return(&cur_win) ;
}

int 
init_proj_win (void)
{
    G_get_window(&cur_win) ;

/* everything went okay if it got this far */
    return(1) ;
}
