/* @(#)adjust.c 2.1  6/26/87 */
#include "gis.h"
#include "windround.h"

adjust_window (window)
    struct Cell_head *window ;
{
    if ( window->ns_res <= 0  || window->ew_res <= 0 )
        G_fatal_error ("illegal resolution value(s) specifed") ;
    if (window->north <= window->south)
        G_fatal_error ("north must be larger than south");
    if (window->east <= window->west)
        G_fatal_error ("east must be larger than west");

/* if the north-south is not multiple of the resolution,
 *    round the south downward
 */
    window->rows = (window->north - window->south) / window->ns_res                   + WINDOW_ROUND ;
    window->south = window->north - window->rows * window->ns_res;

/* do the same for the west */
    window->cols = (window->east - window->west) / window->ew_res
                   + WINDOW_ROUND ;
    window->west = window->east - window->cols * window->ew_res;
}

