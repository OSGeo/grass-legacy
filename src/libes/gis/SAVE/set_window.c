/* %W% %G%
 *******************************************************************
 * G_set_window (window)
 *     Cell_head *window          window to become operative window
 *
 *   Establishes 'window' as the current working window.  Any opened
 *   cell files has its file-to-window mapping reworked.
 *
 *******************************************************************
 * G_get_set_window (window)
 *     Cell_head *window
 * 
 *   The current working window values are returned in the structure
 *   'window'.
 *******************************************************************/

#include "G.h"
#include "windround.h"

G_get_set_window (window)
    struct Cell_head *window;
{
    G__init_window() ;
    G_copy(window, &G__.window, sizeof(*window) ) ;
    return 1;
}

G_set_window (window)
    struct Cell_head *window;
{
    int i;
    int maskfd;

/* check for valid window */
    if (window->ew_res <= 0.0 || window->ns_res <= 0.0)
    {
	G_warning ("set_window: illegal resolution in window");
	return -1;
    }
    window->rows = (window->north - window->south)/ window->ns_res + WINDOW_ROUND ;
    window->cols = (window->east  - window->west) / window->ew_res + WINDOW_ROUND ;

    if (window->cols == 0 || window->rows == 0)
    {
	G_warning ("set_window: empty window");
	return -1;
    }
    if (window->cols < 0 || window->rows < 0)
    {
	G_warning ("set_window: invalid window");
	return -1;
    }

/* except for MASK, cell files open for read must have same projection
 * and zone as new window
 */
    maskfd = G__.auto_mask > 0 ? G__.mask_fd : - 1;
    for (i = 0; i < MAXFILES; i++)
    {
	if (G__.fileinfo[i].open_mode == OPEN_OLD)
	{
	    if (G__.fileinfo[i].cellhd.zone == window->zone &&
	        G__.fileinfo[i].cellhd.proj == window->proj)
		    continue;
	    if (i != maskfd)
	    {
		G_warning ("set_window: projection/zone differs from that of currently open cell files");
		return -1;
	    }
	}
    }

/* close the mask */
    if (G__.auto_mask > 0)
    {
	G_close_cell (maskfd);
	free (G__.mask_buf);
	G__.mask_fd = -1;
	G__.auto_mask = -1;	/* turn off masking */
    }

/* copy the window to the current window */
    G_copy (&G__.window, window, sizeof (*window));

    G__.window_set = 1;

/* now for each possible open cell file, recreate the window mapping */
    for (i = 0; i < MAXFILES; i++)
	if (G__.fileinfo[i].open_mode == OPEN_OLD)
	    G__create_window_mapping (i);

/* turn masking (back) on if necessary */
    G__check_for_auto_masking ();

    return 1;
}
