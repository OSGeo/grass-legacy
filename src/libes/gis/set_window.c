/*
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
    char *err, *G_adjust_Cell_head();
/*  struct Cell_head twindow; */

/* adjust window, check for valid window */
/* adjust the real one, not a copy
    G_copy (&twindow, window, sizeof(struct Cell_head));
    window = &twindow;
*/

    if (err = G_adjust_Cell_head (window, 0,0))
    {
	char msg[1024];
	sprintf (msg, "G_set_window(): %s", err);
	G_warning (msg);
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
		G_warning ("G_set_window(): projection/zone differs from that of currently open raster files");
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
