#include "gis.h"
/* D_setup (clear)
 *
 * This is a high level D call.
 * It does a full setup for the current graphics window.
 *
 *   1. Makes sure there is a current graphics window
 *      (will create a full-screen one, if not
 *   2. Sets the window coordinates so that the graphics window
 *      and the active program window agree
 *      (may change active program window to do this).
 *   3. Performs graphic window coordinate conversion intialization
 *
 * Returns: 0 if ok. Exits with error message if failure.
 *
 * Note: Connection to driver must already be made.
 *
 * clear values:
 *   1: clear window (visually and window coordinates)
 *   0: do not clear window
 */

D_setup (clear)
{
    struct Cell_head window;
    char name[128];
    int t,b,l,r;

    if (D_get_cur_wind(name))
    {
	t = R_screen_top();
	b = R_screen_bot();
	l = R_screen_left();
	r = R_screen_rite();
	strcpy (name, "full_screen");
	D_new_window (name, t, b, l, r);
    }

    if (D_set_cur_wind(name))
	G_fatal_error("Current graphics window not available") ;
    if (D_get_screen_window(&t, &b, &l, &r))
	G_fatal_error("Getting graphics window coordinates") ;

/* clear the window, if requested to do so */
    if (clear)
    {
	D_clear_window();
	R_standard_color(D_translate_color("black"));
	R_box_abs (l, t, r, b);
    }

/* Set the map window associated with graphics window */
    G_get_set_window (&window);
    if (D_check_map_window(&window))
	G_fatal_error("Setting graphics window coordinates") ;
    if(G_set_window (&window) < 0)
	G_fatal_error ("Invalid graphics window coordinates");

/* Determine conversion factors */
    if (D_do_conversions(&window, t, b, l, r))
	G_fatal_error("Error calculating graphics window conversions") ;

/* set text clipping, for good measure */
    R_set_window (t, b, l, r);
    return 0;
}
