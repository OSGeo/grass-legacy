/****************************************************************************
 * D_popup(back_colr, text_colr, div_colr, top, left, percent_per_line, options)
 *    int back_colr ;           color of window
 *    int text_color ;          color of text and border
 *    int div_color ;           color of item divider lines
 *    int left, top ;           pixle coordinates of top-left corner
 *                              (Coordinate system is 0,0 lower left, 
 *                              100,100 upper right)
 *    int percent_per_line ;    percent of entire window per line of text
 *    char *options[] ;         array of text showing options.
 *                              The first entry is a title, not an option
 *
 * The bottom-right coordinates are calculated based on the top-left coors.,
 * the percent_per_line, the number of options, and the longest option text
 * length.
 *
 * - Current screen contents are stashed away in the area.
 * - Area is blanked with the background color and fringed with the
 *    text color.
 * - Options are drawn using the current font.
 * - User uses the mouse to choose the desired option.
 * - Area is restored with the original contents.
 * - Number of this option is returned to the calling program.
 ***************************************************************************/
#include "gis.h"

#define Y_BORDER	5
#define X_BORDER	5

D_popup(back_colr, text_colr, div_colr, top, left, percent_per_line, options)
    char *options[] ;
{
    int t, l, b, r ;
    int opt ;
    int row ;
    int x, y ;
    int button ;
    int text_size ;
    int text_raise ;
    int n_options ;
    int max_len ;
    int len ;
    char *panel ;
    int dots_per_line ;

/* Figure the number of options and the max length of options */
    max_len = 0 ;
    for(n_options=0; options[n_options] != NULL; n_options++)
    {
	len = strlen(options[n_options]) ;
	if (max_len < len) max_len = len ;
    }

/* Figure the dots per line */
    dots_per_line = (R_screen_bot() - R_screen_top()) * percent_per_line / 100 ;
    t = R_screen_bot() - (R_screen_bot() - R_screen_top()) * top / 100 ;
    l = R_screen_left() + (R_screen_rite() - R_screen_left()) * left / 100 ;

/* Figure the bottom and right of the window */
    text_size = (int)(.8 * (float)dots_per_line) ;
    text_raise = (dots_per_line - text_size + 1) / 2;
    if (text_raise == 0)
	text_raise = 1;
    b = Y_BORDER + t + dots_per_line * n_options ;
    r = 2 * X_BORDER + l + text_size * max_len ;

/* Make sure text is not drawn outside of window */
    R_set_window(t, b, l, r) ;

/* Save the panel in some name */
    panel = G_tempfile() ;
    R_panel_save(panel, t, b, l, r) ;

/* Clear the panel */
    R_standard_color(back_colr) ;
    R_box_abs(l,t,r,b) ;

/* Draw border */
    R_standard_color(text_colr) ;
    R_move_abs(l+1, t+1) ;
    R_cont_abs(r-1, t+1) ;
    R_cont_abs(r-1, b-1) ;
    R_cont_abs(l+1, b-1) ;
    R_cont_abs(l+1, t+1) ;

/* Prepare for text */
    R_text_size(text_size, text_size) ;

/* list the options */
    for(opt=1; opt<=n_options; opt++)
    {
	if (opt != n_options)
	{
	    R_standard_color(div_colr) ;
	    R_move_abs(l+2, t + Y_BORDER + opt * dots_per_line) ;
	    R_cont_rel (r-l-4, 0);
	}
	R_standard_color(text_colr) ;
	R_move_abs(l + X_BORDER, t + Y_BORDER + opt * dots_per_line - text_raise) ;
	R_text(options[opt-1]) ;
    }

    R_flush() ;

    x = (l + r) / 2 ;
    y = (t + b) / 2 ;

    while(1)
    {
	int n;

	R_get_location_with_pointer(&x, &y, &button) ;
	if (x > r
	||  x < l
	||  y < t + Y_BORDER + dots_per_line
	||  y > b - Y_BORDER)
		continue ;

	n = y - t - Y_BORDER ;
	if (n % dots_per_line == 0) continue;

	R_panel_restore(panel) ;
	R_panel_delete(panel) ;
	return (n / dots_per_line) ;
    }
}
