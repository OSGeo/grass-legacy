/*
 * Identify a color that has been set in the reset_color() (found in Reset_clr.c
 * file in this directory).  Subsequent graphics calls will use this color.
 *
 * Called by:
 *      Color() in ../lib/Color.c
 */

extern int NCOLORS ;

color(number)
	int number ;
{
	if (number >= NCOLORS)
		return ;
	set_line_index(number);
	set_fill_index(number);
}
