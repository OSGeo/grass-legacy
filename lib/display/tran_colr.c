/* Takes a color name in ascii, returns the color number for that color.
 *    returns 0 if color is not known.
 */

#include <string.h>
#include <grass/display.h>
#include <grass/colors.h>
#include <grass/raster.h>
#include <grass/glocale.h>

int allocated_colors = MAXCOLORS; /* This is the number of the highest allocated color */

/*!
 * \brief color name to number
 *
 * Takes a
 * color <b>name</b> in ascii and returns the color number for that color.
 * Returns 0 if color is not known. The color number returned is for lines and
 * text, not raster graphics.
 *
 *  \param name
 *  \return int
 */

int D_translate_color(const char *str )
{
  int i;

  for (i = 0; i < MAX_COLOR_NAMES; i ++) {
    if (! strcmp (str, standard_color_names[i].name))
      return standard_color_names[i].number ;
  }

  return(0) ;
}


/*!
 * \brief color name and suggested number to actual number
 *
 * Takes a color <b>name</b> or <b>red:green:blue</b> code in ascii
 * and a <b>suggested color index</b>.
 * If the color is a standard preallocated color it returns the color number for that color.
 * Otherwise (if the color is not standard) it sets the color of the supplied index to 
 * the specified color (see R_reset_color).
 * Returns -1 if color is not known and 0 if the color is none.
 *
 *  \param name_or_code
 *  \param suggested_color_index
 *  \return int
 */

int D_translate_or_add_color (const char * str, int index)
{
	int redi, greeni, bluei;
	int i, preallocated, ret;

	char lowerstr[MAX_COLOR_LEN];

	/* Make the color string lowercase for display colors */
	G_strcpy (lowerstr, str );
	G_chop (lowerstr);
	G_tolcase (lowerstr);	

	preallocated = D_translate_color (lowerstr);
	
	if (preallocated != 0) {
		return preallocated;
	}

	ret = G_str_to_color (str, &redi, &greeni, &bluei);

	if (ret == 2) {
		/* None color */
		return 0;
	} else if (ret == 1) {
		/* It would be possible to, at this point, search through
		   the preallocated colors for this color and return the
		   preallocated index on a match. That is what this does: */
		for (i = 1; i <= MAX_COLOR_NUM; i++)
			if (standard_colors_rgb[i].r == redi &&
			    standard_colors_rgb[i].g == greeni &&
			    standard_colors_rgb[i].b == bluei)
				return i ;

		/* Add the specified color to the suggested index */
		R_reset_color ( (unsigned char) redi, (unsigned char) greeni, (unsigned char) bluei, index) ;
		return index ;
	} 

	return -1 ;
}

/*!
 * \brief create new suggested color number
 *
 * Returns an integer which is a suggested number greater than
 * the maximum number of basic colors to use as a color index
 * for custom colors. Each call returns a number one higher
 * than the number returned by the previous call.
 *
 *  \return int
 */

int D_allocate_color () {
    return (++allocated_colors) ;
}

/*!
 * \brief color option text to usable color number
 *
 * Converts or looks up the color provided in the string.
 * Returns a color number usable by D_raster_use_color.
 * If the color does not exist exits with a fatal error and message.
 * If the color is none and none_acceptable is not true exits with
 * a fatal error and message.
 *
 *  \param name_or_code
 *  \param none_acceptable
 *  \return int
 */

int D_parse_color (const char * str, int none_acceptable) {
    int color ;
    color = D_translate_or_add_color(str, D_allocate_color());
    if (color == -1)
        G_fatal_error(_("[%s]: No such color"), str);
    if (color == 0 && (! none_acceptable))
        G_fatal_error(_("[%s]: No such color"), str);
    return color;
}

/*!
 * \brief draw with a color from D_parse_color
 *
 * Calls R_color or R_standard_color to use the color provided by 
 * D_parse_color. Returns 1 if color can be used to draw (is
 * good and isn't none), 0 otherwise.
 *
 *  \param color
 *  \return int
 */

int D_raster_use_color (int color) {
    if (color == 0 || color == -1)
        return 0 ;
    if (0 < color && color <= MAXCOLORS)
    {
        R_standard_color (color) ;
        return 1 ;
    } else {
        R_color (color) ;
        return 1;
    }
    return 0 ;
}
