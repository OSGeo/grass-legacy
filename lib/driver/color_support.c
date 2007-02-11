/***********************************************************************
 * The assign_fixed_color and get_fixed_color manage lookup tables
 * between map colors and real colors.
 * The assign_standard_color and get_standard_color manage lookup tables
 * between the standard (always available) colors and real colors.
 *
 ***********************************************************************
 */

#include <stdio.h>
#include <stdlib.h>
#include <grass/gis.h>
#include "driverlib.h"

#define MAX_STD_COLORS	15

static int standard_colors[MAX_STD_COLORS];

/* assign_standard_color(user_color, real_color)
 *     int user_color       The color number the user wants associated with
 *     int real_color       this real (or device dependent) color.
 *
 * On allocation error the current program ungracefully exits. */
void LIB_assign_standard_color(int user_color, int real_color)
{
	if ((user_color >= 0) && (user_color < MAX_STD_COLORS))
		standard_colors[user_color] = real_color;
}

/* get_standard_color(user_color)
 *     int user_color       The color referenced by the user
 *
 * returns the real device color as set in an assign_fixed_color call, or
 * zero if not set. */
int get_standard_color(int user_color)
{
	if ((user_color >= 0) && (user_color < MAX_STD_COLORS))
		return standard_colors[user_color];
	else
		return 0;
}

/* get_max_std_colors()
 * 
 * returns the number of colors always reserved in floating color tables for
 * the standard colors */
int get_max_std_colors(void)
{
	return MAX_STD_COLORS;
}

