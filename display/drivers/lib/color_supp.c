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
#include "gis.h"
#include "driverlib.h"

#define MAX_STD_COLORS	15

static int *pos_color_lookup ;
static int *neg_color_lookup ;
static int pos_color_look_alloc = 0 ;
static int neg_color_look_alloc = 0 ;
static int standard_colors[MAX_STD_COLORS] ;

static int check_alloc_color(int);

/* assign_fixed_color(user_color, real_color)
 *     int user_color       The color number the user wants associated with
 *     int real_color       this real (or device dependent) color.
 *
 * Maintains two tables, one for colors greater than or equal to zero, and the
 *  other for colors less than zero.  (With GRASS 3.0 negative category numbers
 *  are accommodated in the data base.)
 *
 * On allocation error the current program ungracefully exits. */
int
assign_fixed_color(int user_color, int real_color)
{
	check_alloc_color(user_color) ;
	if (user_color >= 0)
		pos_color_lookup[user_color] = real_color ;
	else
		neg_color_lookup[-user_color] = real_color ;

	return 0;
}

/* get_fixed_color(user_color)
 *     int user_color       The color referenced by the user
 *
 * returns the real device color as set in an assign_fixed_color call, or
 * zero if not set. */
int
get_fixed_color(int user_color)
{
	if (user_color >= 0)
	{
		if (pos_color_look_alloc > user_color)
			return(pos_color_lookup[user_color]) ;
		else
			return 0 ;
	}
	else
	{
		user_color = - user_color ;
		if (neg_color_look_alloc > user_color)
			return(neg_color_lookup[user_color]) ;
		else
			return 0 ;
	}

	return 0;
}

int get_fixed_color_array(int *a, int num)
{
	int i, j ;

	for(i=0; i<num; i++, a++)
	{
		if (*a >= 0)
		{
			j = *a ;
			if (pos_color_look_alloc > j)
				*a = pos_color_lookup[j] ;
			else
				*a = 0 ;
		}
		else
		{
			j = - *a ;
			if (neg_color_look_alloc > j)
				*a = neg_color_lookup[j] ;
			else
				*a = 0 ;
		}
	}

	return 0;
}

static int check_alloc_color(int n)
{
	int to_alloc ;

	if (n >= 0)
	{
		n++ ;
		if (n < pos_color_look_alloc)
			return 0;

		to_alloc = pos_color_look_alloc ;
		while (n >= to_alloc)
			to_alloc += 512 ;
		
		if (pos_color_look_alloc)
			pos_color_lookup = (int *)G_realloc(pos_color_lookup, to_alloc * sizeof(int)) ;
		else
			pos_color_lookup = (int *)G_malloc(to_alloc * sizeof(int)) ;

		if (!pos_color_lookup)
		{
			fprintf(stderr,"ERROR: insufficient memory in check_alloc_color\n") ;
			exit(-1) ;
		}

		pos_color_look_alloc = to_alloc ;
		return 0;
	}
	else
	{
		n = -n + 1 ;

		if (n < neg_color_look_alloc)
			return 0;

		to_alloc = neg_color_look_alloc ;
		while (n >= to_alloc)
			to_alloc += 512 ;
		
		if (neg_color_look_alloc)
			neg_color_lookup = (int *)G_realloc(neg_color_lookup, to_alloc * sizeof(int)) ;
		else
			neg_color_lookup = (int *)G_malloc(to_alloc * sizeof(int)) ;

		if(!neg_color_lookup)
		{
			fprintf(stderr,"ERROR: insufficient memory in check_alloc_color\n") ;
			exit(-1) ;
		}

		neg_color_look_alloc = to_alloc ;
		return 0;
	}

	return 0;
}

/* assign_standard_color(user_color, real_color)
 *     int user_color       The color number the user wants associated with
 *     int real_color       this real (or device dependent) color.
 *
 * On allocation error the current program ungracefully exits. */
int assign_standard_color(int user_color, int real_color)
{
	if ((user_color >= 0) && (user_color < MAX_STD_COLORS))
		standard_colors[user_color] = real_color ;

	return 0;
}

/* get_standard_color(user_color)
 *     int user_color       The color referenced by the user
 *
 * returns the real device color as set in an assign_fixed_color call, or
 * zero if not set. */
int get_standard_color(int user_color)
{
	if ((user_color >= 0) && (user_color < MAX_STD_COLORS))
		return standard_colors[user_color] ;
	else
		return 0 ;
}

/* get_max_std_colors()
 * 
 * returns the number of colors always reserved in floating color tables for
 * the standard colors */
int get_max_std_colors (void)
{
	return(MAX_STD_COLORS) ;
}
