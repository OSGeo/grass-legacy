
#include "colors.h"
static int color_offset = 0 ;
static int first_time = 1 ;
static max_std_colors ;
static n_colors ;

Color(number)
{
	Dcolor(_get_color_index(number)) ;
}

RGB_color(r, g, b)
	unsigned char r, g, b ;
{
	/* assumes fixed color mode */
	Dcolor(_get_lookup_for_color(r, g, b)) ;
}

_get_color_index(number)
	int number ;
{
	int num ;

	if (first_time)
	{
		max_std_colors = get_max_std_colors() ;
		n_colors = get_num_colors() ;
		first_time = 0 ;
	}

	if (get_table_type() == FIXED)
	{
		return(get_fixed_color(number)) ;
	}
	else      /* table_type == FLOAT */
	{
		if (number >= 0)
		{
			/* silently wrap colors in float mode */
			num = max_std_colors + color_offset + number ;
			if (num > n_colors)
				num = num % n_colors ;
			return(num) ;
		}
		/* else Ignore negative color requests in float mode */
	}
}

_get_color_index_array(a, num)
	int *a ;
	int num ;
{
	int i ;

	if (first_time)
	{
		max_std_colors = get_max_std_colors() ;
		n_colors = get_num_colors() ;
		first_time = 0 ;
	}

	if (get_table_type() == FIXED)
	{
		get_fixed_color_array(a, num) ;
		return ;
	}
	else      /* table_type == FLOAT */
	{
		for (i=0; i<num; i++)
		{
		    if (*a >= 0)
		    {
			    /* silently wrap colors in float mode */
			    *a = max_std_colors + color_offset + *a ;
			    if (*a > n_colors)
				    *a = *a % n_colors ;
		    }
		    /* else Ignore negative color requests in float mode */
		    else
		    {
			    *a = 0 ;
		    }
		    a++ ;
		}
	}
}

Standard_color(number)
	int number ;
{
	int num ;

	if (get_table_type() == FIXED)
{
		Dcolor(get_standard_color(number)) ;
}
	else
		Dcolor(number);
}

Color_offset(n)
{
	color_offset = n ;
}

get_color_offset()
{
	return color_offset ;
}
