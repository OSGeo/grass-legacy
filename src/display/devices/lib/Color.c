#include "colors.h"
#include "driverlib.h"
static int color_offset = 0 ;
static int first_time = 1 ;
static int max_std_colors ;
static int n_colors ;

int Color(int number)
{
	color(_get_color_index(number)) ;
	return 0;
}

int RGB_color(unsigned char r,unsigned char g,unsigned char b)
{
	/* assumes fixed color mode */
	color(_get_lookup_for_color(r, g, b)) ;
	return 0;
}

int _get_color_index(int number)
{
	int num ;

	if (first_time)
	{
		max_std_colors = get_max_std_colors() ;
		Get_num_colors(&n_colors) ;
		first_time = 0 ;
	}

/* table_type == FIXED */
	if (get_table_type() == FIXED)
		return(get_fixed_color(number)) ;

/* table_type == FLOAT */
	if (number >= 0)
	{
		/* silently wrap colors in float mode */
		num = max_std_colors + color_offset + number ;
		if (num > n_colors)
			num = num % n_colors ;
		return(num) ;
	}
	return 0; /* Ignore negative color requests in float mode */
}

int _get_color_index_array( int *a, int num)
{
	int i ;

	if (first_time)
	{
		max_std_colors = get_max_std_colors() ;
		Get_num_colors(&n_colors) ;
		first_time = 0 ;
	}

	if (get_table_type() == FIXED)
	{
		get_fixed_color_array(a, num) ;
		return 0;
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
	return 0;
}

int Standard_color(int number)
{
	if (get_table_type() == FIXED)
{
		color(get_standard_color(number)) ;
}
	else
		color(number);
	return 0;
}

int Color_offset(int n)
{
	color_offset = 0 + n ;
	return 0;
}

int get_color_offset(void)
{
	return color_offset ;
}
