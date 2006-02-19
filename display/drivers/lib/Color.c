#include <grass/colors.h>
#include "driver.h"
#include "driverlib.h"

static int color_offset = 0;
static int first_time = 1;
static int max_std_colors;
static int n_colors;

int DRV_lookup_color(int r, int g, int b)
{
	if (driver->lookup_color)
		return (*driver->lookup_color)(r, g, b);
	return 0;
}

void DRV_lookup_rgb(int number, int *r, int *g, int *b)
{
	if (driver->lookup_rgb)
		(*driver->lookup_rgb)(number, r, g, b);
}

void DRV_color(int number)
{
	if (driver->color)
		(*driver->color)(number);
}

void COM_Color(int number)
{
	DRV_color(LIB_get_color_index(number));
}

void COM_Color_RGB(unsigned char r, unsigned char g, unsigned char b)
{
	/* assumes fixed color mode */
	DRV_color(DRV_lookup_color(r, g, b));
}

void COM_Standard_color(int number)
{
	if (DRV_get_table_type() == FIXED)
		DRV_color(get_standard_color(number));
	else
		DRV_color(number);
}

void COM_Color_offset(int n)
{
	color_offset = n;
}

int LIB_get_color_index(int number)
{
	int num;

	if (first_time)
	{
		max_std_colors = get_max_std_colors();
		COM_Get_num_colors(&n_colors);
		first_time = 0;
	}

	/* table_type == FIXED */
	if (DRV_get_table_type() == FIXED)
		return get_fixed_color(number);

	/* table_type == FLOAT */
	if (number >= 0)
	{
		/* silently wrap colors in float mode */
		num = max_std_colors + color_offset + number;
		if (num > n_colors)
			num = num % n_colors;
		return num;
	}

	return 0; /* Ignore negative color requests in float mode */
}

void LIB_get_color_index_array(int *dst, const int *src, int num)
{
	int i;

	if (first_time)
	{
		max_std_colors = get_max_std_colors();
		COM_Get_num_colors(&n_colors);
		first_time = 0;
	}

	if (DRV_get_table_type() == FIXED)
	{
		get_fixed_color_array(dst, src, num);
		return;
	}

	for (i = 0; i < num; i++)
	{
		int c = *src;

		if (c >= 0)
		{
			/* silently wrap colors in float mode */
			c = max_std_colors + color_offset + c;
			if (c > n_colors)
				c %= n_colors;
		}
		/* else Ignore negative color requests in float mode */
		else
			c = 0;

		dst[i] = c;
	}
}

int get_color_offset(void)
{
	return color_offset;
}

