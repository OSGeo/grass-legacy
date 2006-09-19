#include <grass/colors.h>
#include "driver.h"
#include "driverlib.h"

int DRV_lookup_color(int r, int g, int b)
{
	if (driver->lookup_color)
		return (*driver->lookup_color)(r, g, b);
	return 0;
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
	DRV_color(DRV_lookup_color(r, g, b));
}

void COM_Standard_color(int number)
{
	DRV_color(get_standard_color(number));
}

int LIB_get_color_index(int number)
{
	return get_fixed_color(number);
}

void LIB_get_color_index_array(int *dst, const int *src, int num)
{
	get_fixed_color_array(dst, src, num);
}

