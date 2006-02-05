#include <stdio.h>
#include "driver.h"
#include "driverlib.h"
#include "colors.h"

void DRV_reset_color(int number, int red, int grn, int blu)
{
	if (driver->reset_color)
		(*driver->reset_color)(number, red, grn, blu);
}

static void _fixed_Reset_color(
	unsigned char red, unsigned char grn, unsigned char blu,
	int number)
{
	LIB_assign_fixed_color(number, DRV_lookup_color(red, grn, blu));
}

static void _fixed_Reset_colors(
	int min,int max,
	const unsigned char *red,
	const unsigned char *grn,
	const unsigned char *blu)
{
	int n, i;

	for (i = min, n = 0; i <= max; i++, n++)
		_fixed_Reset_color(red[n], grn[n], blu[n], i);
}

static void _float_Reset_color(
	unsigned char red, unsigned char grn, unsigned char blu, int i)
{
	DRV_reset_color(i + get_color_offset() + get_max_std_colors(), red, grn, blu);
}

static void _float_Reset_colors(
	int min, int max,
	const unsigned char *red,
	const unsigned char *grn,
	const unsigned char *blu)
{
	int n, i;

	for (i = min, n = 0; i <= max; i++, n++)
		_float_Reset_color(red[n], grn[n], blu[n], i);
}

void COM_Reset_color(
	unsigned char red, unsigned char grn, unsigned char blu,
	int num)
{
	if (DRV_get_table_type() == FLOAT)
		_float_Reset_color(red, grn, blu, num);
	else
		_fixed_Reset_color(red, grn, blu, num);
}

void COM_Reset_colors(
	int min, int max,
	const unsigned char *red,
	const unsigned char *grn,
	const unsigned char *blu)
{
	if (DRV_get_table_type() == FLOAT)
		_float_Reset_colors(min, max, red, grn, blu);
	else
		_fixed_Reset_colors(min, max, red, grn, blu);
}

