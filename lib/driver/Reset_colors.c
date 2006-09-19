#include <stdio.h>
#include "driver.h"
#include "driverlib.h"
#include <grass/colors.h>

void COM_Reset_color(
	unsigned char red, unsigned char grn, unsigned char blu,
	int num)
{
	LIB_assign_fixed_color(num, DRV_lookup_color(red, grn, blu));
}

void COM_Reset_colors(
	int min, int max,
	const unsigned char *red,
	const unsigned char *grn,
	const unsigned char *blu)
{
	int n, i;

	for (i = min, n = 0; i <= max; i++, n++)
		COM_Reset_color(red[n], grn[n], blu[n], i);
}

