#include "driver.h"
#include "driverlib.h"

static int linewidth = 0;

int COM_Line_width(int width)
{
	int ret = linewidth;

	if (driver->Line_width)
		return (*driver->Line_width)(width);

	if (width >= 0)
		linewidth = width;

	return ret;
}

