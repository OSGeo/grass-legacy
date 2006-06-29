#include "driver.h"
#include "driverlib.h"

int COM_Get_location_with_pointer(int *wx, int *wy, int *button, int cmd)
{
	int ret = 1;

	if (driver->Get_with_pointer)
	{
		ret = (*driver->Get_with_pointer)(wx, wy, button, cmd);
		if (*button >= 0 && *button <= 3)
			*button = mouse_button[*button-1];
	}

	return ret;
}

int COM_Get_location_with_line(int cx, int cy, int *wx, int *wy, int *button, int cmd)
{
	int ret = 1;

	if (driver->Get_with_line)
	{
		ret = (*driver->Get_with_line)(cx, cy, wx, wy, button, cmd);
		if (*button >= 0 && *button <= 3)
			*button = mouse_button[*button-1];
	}

	return ret;
}

int COM_Get_location_with_box(int cx, int cy, int *wx, int *wy, int *button, int cmd)
{
	int ret = 1;

	if (driver->Get_with_box)
	{
		ret = (*driver->Get_with_box)(cx, cy, wx, wy, button, cmd);
		if (*button >= 0 && *button <= 3)
			*button = mouse_button[*button-1];
	}

	return ret;
}

