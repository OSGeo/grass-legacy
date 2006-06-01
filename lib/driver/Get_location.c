#include "driver.h"
#include "driverlib.h"

int COM_Get_location_with_pointer(int *wx, int *wy, int *button, int cmd)
{
	return (driver->Get_with_pointer)
		? (*driver->Get_with_pointer)(wx, wy, button, cmd)
		: 1;
}

int COM_Get_location_with_line(int cx, int cy, int *wx, int *wy, int *button, int cmd)
{
	return (driver->Get_with_line)
		? (*driver->Get_with_line)(cx, cy, wx, wy, button, cmd)
		: 1;
}

int COM_Get_location_with_box(int cx, int cy, int *wx, int *wy, int *button, int cmd)
{
	return (driver->Get_with_box)
		? (*driver->Get_with_box)(cx, cy, wx, wy, button, cmd)
		: 1;
}

