#include <grass/colors.h>
#include "driver.h"
#include "driverlib.h"

int DRV_get_table_type(void)
{
	if (driver->get_table_type)
		return (*driver->get_table_type)();
	return FIXED;
}

int COM_Color_table_float(void)
{
	if (*driver->Color_table_float)
		return (*driver->Color_table_float)();
	return 0;
}

int COM_Color_table_fixed(void)
{
	if (*driver->Color_table_fixed)
		return (*driver->Color_table_fixed)();
	return 0;
}

