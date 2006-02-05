#include "driver.h"
#include "driverlib.h"

/* Returns 0 (no) or 1 (yes) to indicate whether or not a floating
 * color table is available. */

int COM_Can_do_float(void)
{
	if (driver->Can_do_float)
		return (*driver->Can_do_float)();
	return 0;
}

