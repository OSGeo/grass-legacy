#include "colors.h"
#include "driver.h"
#include "driverlib.h"

void COM_Number_of_colors(int *n)
{
	COM_Get_num_colors(n);

	/* reduce the number of colors by the number of
	 * vector colors, if current table type is float
	 */

	*n -= get_color_offset();
	if (DRV_get_table_type() == FLOAT)
		*n -= get_max_std_colors();
}

