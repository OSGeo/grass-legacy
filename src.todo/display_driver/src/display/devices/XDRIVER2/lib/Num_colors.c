#include "../XDRIVER.h"
#include "colors.h"

int Number_of_colors (int *n)
{
    Get_num_colors(n);

/* reduce the number of colors by the number of
 * vector colors, if current table type is float
 */
    *n -= get_color_offset();
    if (get_table_type() == FLOAT)
	 *n -= get_max_std_colors();

    return 0;
}
