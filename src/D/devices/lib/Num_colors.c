#include "colors.h"
Number_of_colors(n)
    int *n;
{
    Get_num_colors(n);

/* reduce the number of colors by the number of
 * vector colors, if current table type is float
 */
    if (get_table_type() == FLOAT)
	 *n -= get_max_std_colors();
}
